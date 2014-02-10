#include "BetterYao4.h"
#include "garbled_circuit.h"
#include <algorithm>

#include <log4cxx/logger.h>
static log4cxx::LoggerPtr logger(log4cxx::Logger::getLogger("BetterYao4.cpp"));


BetterYao4::BetterYao4(EnvParams &params) : YaoBase(params), m_ot_bit_cnt(0)
{
	// Init variables
	m_rnds.resize(Env::node_load());
	m_ccts.resize(Env::node_load());
	m_gcs.resize(Env::node_load());

	for (size_t ix = 0; ix < m_gcs.size(); ix++) { init(m_gcs[ix]); }

	m_gen_inp_hash.resize(Env::node_load());
	m_gen_inp_masks.resize(Env::node_load());
	m_gen_inp_decom.resize(Env::node_load());

	m_gen_inp_cnt = read_alice_length(Env::private_file());
	m_evl_inp_cnt = read_bob_length(Env::private_file());

	static byte MASK[8] = { 0xFF, 0x01, 0x03, 0x07, 0x0F, 0x1F, 0x3F, 0x7F};

	m_evl_inp.resize((m_evl_inp_cnt+7)/8);
	m_evl_inp.back() &= MASK[m_evl_inp_cnt%8];

	m_gen_inp.resize((m_gen_inp_cnt+7)/8);
	m_gen_inp.back() &= MASK[m_gen_inp_cnt%8];
}


void BetterYao4::start()
{
	oblivious_transfer();
	cut_and_choose();
	cut_and_choose2();
	consistency_check();
	circuit_evaluate();
	final_report();
}

void BetterYao4::oblivious_transfer()
{
	step_init();

	double start; // time marker

	Bytes send, recv, bufr(Env::elm_size_in_bytes()*4);
	std::vector<Bytes> bufr_chunks, recv_chunks;

	G X[2], Y[2], gr, hr;
	Z s[2], t[2],  y,  a,  r;

	// step 1: generating the CRS: g[0], h[0], g[1], h[1]
	if (Env::is_root())
	{
		EVL_BEGIN
			start = MPI_Wtime();
				y.random();
				a.random();

				m_ot_g[0].random();
				m_ot_g[1] = m_ot_g[0]^y;          // g[1] = g[0]^y

				m_ot_h[0] = m_ot_g[0]^a;          // h[0] = g[0]^a
				m_ot_h[1] = m_ot_g[1]^(a + Z(1)); // h[1] = g[1]^(a+1)

				bufr.clear();
				bufr += m_ot_g[0].to_bytes();
				bufr += m_ot_g[1].to_bytes();
				bufr += m_ot_h[0].to_bytes();
				bufr += m_ot_h[1].to_bytes();
			m_timer_evl += MPI_Wtime() - start;

			start = MPI_Wtime(); // send to Gen's root process
				EVL_SEND(bufr);
			m_timer_com += MPI_Wtime() - start;
		EVL_END

		GEN_BEGIN
			start = MPI_Wtime();
				bufr = GEN_RECV();
			m_timer_com += MPI_Wtime() - start;
		GEN_END

	    m_comm_sz += bufr.size();
	}

	// send g[0], g[1], h[0], h[1] to slave processes
	start = MPI_Wtime();
		MPI_Bcast(&bufr[0], bufr.size(), MPI_BYTE, 0, m_mpi_comm);
	m_timer_mpi += MPI_Wtime() - start;

	start = MPI_Wtime();
		bufr_chunks = bufr.split(Env::elm_size_in_bytes());

		m_ot_g[0].from_bytes(bufr_chunks[0]);
		m_ot_g[1].from_bytes(bufr_chunks[1]);
		m_ot_h[0].from_bytes(bufr_chunks[2]);
		m_ot_h[1].from_bytes(bufr_chunks[3]);

		// pre-processing
		m_ot_g[0].fast_exp();
		m_ot_g[1].fast_exp();
		m_ot_h[0].fast_exp();
		m_ot_h[1].fast_exp();

		// allocate memory for m_keys
		m_ot_keys.resize(Env::node_load());
		for (size_t ix = 0; ix < m_ot_keys.size(); ix++)
		{
			m_ot_keys[ix].reserve(m_evl_inp_cnt*2);
		}
	m_timer_evl += MPI_Wtime() - start;
	m_timer_gen += MPI_Wtime() - start;

	// Step 2: ZKPoK of (g[0], g[1], h[0], h[1])
	// TODO

	// Step 3: gr=g[b]^r, hr=h[b]^r, where b is the evaluator's bit
	if (Env::is_root())
	{
		EVL_BEGIN
			start = MPI_Wtime();
				bufr.clear(); bufr.reserve(Env::exp_size_in_bytes()*m_evl_inp_cnt);
				send.clear(); send.reserve(Env::elm_size_in_bytes()*m_evl_inp_cnt*2);
				for (size_t bix = 0; bix < m_evl_inp_cnt; bix++)
				{
					r.random();
					bufr += r.to_bytes();  // to be shared with slave evaluators

					byte bit_value = m_evl_inp.get_ith_bit(bix);
					send += (m_ot_g[bit_value]^r).to_bytes(); // gr
					send += (m_ot_h[bit_value]^r).to_bytes(); // hr
				}
			m_timer_evl += MPI_Wtime() - start;

			start = MPI_Wtime();
				EVL_SEND(send); // send (gr, hr)'s
			m_timer_com += MPI_Wtime() - start;

			m_comm_sz += send.size();
		EVL_END

		GEN_BEGIN
			start = MPI_Wtime();
				bufr = GEN_RECV(); // receive (gr, hr)'s
			m_timer_com += MPI_Wtime() - start;

			m_comm_sz += bufr.size();
		GEN_END
	}

	EVL_BEGIN // forward rs to slave evaluators
		start = MPI_Wtime();
			bufr.resize(Env::exp_size_in_bytes()*m_evl_inp_cnt);
		m_timer_evl += MPI_Wtime() - start;

		start = MPI_Wtime();
			MPI_Bcast(&bufr[0], bufr.size(), MPI_BYTE, 0, m_mpi_comm); // now every evaluator has r's
		m_timer_mpi += MPI_Wtime() - start;

		start = MPI_Wtime();
			bufr_chunks = bufr.split(Env::exp_size_in_bytes());
		m_timer_evl += MPI_Wtime() - start;
	EVL_END

	GEN_BEGIN // forward (gr, hr)s to slave generators
		start = MPI_Wtime();
			bufr.resize(Env::elm_size_in_bytes()*m_evl_inp_cnt*2);
		m_timer_gen += MPI_Wtime() - start;

		start = MPI_Wtime();
			MPI_Bcast(&bufr[0], bufr.size(), MPI_BYTE, 0, m_mpi_comm); // now every generator has (gr, hr)s
		m_timer_mpi += MPI_Wtime() - start;

		start = MPI_Wtime();
			bufr_chunks = bufr.split(Env::elm_size_in_bytes());
		m_timer_gen += MPI_Wtime() - start;
	GEN_END

	// Step 4: the generator computes X[0], Y[0], X[1], Y[1]
	GEN_BEGIN
		for (size_t bix = 0; bix < m_evl_inp_cnt; bix++)
		{
			start = MPI_Wtime();
				gr.from_bytes(bufr_chunks[2*bix+0]);
				hr.from_bytes(bufr_chunks[2*bix+1]);

				if (m_ot_keys.size() > 2)
				{
					gr.fast_exp();
					hr.fast_exp();
				}
			m_timer_gen += MPI_Wtime() - start;

			for (size_t cix = 0; cix < m_ot_keys.size(); cix++)
			{
				start = MPI_Wtime();
					Y[0].random(); // K[0]
					Y[1].random(); // K[1]

					m_ot_keys[cix].push_back(Y[0].to_bytes().hash(Env::k()));
					m_ot_keys[cix].push_back(Y[1].to_bytes().hash(Env::k()));

					s[0].random(); s[1].random();
					t[0].random(); t[1].random();

					// X[b] = ( g[b]^s[b] ) * ( h[b]^t[b] ), where b = 0, 1
					X[0] = m_ot_g[0]^s[0]; X[0] *= m_ot_h[0]^t[0];
					X[1] = m_ot_g[1]^s[1]; X[1] *= m_ot_h[1]^t[1];

					// Y[b] = ( gr^s[b] ) * ( hr^t[b] ) * K[b], where b = 0, 1
					Y[0] *= gr^s[0]; Y[0] *= hr^t[0];
					Y[1] *= gr^s[1]; Y[1] *= hr^t[1];

					send.clear();
					send += X[0].to_bytes(); send += X[1].to_bytes();
					send += Y[0].to_bytes(); send += Y[1].to_bytes();
				m_timer_gen += MPI_Wtime() - start;

				start = MPI_Wtime();
					GEN_SEND(send);
				m_timer_com += MPI_Wtime() - start;

				m_comm_sz += send.size();
			}
		}

		for (size_t ix = 0; ix < m_ot_keys.size(); ix++)
		{
			assert(m_ot_keys[ix].size() == m_evl_inp_cnt*2);
		}
	GEN_END

	// Step 5: the evaluator computes K = Y[b]/X[b]^r
	EVL_BEGIN
		for (size_t bix = 0; bix < m_evl_inp_cnt; bix++)
		{
			start = MPI_Wtime();
				int bit_value = m_evl_inp.get_ith_bit(bix);
				r.from_bytes(bufr_chunks[bix]);
			m_timer_evl += MPI_Wtime() - start;

			for (size_t cix = 0; cix < m_ot_keys.size(); cix++)
			{
				start = MPI_Wtime();
					recv = EVL_RECV(); // receive X[0], X[1], Y[0], Y[1]
				m_timer_com += MPI_Wtime() - start;

				m_comm_sz += recv.size();

				start = MPI_Wtime();
					recv_chunks = recv.split(Env::elm_size_in_bytes());

					X[bit_value].from_bytes(recv_chunks[    bit_value]); // X[b]
					Y[bit_value].from_bytes(recv_chunks[2 + bit_value]); // Y[b]

					// K = Y[b]/(X[b]^r)
					Y[bit_value] /= X[bit_value]^r;
					m_ot_keys[cix].push_back(Y[bit_value].to_bytes().hash(Env::k()));
				m_timer_evl += MPI_Wtime() - start;
			}
		}

		for (size_t ix = 0; ix < m_ot_keys.size(); ix++)
		{
			assert(m_ot_keys[ix].size() == m_evl_inp_cnt);
		}
	EVL_END

	step_report("ob-transfer");
}

Bytes BetterYao4::flip_coins(size_t len_in_bytes)
{
	double start;

	Bytes bufr;

	if (Env::is_root())
	{
		start = MPI_Wtime();
			Bytes coins = m_prng.rand(len_in_bytes*8);	// Step 0: flip coins
			Bytes remote_coins, comm, open;
		m_timer_gen += MPI_Wtime() - start;
		m_timer_evl += MPI_Wtime() - start;

		GEN_BEGIN
			start = MPI_Wtime();
				open = m_prng.rand(Env::k()) + coins;	// Step 1: commit to coins
				comm = open.hash(Env::k());
			m_timer_gen += MPI_Wtime() - start;

			start = MPI_Wtime();
				GEN_SEND(comm);
				remote_coins = GEN_RECV();				// Step 2: receive alice's coins
				GEN_SEND(open);							// Step 3: decommit to the coins
			m_timer_com += MPI_Wtime() - start;
		GEN_END

		EVL_BEGIN
			start = MPI_Wtime();
				comm = EVL_RECV();						// Step 1: receive bob's commitment
				EVL_SEND(coins);						// Step 2: send coins to bob
				open = EVL_RECV();
			m_timer_com += MPI_Wtime() - start;

			start = MPI_Wtime();
				if (!(open.hash(Env::k()) == comm))		// Step 3: check bob's decommitment
				{
					LOG4CXX_FATAL(logger, "commitment to coins can't be properly opened");
					MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
				}
				remote_coins = Bytes(open.begin()+Env::k()/8, open.end());
			m_timer_evl += MPI_Wtime() - start;
		EVL_END

		m_comm_sz = comm.size() + remote_coins.size() + open.size();

		start = MPI_Wtime();
			coins ^= remote_coins;
			bufr.swap(coins);
		m_timer_evl += MPI_Wtime() - start;
		m_timer_gen += MPI_Wtime() - start;
	}

	return bufr;
}

void BetterYao4::cut_and_choose()
{
	step_init();

	double start;

	Bytes coins = flip_coins(Env::key_size_in_bytes()); // only roots get the result

	if (Env::is_root())
	{
		start = MPI_Wtime();
			Prng prng;
			prng.srand(coins); // use the coins to generate more random bits

			// make 60-40 check-vs-evaluateion circuit ratio
			m_all_chks.assign(Env::s(), 1);

			// FisherÐYates shuffle
			std::vector<uint16_t> indices(m_all_chks.size());
			for (size_t ix = 0; ix < indices.size(); ix++) { indices[ix] = ix; }

			// starting from 1 since the 0-th circuit is always evaluation-circuit
			for (size_t ix = 1; ix < indices.size(); ix++)
			{
				int rand_ix = prng.rand_range(indices.size()-ix);
				std::swap(indices[ix], indices[ix+rand_ix]);
			}

			int num_of_evls;
			switch(m_all_chks.size())
			{
			case 0: case 1:
				LOG4CXX_FATAL(logger, "there isn't enough circuits for cut-and-choose");
				MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
				break;

			case 2: case 3:
				num_of_evls = 1;
				break;

			case 4:
				num_of_evls = 2;
				break;

			default:
				num_of_evls = m_all_chks.size()*2/5;
				break;
			}

			for (size_t ix = 0; ix < num_of_evls; ix++) { m_all_chks[indices[ix]] = 0; }
		m_timer_evl += MPI_Wtime() - start;
		m_timer_gen += MPI_Wtime() - start;
	}

	start = MPI_Wtime();
		m_chks.resize(Env::node_load());
	m_timer_evl += MPI_Wtime() - start;
	m_timer_gen += MPI_Wtime() - start;

	start = MPI_Wtime();
		MPI_Scatter(&m_all_chks[0], m_chks.size(), MPI_BYTE, &m_chks[0], m_chks.size(), MPI_BYTE, 0, m_mpi_comm);
	m_timer_mpi += MPI_Wtime() - start;

	step_report("cut-&-check");
}

void BetterYao4::cut_and_choose2()
{
	step_init();

	cut_and_choose2_ot();
	cut_and_choose2_precomputation();

	for (size_t ix = 0; ix < m_gcs.size(); ix++)
	{
		cut_and_choose2_evl_circuit(ix);
		cut_and_choose2_chk_circuit(ix);
	}

	step_report("cut-'n-chk2");
}

//
// Outputs: m_prngs[]
//
void BetterYao4::cut_and_choose2_ot()
{
	double start;
	m_ot_bit_cnt = Env::node_load();

	EVL_BEGIN
		start = MPI_Wtime();
			m_ot_recv_bits.resize((m_ot_bit_cnt+7)/8);
			for (size_t ix = 0; ix < m_chks.size(); ix++)
			{
				m_ot_recv_bits.set_ith_bit(ix, m_chks[ix]);
			}
		m_timer_evl += MPI_Wtime() - start;
	EVL_END

	ot_init();
	ot_random();

	// Gen's m_ot_out has 2*Env::node_load() seeds and
	// Evl's m_ot_out has   Env::node_load() seeds according to m_chks.

	GEN_BEGIN
		start = MPI_Wtime();
			m_prngs.resize(2*Env::node_load());
			for (size_t ix = 0; ix < m_prngs.size(); ix++)
			{
				m_prngs[ix].srand(m_ot_out[ix]);
			}
		m_timer_gen += MPI_Wtime() - start;
	GEN_END

	EVL_BEGIN
		start = MPI_Wtime();
			m_prngs.resize(Env::node_load());
			for (size_t ix = 0; ix < m_prngs.size(); ix++)
			{
				m_prngs[ix].srand(m_ot_out[ix]);
			}
		m_timer_evl += MPI_Wtime() - start;
	EVL_END
}

//
// Outputs: m_rnds[], m_gen_inp_masks[], m_gcs[].m_gen_inp_decom
//

extern "C" {
void finalize(PCFState *st);
}

void BetterYao4::cut_and_choose2_precomputation()
{
	double start;

	GEN_BEGIN
		start = MPI_Wtime();
			for (size_t ix = 0; ix < m_gcs.size(); ix++)
			{
				m_rnds[ix] = m_prng.rand(Env::k());
				m_gen_inp_masks[ix] = m_prng.rand(m_gen_inp_cnt);

				gen_init(m_gcs[ix], m_ot_keys[ix], m_gen_inp_masks[ix], m_rnds[ix]);

				m_gcs[ix].m_st = 
					load_pcf_file(Env::pcf_file(), m_gcs[ix].m_const_wire, m_gcs[ix].m_const_wire+1, copy_key);
                                m_gcs[ix].m_st->alice_in_size = m_gen_inp_cnt;
                                m_gcs[ix].m_st->bob_in_size = m_evl_inp_cnt;

				set_external_state(m_gcs[ix].m_st, &m_gcs[ix]);
				set_key_copy_function(m_gcs[ix].m_st, copy_key);
				set_key_delete_function(m_gcs[ix].m_st, delete_key);
				set_callback(m_gcs[ix].m_st, gen_next_gate_m);

				while ((m_gcs[ix].m_gen_inp_decom.size()/2 < m_gen_inp_cnt) && get_next_gate(m_gcs[ix].m_st))
				{
					send(m_gcs[ix]); // discard the garbled gates for now
				}

finalize(m_gcs[ix].m_st);
			}
		m_timer_gen += MPI_Wtime() - start;
	GEN_END
}

void BetterYao4::cut_and_choose2_evl_circuit(size_t ix)
{
	double start;

	Bytes bufr;

	// send masked gen inp
	GEN_BEGIN
		start = MPI_Wtime();
			bufr = m_gen_inp_masks[ix] ^ m_gen_inp;
			bufr ^= m_prngs[2*ix+0].rand(bufr.size()*8); // encrypt message
		m_timer_gen += MPI_Wtime() - start;

		start = MPI_Wtime();
			GEN_SEND(bufr);
		m_timer_com += MPI_Wtime() - start;
	GEN_END

	EVL_BEGIN
		start = MPI_Wtime();
			bufr = EVL_RECV();
		m_timer_com += MPI_Wtime() - start;

		start = MPI_Wtime();
			if (!m_chks[ix]) // evaluation circuit
			{
				bufr ^= m_prngs[ix].rand(bufr.size()*8); // decrypt message
				m_gen_inp_masks[ix] = bufr;
			}
		m_timer_evl += MPI_Wtime() - start;
	EVL_END

	m_comm_sz += bufr.size();

	// send constant keys m_gcs[ix].m_const_wire
	GEN_BEGIN
		start = MPI_Wtime();
			bufr = get_const_key(m_gcs[ix], 0, 0) + get_const_key(m_gcs[ix], 1, 1);
			bufr ^= m_prngs[2*ix+0].rand(bufr.size()*8); // encrypt message
		m_timer_gen += MPI_Wtime() - start;

		start = MPI_Wtime();
			GEN_SEND(bufr);
		m_timer_com += MPI_Wtime() - start;
	GEN_END

	EVL_BEGIN
		start = MPI_Wtime();
			bufr = EVL_RECV();
		m_timer_com += MPI_Wtime() - start;

		start = MPI_Wtime();
			if (!m_chks[ix]) // evaluation circuit
			{
				bufr ^= m_prngs[ix].rand(bufr.size()*8); // decrypt message
				vector<Bytes> bufr_chunks = bufr.split(Env::key_size_in_bytes());
				set_const_key(m_gcs[ix], 0, bufr_chunks[0]);
				set_const_key(m_gcs[ix], 1, bufr_chunks[1]);
			}
		m_timer_evl += MPI_Wtime() - start;
	EVL_END

	m_comm_sz += bufr.size();

	// send m_gcs[ix].m_gen_inp_decom
	GEN_BEGIN
		assert(m_gcs[ix].m_gen_inp_decom.size() == 2*m_gen_inp_cnt);
	GEN_END

	EVL_BEGIN
		if (!m_chks[ix]) { m_gcs[ix].m_gen_inp_decom.resize(m_gen_inp_cnt); }
	EVL_END

	for (size_t jx = 0; jx < m_gen_inp_cnt; jx++)
	{
		GEN_BEGIN
			start = MPI_Wtime();
				byte bit = m_gen_inp.get_ith_bit(jx) ^ m_gen_inp_masks[ix].get_ith_bit(jx);
				bufr = m_gcs[ix].m_gen_inp_decom[2*jx+bit];
				bufr ^= m_prngs[2*ix+0].rand(bufr.size()*8); // encrypt message
			m_timer_gen += MPI_Wtime() - start;

			start = MPI_Wtime();
				GEN_SEND(bufr);
			m_timer_com += MPI_Wtime() - start;
		GEN_END

		EVL_BEGIN
			start = MPI_Wtime();
				bufr = EVL_RECV();
			m_timer_com += MPI_Wtime() - start;

			start = MPI_Wtime();
				if (!m_chks[ix]) // evaluation circuit
				{
					bufr ^= m_prngs[ix].rand(bufr.size()*8); // decrypt message
					m_gcs[ix].m_gen_inp_decom[jx] = bufr;
				}
			m_timer_evl += MPI_Wtime() - start;
		EVL_END

		m_comm_sz += bufr.size();
	}
}


void BetterYao4::cut_and_choose2_chk_circuit(size_t ix)
{
	double start;

	Bytes bufr;
	vector<Bytes> bufr_chunks;

	// send m_gen_inp_masks[ix]
	GEN_BEGIN
		start = MPI_Wtime();
			bufr = m_gen_inp_masks[ix];
			bufr ^= m_prngs[2*ix+1].rand(bufr.size()*8); // encrypt message
		m_timer_gen += MPI_Wtime() - start;

		start = MPI_Wtime();
			GEN_SEND(bufr);
		m_timer_com += MPI_Wtime() - start;
	GEN_END

	EVL_BEGIN
		start = MPI_Wtime();
			bufr = EVL_RECV();
		m_timer_com += MPI_Wtime() - start;

		start = MPI_Wtime();
			if (m_chks[ix]) // check circuit
			{
				bufr ^= m_prngs[ix].rand(bufr.size()*8); // decrypt message
				m_gen_inp_masks[ix] = bufr;
			}
		m_timer_evl += MPI_Wtime() - start;
	EVL_END

	m_comm_sz += bufr.size();

	// send m_rnds[ix]
	GEN_BEGIN
		start = MPI_Wtime();
			bufr = m_rnds[ix];
			bufr ^= m_prngs[2*ix+1].rand(bufr.size()*8); // encrypt message
		m_timer_gen += MPI_Wtime() - start;

		start = MPI_Wtime();
			GEN_SEND(bufr);
		m_timer_com += MPI_Wtime() - start;
	GEN_END

	EVL_BEGIN
		start = MPI_Wtime();
			bufr = EVL_RECV();
		m_timer_com += MPI_Wtime() - start;

		start = MPI_Wtime();
			if (m_chks[ix]) // check circuit
			{
				bufr ^= m_prngs[ix].rand(bufr.size()*8); // decrypt message
				m_rnds[ix] = bufr;
			}
		m_timer_evl += MPI_Wtime() - start;
	EVL_END

	m_comm_sz += bufr.size();


	// send m_ot_kesy[ix]
	GEN_BEGIN
		assert(m_ot_keys[ix].size() == 2*m_evl_inp_cnt);
	GEN_END

	EVL_BEGIN
		if (m_chks[ix]) { m_ot_keys[ix].resize(2*m_evl_inp_cnt); }
	EVL_END

	for (size_t jx = 0; jx < 2*m_evl_inp_cnt; jx++)
	{
		GEN_BEGIN
			start = MPI_Wtime();
				bufr = m_ot_keys[ix][jx];
				bufr ^= m_prngs[2*ix+1].rand(bufr.size()*8); // encrypt message
			m_timer_gen += MPI_Wtime() - start;

			start = MPI_Wtime();
				GEN_SEND(bufr);
			m_timer_com += MPI_Wtime() - start;
		GEN_END

		EVL_BEGIN
			start = MPI_Wtime();
				bufr = EVL_RECV();
			m_timer_com += MPI_Wtime() - start;

			start = MPI_Wtime();
				if (m_chks[ix]) // check circuit
				{
					bufr ^= m_prngs[ix].rand(bufr.size()*8); // decrypt message
					m_ot_keys[ix][jx] = bufr;
				}
			m_timer_evl += MPI_Wtime() - start;
		EVL_END

		m_comm_sz += bufr.size();
	}

	// send m_gcs[ix].m_gen_inp_decom
	GEN_BEGIN
		assert(m_gcs[ix].m_gen_inp_decom.size() == 2*m_gen_inp_cnt);
	GEN_END

	EVL_BEGIN
		if (m_chks[ix]) { m_gen_inp_decom[ix].resize(2*m_gen_inp_cnt); }
	EVL_END

	for (size_t jx = 0; jx < 2*m_gen_inp_cnt; jx++)
	{
		GEN_BEGIN
			start = MPI_Wtime();
				bufr = m_gcs[ix].m_gen_inp_decom[jx];
				bufr ^= m_prngs[2*ix+1].rand(bufr.size()*8); // encrypt message
			m_timer_gen += MPI_Wtime() - start;

			start = MPI_Wtime();
				GEN_SEND(bufr);
			m_timer_com += MPI_Wtime() - start;
		GEN_END

		EVL_BEGIN
			start = MPI_Wtime();
				bufr = EVL_RECV();
			m_timer_com += MPI_Wtime() - start;

			start = MPI_Wtime();
				if (m_chks[ix]) // check circuit
				{
					bufr ^= m_prngs[ix].rand(bufr.size()*8); // decrypt message
					m_gen_inp_decom[ix][jx] = bufr;
				}
			m_timer_evl += MPI_Wtime() - start;
		EVL_END

		m_comm_sz += bufr.size();
	}
}

#include <unistd.h>

void BetterYao4::consistency_check()
{
	step_init();

	Bytes bufr;
	std::vector<Bytes> bufr_chunks;

	double start;

	// jointly pick a 2-UHF matrix
	bufr = flip_coins(Env::k()*((m_gen_inp_cnt+7)/8)); // only roots get the result

	start = MPI_Wtime();
		bufr.resize(Env::k()*((m_gen_inp_cnt+7)/8));
	m_timer_evl += MPI_Wtime() - start;
	m_timer_gen += MPI_Wtime() - start;

	start = MPI_Wtime();
		MPI_Bcast(&bufr[0], bufr.size(), MPI_BYTE, 0, m_mpi_comm);
	m_timer_mpi += MPI_Wtime() - start;

	start = MPI_Wtime();
		m_matrix = bufr.split(bufr.size()/Env::k());
	m_timer_evl += MPI_Wtime() - start;
	m_timer_gen += MPI_Wtime() - start;

	// now everyone agrees on the UHF given by m_matrix
	for (size_t ix = 0; ix < m_gcs.size(); ix++)
		for (size_t kx = 0; kx < m_matrix.size(); kx++)
	{
		GEN_BEGIN
			start = MPI_Wtime();
				gen_next_gen_inp_com(m_gcs[ix], m_matrix[kx], kx);
				bufr = send(m_gcs[ix]);
			m_timer_gen += MPI_Wtime() - start;

			start = MPI_Wtime();
				GEN_SEND(bufr);
			m_timer_com += MPI_Wtime() - start;

		GEN_END

		EVL_BEGIN
			start = MPI_Wtime();
				bufr = EVL_RECV();
			m_timer_com += MPI_Wtime() - start;

			if (!m_chks[ix]) // evaluation circuit
			{
				start = MPI_Wtime();
					recv(m_gcs[ix], bufr);
					evl_next_gen_inp_com(m_gcs[ix], m_matrix[kx], kx);
				m_timer_evl += MPI_Wtime() - start;
			}
		EVL_END

		m_comm_sz += bufr.size();
	}

	EVL_BEGIN
		for (size_t ix = 0; ix < m_gcs.size(); ix++)
			if (!m_chks[ix])
		{
			m_gen_inp_hash[ix] = m_gcs[ix].m_gen_inp_hash;
		}
	EVL_END

	step_report("const-check");
}

void BetterYao4::circuit_evaluate()
{
	step_init();

	double start;

	int verify = 1;
	Bytes bufr;

	for (size_t ix = 0; ix < m_gcs.size(); ix++)
	{
		GEN_BEGIN
			start = MPI_Wtime();
				gen_init(m_gcs[ix], m_ot_keys[ix], m_gen_inp_masks[ix], m_rnds[ix]);
//std::cout << ix << " gen const 0: " << get_const_key(m_gcs[ix], 0, 0).to_hex() << std::endl;
//std::cout << ix << " gen const 1: " << get_const_key(m_gcs[ix], 1, 1).to_hex() << std::endl;
			m_timer_gen += MPI_Wtime() - start;
		GEN_END

		EVL_BEGIN
			start = MPI_Wtime();
				if (m_chks[ix]) // check-circuits
				{
					gen_init(m_gcs[ix], m_ot_keys[ix], m_gen_inp_masks[ix], m_rnds[ix]);
//std::cout << "check ";
				}
				else // evaluation-circuits
				{
					evl_init(m_gcs[ix], m_ot_keys[ix], m_gen_inp_masks[ix], m_evl_inp);
//std::cout << "evl ";
				}
			m_timer_evl += MPI_Wtime() - start;
//std::cout << ix << " evl const 0: " << get_const_key(m_gcs[ix], 0, 0).to_hex() << std::endl;
//std::cout << ix << " evl const 1: " << get_const_key(m_gcs[ix], 1, 1).to_hex() << std::endl;
		EVL_END

		start = MPI_Wtime();
			m_gcs[ix].m_st = 
				load_pcf_file(Env::pcf_file(), m_gcs[ix].m_const_wire, m_gcs[ix].m_const_wire+1, copy_key);
                        m_gcs[ix].m_st->alice_in_size = m_gen_inp_cnt;
                        m_gcs[ix].m_st->bob_in_size = m_evl_inp_cnt;
	
			set_external_state(m_gcs[ix].m_st, &m_gcs[ix]);
			set_key_copy_function(m_gcs[ix].m_st, copy_key);
			set_key_delete_function(m_gcs[ix].m_st, delete_key);
		m_timer_gen += MPI_Wtime() - start;
		m_timer_evl += MPI_Wtime() - start;

		GEN_BEGIN // generate and send the circuit gate-by-gate
			start = MPI_Wtime();
				set_callback(m_gcs[ix].m_st, gen_next_gate_m);
				while (get_next_gate(m_gcs[ix].m_st))
				{
						bufr = send(m_gcs[ix]);
					m_timer_gen += MPI_Wtime() - start;
	
					start = MPI_Wtime();
						GEN_SEND(bufr);
					m_timer_com += MPI_Wtime() - start;
	
					m_comm_sz += bufr.size();

					start = MPI_Wtime(); // start m_timer_gen
				}
			m_timer_gen += MPI_Wtime() - start;

			GEN_SEND(Bytes(0)); // a redundant value to prevent the evlauator from hanging
		GEN_END

		EVL_BEGIN // receive and evaluate the circuit gate-by-gate
			if (m_chks[ix]) // check circuit
			{
				start = MPI_Wtime();
					set_callback(m_gcs[ix].m_st, gen_next_gate_m);
					while (get_next_gate(m_gcs[ix].m_st))
					{
							bufr = send(m_gcs[ix]);
						m_timer_evl += MPI_Wtime() - start;
	
						start = MPI_Wtime();
							Bytes recv = EVL_RECV();
						m_timer_com += MPI_Wtime() - start;
	
						m_comm_sz += bufr.size();

						start = MPI_Wtime(); // start m_timer_evl
							verify &= (bufr == recv);
					}
				m_timer_gen += MPI_Wtime() - start;

				EVL_RECV(); // a redundant value to prevent the evlauator from hanging
			}
			else // evaluation circuit
			{
				start = MPI_Wtime();
					set_callback(m_gcs[ix].m_st, evl_next_gate_m);
					do {
						m_timer_evl += MPI_Wtime() - start;
	
						start = MPI_Wtime();
							bufr = EVL_RECV();
						m_timer_com += MPI_Wtime() - start;
	
						m_comm_sz += bufr.size();

						start = MPI_Wtime();
							recv(m_gcs[ix], bufr);
					} while (get_next_gate(m_gcs[ix].m_st));
				m_timer_evl += MPI_Wtime() - start;
			}
		EVL_END
	}

	EVL_BEGIN // check the hash of all the garbled circuits
		int all_verify = 0;

		start = MPI_Wtime();
			MPI_Reduce(&verify, &all_verify, 1, MPI_INT, MPI_LAND, 0, m_mpi_comm);
		m_timer_mpi += MPI_Wtime() - start;

		start = MPI_Wtime();
			if (Env::is_root() && !all_verify)
			{
				LOG4CXX_FATAL(logger, "Verification failed");
				//MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
			}

			Bytes gen_inp_hash;

			for (size_t ix = 0; ix < m_gcs.size(); ix++)
			{
				// check the commitments associated with the generator's input wires
				if (m_chks[ix]) // check circuit
				{
					for (size_t jx = 0; jx < m_gen_inp_cnt*2; jx++)
					{
						if (!(m_gen_inp_decom[ix][jx] == m_gcs[ix].m_gen_inp_decom[jx]))
						{
							LOG4CXX_FATAL(logger, "Commitment Verification Failure (check circuit)");
							MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
						}
					}
				}
				else // evaluation circuit
				{
					if (!pass_check(m_gcs[ix]))
					{
						LOG4CXX_FATAL(logger, "Commitment Verification Failure (evaluation circuit)");
						MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
					}

					if (gen_inp_hash.size() == 0)
					{
						gen_inp_hash = m_gen_inp_hash[ix];
					}
					else if (!(gen_inp_hash == m_gen_inp_hash[ix]))
					{
						LOG4CXX_FATAL(logger, "Generator Input Hash Inconsistent Failure (evaluation circuit)");
						MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
					}
				}

				trim_output(m_gcs[ix]);
			}
		m_timer_evl += MPI_Wtime() - start;
	EVL_END

	step_report("circuit-evl");

	if (m_gcs[0].m_evl_out_ix != 0)
		proc_evl_out();

    if (m_gcs[0].m_gen_out_ix != 0)
        proc_gen_out();
}



void BetterYao4::proc_evl_out()
{
	EVL_BEGIN
		step_init();

		double start;
		Bytes send, recv;

		start = MPI_Wtime();
			for (size_t ix = 0; ix < m_gcs.size(); ix++) // fill zeros for uniformity (convenient for MPIs)
			{
				send += m_gcs[ix].m_evl_out;
			}

			if (Env::is_root() == 0)
			{
				recv.resize(send.size()*Env::node_amnt());
			}
		m_timer_evl += MPI_Wtime() - start;

		start = MPI_Wtime();
			MPI_Gather(&send[0], send.size(), MPI_BYTE, &recv[0], send.size(), MPI_BYTE, 0, m_mpi_comm);
		m_timer_mpi += MPI_Wtime() - start;

		start = MPI_Wtime();
			if (Env::is_root())
			{
				size_t chks_total = 0;
				for (size_t ix = 0; ix < m_all_chks.size(); ix++)
					chks_total += m_all_chks[ix];

				// find majority by locating the median of output from evaluation-circuits
				std::vector<Bytes> vec = recv.split((Env::circuit().evl_out_cnt()+7)/8);
				size_t median_ix = (chks_total+vec.size())/2;
				std::nth_element(vec.begin(), vec.begin()+median_ix, vec.end());

				m_evl_out = *(vec.begin()+median_ix);
			}
		m_timer_evl += MPI_Wtime() - start;

		step_report_no_sync("chk-evl-out");
	EVL_END
}

void BetterYao4::proc_gen_out()
{
	step_init();

	// TODO: implement Ki08
	m_gen_out = m_gcs[0].m_gen_out;

	EVL_BEGIN
        m_gen_out = m_gcs[0].m_gen_out;
        EVL_SEND(m_gen_out);
	EVL_END

	GEN_BEGIN
        m_gen_out = GEN_RECV();
    GEN_END

	step_report("chk-gen-out");
}


//
// Implementation of "Two-Output Secure Computation with Malicious Adversaries"
// by abhi shelat and Chih-hao Shen from EUROCRYPT'11 (Protocol 2)
//
// The evaluator (sender) generates m_ot_bit_cnt pairs of k-bit random strings, and
// the generator (receiver) has input m_ot_bits and will receive output m_ot_out.
//
void BetterYao4::ot_init()
{
	double start;

	start = MPI_Wtime();
		std::vector<Bytes> bufr_chunks;
		Bytes bufr(Env::elm_size_in_bytes()*4);

		Z y, a;
	m_timer_gen += MPI_Wtime() - start;
	m_timer_evl += MPI_Wtime() - start;

	// step 1: ZKPoK of the CRS: g[0], h[0], g[1], h[1]
	if (Env::is_root())
	{
		EVL_BEGIN // evaluator (OT receiver)
			start = MPI_Wtime();
				y.random();

				a.random();

				m_ot_g[0].random();
				m_ot_g[1] = m_ot_g[0]^y;          // g[1] = g[0]^y

				m_ot_h[0] = m_ot_g[0]^a;          // h[0] = g[0]^a
				m_ot_h[1] = m_ot_g[1]^(a + Z(1)); // h[1] = g[1]^(a+1)

				bufr.clear();
				bufr += m_ot_g[0].to_bytes();
				bufr += m_ot_g[1].to_bytes();
				bufr += m_ot_h[0].to_bytes();
				bufr += m_ot_h[1].to_bytes();
			m_timer_evl += MPI_Wtime() - start;

			start = MPI_Wtime();
				EVL_SEND(bufr);
			m_timer_com += MPI_Wtime() - start;
		EVL_END

		GEN_BEGIN // generator (OT sender)
			start = MPI_Wtime();
				bufr = GEN_RECV();
			m_timer_com += MPI_Wtime() - start;
		GEN_END

	    m_comm_sz += bufr.size();
	}

	// send g[0], g[1], h[0], h[1] to slave processes
	start = MPI_Wtime();
		MPI_Bcast(&bufr[0], bufr.size(), MPI_BYTE, 0, m_mpi_comm);
	m_timer_mpi += MPI_Wtime() - start;

	start = MPI_Wtime();
		bufr_chunks = bufr.split(Env::elm_size_in_bytes());

		m_ot_g[0].from_bytes(bufr_chunks[0]);
		m_ot_g[1].from_bytes(bufr_chunks[1]);
		m_ot_h[0].from_bytes(bufr_chunks[2]);
		m_ot_h[1].from_bytes(bufr_chunks[3]);

		// group element pre-processing
		m_ot_g[0].fast_exp();
		m_ot_g[1].fast_exp();
		m_ot_h[0].fast_exp();
		m_ot_h[1].fast_exp();
	m_timer_gen += MPI_Wtime() - start;
	m_timer_evl += MPI_Wtime() - start;
}


void BetterYao4::ot_random()
{
	double start;

	start = MPI_Wtime();
		Bytes send, recv;
		std::vector<Bytes> recv_chunks;

		Z r, s[2], t[2];
		G gr, hr, X[2], Y[2];

		m_ot_out.clear();
		m_ot_out.reserve(2*m_ot_bit_cnt); // the receiver only uses half of it
	m_timer_gen += MPI_Wtime() - start;
	m_timer_evl += MPI_Wtime() - start;

	EVL_BEGIN // evaluator (OT receiver)
		assert(m_ot_recv_bits.size() >= ((m_ot_bit_cnt+7)/8));

		for (size_t bix = 0; bix < m_ot_bit_cnt; bix++)
		{
			// Step 1: gr=g[b]^r, hr=h[b]^r, where b is the receiver's bit
			start = MPI_Wtime();
				int bit_value = m_ot_recv_bits.get_ith_bit(bix);

				r.random();

				gr = m_ot_g[bit_value]^r;
				hr = m_ot_h[bit_value]^r;

				send.clear();
				send += gr.to_bytes();
				send += hr.to_bytes();
			m_timer_evl += MPI_Wtime() - start;

			start = MPI_Wtime();
				EVL_SEND(send);

				// Step 2: the evaluator computes X[0], Y[0], X[1], Y[1]
				recv.clear();
				recv += EVL_RECV(); // receive X[0], Y[0], X[1], Y[1]
			m_timer_com += MPI_Wtime() - start;

			m_comm_sz += send.size() + recv.size();

			// Step 3: the evaluator computes K = Y[b]/X[b]^r
			start = MPI_Wtime();
				recv_chunks = recv.split(Env::elm_size_in_bytes());

				X[bit_value].from_bytes(recv_chunks[    bit_value]); // X[b]
				Y[bit_value].from_bytes(recv_chunks[2 + bit_value]); // Y[b]

				// K = Y[b]/(X[b]^r)
				Y[bit_value] /= X[bit_value]^r;
				m_ot_out.push_back(Y[bit_value].to_bytes().hash(Env::k()));
			m_timer_evl += MPI_Wtime() - start;
		}

		assert(m_ot_out.size() == m_ot_bit_cnt);
	EVL_END

	GEN_BEGIN // generator (OT sender)
		for (size_t bix = 0; bix < m_ot_bit_cnt; bix++)
		{
			// Step 1: gr=g[b]^r, hr=h[b]^r, where b is the receiver's bit
			start = MPI_Wtime();
				recv.clear();
				recv += GEN_RECV(); // receive gr, hr
			m_timer_com += MPI_Wtime() - start;

			m_comm_sz += recv.size();

			// Step 2: the evaluator computes X[0], Y[0], X[1], Y[1]
			start = MPI_Wtime();
				recv_chunks = recv.split(Env::elm_size_in_bytes());

				gr.from_bytes(recv_chunks[0]);
				hr.from_bytes(recv_chunks[1]);

				Y[0].random(); Y[1].random(); // K[0], K[1] sampled at random

				m_ot_out.push_back(Y[0].to_bytes().hash(Env::k()));
				m_ot_out.push_back(Y[1].to_bytes().hash(Env::k()));

				s[0].random(); s[1].random();
				t[0].random(); t[1].random();

				// X[b] = ( g[b]^s[b] ) * ( h[b]^t[b] ) for b = 0, 1
				X[0] = m_ot_g[0]^s[0]; X[0] *= m_ot_h[0]^t[0];
				X[1] = m_ot_g[1]^s[1]; X[1] *= m_ot_h[1]^t[1];

				// Y[b] = ( gr^s[b] ) * ( hr^t[b] ) * K[b] for b = 0, 1
				Y[0] *= gr^s[0]; Y[0] *= hr^t[0];
				Y[1] *= gr^s[1]; Y[1] *= hr^t[1];

				send.clear();
				send += X[0].to_bytes();
				send += X[1].to_bytes();
				send += Y[0].to_bytes();
				send += Y[1].to_bytes();
			m_timer_gen += MPI_Wtime() - start;

			start = MPI_Wtime();
				GEN_SEND(send);
			m_timer_com += MPI_Wtime() - start;

			m_comm_sz += send.size();
		}

		assert(m_ot_out.size() == 2*m_ot_bit_cnt);
	GEN_END
}
