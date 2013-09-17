#include "garbled_circuit.h"


void *copy_key(void *old_key)
{
	__m128i *new_key = 0;
	if (old_key != 0)
	{
		new_key = (__m128i*)_mm_malloc(sizeof(__m128i), sizeof(__m128i));
		*new_key = *reinterpret_cast<__m128i*>(old_key);
	}
	return new_key;
}

void delete_key(void *key)
{
	if (key != 0) _mm_free(key);
}

const Bytes &get_const_key(garbled_circuit_t &cct, byte c, byte b)
{
	assert(c == 0 || c == 1); // wire for constant 0 or 1
	assert(b == 0 || b == 1); // with bit value 0 or 1
	static Bytes tmp(16);

	tmp.resize(16);
	if (b)
	{
		_mm_storeu_si128(reinterpret_cast<__m128i*>(&tmp[0]), _mm_xor_si128(cct.m_R, cct.m_const_wire[c]));
	}
	else
	{
		_mm_storeu_si128(reinterpret_cast<__m128i*>(&tmp[0]), cct.m_const_wire[c]);
	}

	tmp.resize(Env::key_size_in_bytes());
	return tmp;
}

void set_const_key(garbled_circuit_t &cct, byte c, const Bytes &key)
{
	assert(c == 0 || c == 1); // wire for constant 0 or 1
	Bytes tmp = key;
	tmp.resize(16);
	cct.m_const_wire[c] = _mm_loadu_si128(reinterpret_cast<__m128i*>(&tmp[0]));
}

namespace
{
const int CIRCUIT_HASH_BUFFER_SIZE = 1024*1024;
const int MAX_OUTPUT_SIZE = 1024;

void update_hash(garbled_circuit_t &cct, const Bytes &data)
{
	cct.m_bufr += data;

#ifdef RAND_SEED
	if (cct.m_bufr.size() > CIRCUIT_HASH_BUFFER_SIZE) // hash the circuit by chunks
	{
		cct.m_hash.update(cct.m_bufr);
		cct.m_bufr.clear();
	}
#endif
}

};

void gen_init(garbled_circuit_t &cct, const vector<Bytes> &ot_keys, const Bytes &gen_inp_mask, const Bytes &seed)
{
	cct.m_ot_keys = &ot_keys;
	cct.m_gen_inp_mask = gen_inp_mask;
	cct.m_prng.srand(seed);

	// R is a random k-bit string whose 0-th bit has to be 1
	Bytes tmp;

	tmp = cct.m_prng.rand(Env::k());
	tmp.set_ith_bit(0, 1);
	tmp.resize(16, 0);
	cct.m_R = _mm_loadu_si128(reinterpret_cast<const __m128i*>(&tmp[0]));

	// pick zero-keys for constant wires
	tmp = cct.m_prng.rand(Env::k());
	tmp.resize(16, 0);
	cct.m_const_wire[0] = _mm_loadu_si128(reinterpret_cast<const __m128i*>(&tmp[0]));

	tmp = cct.m_prng.rand(Env::k());
	tmp.resize(16, 0);
	cct.m_const_wire[1] = _mm_loadu_si128(reinterpret_cast<const __m128i*>(&tmp[0]));

	cct.m_gate_ix = 0;

	cct.m_gen_inp_ix = 0;
	cct.m_evl_inp_ix = 0;
	cct.m_gen_out_ix = 0;
	cct.m_evl_out_ix = 0;

	cct.m_o_bufr.clear();

	tmp.assign(16, 0);
	for (size_t ix = 0; ix < Env::k(); ix++) tmp.set_ith_bit(ix, 1);
	cct.m_clear_mask = _mm_loadu_si128(reinterpret_cast<__m128i*>(&tmp[0]));
}

void evl_init(garbled_circuit_t &cct, const vector<Bytes> &ot_keys, const Bytes &masked_gen_inp, const Bytes &evl_inp)
{
	cct.m_ot_keys = &ot_keys;
	cct.m_gen_inp_mask = masked_gen_inp;
	cct.m_evl_inp = evl_inp;

	cct.m_evl_out.reserve(MAX_OUTPUT_SIZE);
	cct.m_evl_out.clear(); // will grow dynamically
	cct.m_gen_out.reserve(MAX_OUTPUT_SIZE);
	cct.m_gen_out.clear(); // will grow dynamically

	cct.m_gate_ix = 0;

	cct.m_gen_inp_ix = 0;
	cct.m_evl_inp_ix = 0;
	cct.m_gen_out_ix = 0;
	cct.m_evl_out_ix = 0;

	cct.m_i_bufr.clear();

	Bytes tmp(16);
	for (size_t ix = 0; ix < Env::k(); ix++) tmp.set_ith_bit(ix, 1);
	cct.m_clear_mask = _mm_loadu_si128(reinterpret_cast<__m128i*>(&tmp[0]));

	cct.m_bufr.reserve(CIRCUIT_HASH_BUFFER_SIZE);
	cct.m_bufr.clear();
	cct.m_hash.init();
}


void *gen_next_gate(struct PCFState *st, struct PCFGate *current_gate)
{
	garbled_circuit_t &cct =
		*reinterpret_cast<garbled_circuit_t*>(get_external_state(st));

	static __m128i current_zero_key;

	if (current_gate->tag == TAG_INPUT_A)
	{
		__m128i a[2];

		Bytes tmp = cct.m_prng.rand(Env::k());
		tmp.resize(16, 0);
		current_zero_key = _mm_loadu_si128(reinterpret_cast<__m128i*>(&tmp[0]));

		uint32_t gen_inp_ix = current_gate->wire1;

		a[0] = current_zero_key;
		a[1] = _mm_xor_si128(current_zero_key, cct.m_R);

		//uint8_t bit = cct.m_gen_inp_mask.get_ith_bit(gen_inp_ix);
		uint8_t bit = cct.m_gen_inp.get_ith_bit(gen_inp_ix);

		_mm_storeu_si128(reinterpret_cast<__m128i*>(&tmp[0]), a[  bit]);
		cct.m_o_bufr.insert(cct.m_o_bufr.end(), tmp.begin(), tmp.begin()+Env::key_size_in_bytes());

		cct.m_gen_inp_ix++; // after PCF compiler, this isn't really necessary
	}
	else if (current_gate->tag == TAG_INPUT_B)
	{
		__m128i a[2];

		Bytes tmp = cct.m_prng.rand(Env::k());
		tmp.resize(16, 0);
		current_zero_key = _mm_loadu_si128(reinterpret_cast<__m128i*>(&tmp[0]));

		uint32_t evl_inp_ix = current_gate->wire1;

		tmp = (*cct.m_ot_keys)[2*evl_inp_ix+0];
		tmp.resize(16, 0);
		a[0] = _mm_loadu_si128(reinterpret_cast<__m128i*>(&tmp[0]));

		tmp = (*cct.m_ot_keys)[2*evl_inp_ix+1];
		tmp.resize(16, 0);
		a[1] = _mm_loadu_si128(reinterpret_cast<__m128i*>(&tmp[0]));

		// a[0] ^= zero_key; a[1] ^= zero_key ^ R;
		a[0] = _mm_xor_si128(a[0], current_zero_key);
		a[1] = _mm_xor_si128(a[1], _mm_xor_si128(current_zero_key, cct.m_R));

		// cct.m_o_bufr += a[0];
		_mm_storeu_si128(reinterpret_cast<__m128i*>(&tmp[0]), a[0]);
		cct.m_o_bufr.insert(cct.m_o_bufr.end(), tmp.begin(), tmp.begin()+Env::key_size_in_bytes());

		// cct.m_o_bufr += a[1];
		_mm_storeu_si128(reinterpret_cast<__m128i*>(&tmp[0]), a[1]);
		cct.m_o_bufr.insert(cct.m_o_bufr.end(), tmp.begin(), tmp.begin()+Env::key_size_in_bytes());

		cct.m_evl_inp_ix++; // after PCF compiler, this isn't really necessary
	}
	else
	{
#ifdef FREE_XOR
		if (current_gate->truth_table == 0x06) // if XOR gate
		{
			current_zero_key = _mm_xor_si128
			(
				*reinterpret_cast<__m128i*>(get_wire_key(st, current_gate->wire1)),
				*reinterpret_cast<__m128i*>(get_wire_key(st, current_gate->wire2))
			);
		}
		else
#endif
		{
			uint8_t bit;
			__m128i aes_key[2], aes_plaintext, aes_ciphertext;
			__m128i X[2], Y[2], Z[2];
			static Bytes tmp(16, 0);

			aes_plaintext = _mm_set1_epi64x(cct.m_gate_ix);

			X[0] = *reinterpret_cast<__m128i*>(get_wire_key(st, current_gate->wire1));
			Y[0] = *reinterpret_cast<__m128i*>(get_wire_key(st, current_gate->wire2));

			X[1] = _mm_xor_si128(X[0], cct.m_R); // X[1] = X[0] ^ R
			Y[1] = _mm_xor_si128(Y[0], cct.m_R); // Y[1] = Y[0] ^ R

			const uint8_t perm_x = _mm_extract_epi8(X[0], 0) & 0x01; // permutation bit for X
			const uint8_t perm_y = _mm_extract_epi8(Y[0], 0) & 0x01; // permutation bit for Y
			const uint8_t de_garbled_ix = (perm_y<<1)|perm_x; // wire1+2*wire2

			// encrypt the 0-th entry : (X[x], Y[y])
			aes_key[0] = _mm_load_si128(X+perm_x);
			aes_key[1] = _mm_load_si128(Y+perm_y);

			KDF256((uint8_t*)&aes_plaintext, (uint8_t*)&aes_ciphertext, (uint8_t*)aes_key);
			aes_ciphertext = _mm_and_si128(aes_ciphertext, cct.m_clear_mask); // clear extra bits so that only k bits left
			//bit = current_gate.m_table[de_garbled_ix];
			bit = (current_gate->truth_table>>(3-de_garbled_ix))&0x01;

#ifdef GRR
			// GRR technique: using zero entry's key as one of the output keys
			_mm_store_si128(Z+bit, aes_ciphertext);
			Z[1-bit] = _mm_xor_si128(Z[bit], cct.m_R);
			current_zero_key = _mm_load_si128(Z);
#else
			tmp = cct.m_prng.rand(Env::k());
			tmp.resize(16, 0);
			Z[0] = _mm_loadu_si128(reinterpret_cast<__m128i*>(&tmp[0]));
			Z[1] = _mm_xor_si128(Z[0], cct.m_R);

			aes_ciphertext = _mm_xor_si128(aes_ciphertext, Z[bit]);
			_mm_storeu_si128(reinterpret_cast<__m128i*>(&tmp[0]), aes_ciphertext);
			cct.m_o_bufr.insert(cct.m_o_bufr.end(), tmp.begin(), tmp.begin()+Env::key_size_in_bytes());
#endif

			// encrypt the 1st entry : (X[1-x], Y[y])
			aes_key[0] = _mm_xor_si128(aes_key[0], cct.m_R);

			KDF256((uint8_t*)&aes_plaintext, (uint8_t*)&aes_ciphertext, (uint8_t*)aes_key);
			aes_ciphertext = _mm_and_si128(aes_ciphertext, cct.m_clear_mask);
			//bit = current_gate.m_table[0x01^de_garbled_ix];
			bit = (current_gate->truth_table>>(3-(0x01^de_garbled_ix)))&0x01;
			aes_ciphertext = _mm_xor_si128(aes_ciphertext, Z[bit]);
			_mm_storeu_si128(reinterpret_cast<__m128i*>(&tmp[0]), aes_ciphertext);
			cct.m_o_bufr.insert(cct.m_o_bufr.end(), tmp.begin(), tmp.begin()+Env::key_size_in_bytes());

			// encrypt the 2nd entry : (X[x], Y[1-y])
			aes_key[0] = _mm_xor_si128(aes_key[0], cct.m_R);
			aes_key[1] = _mm_xor_si128(aes_key[1], cct.m_R);

			KDF256((uint8_t*)&aes_plaintext, (uint8_t*)&aes_ciphertext, (uint8_t*)aes_key);
			aes_ciphertext = _mm_and_si128(aes_ciphertext, cct.m_clear_mask);
			//bit = current_gate.m_table[0x02^de_garbled_ix];
			bit = (current_gate->truth_table>>(3-(0x02^de_garbled_ix)))&0x01;
			aes_ciphertext = _mm_xor_si128(aes_ciphertext, Z[bit]);
			_mm_storeu_si128(reinterpret_cast<__m128i*>(&tmp[0]), aes_ciphertext);
			cct.m_o_bufr.insert(cct.m_o_bufr.end(), tmp.begin(), tmp.begin()+Env::key_size_in_bytes());

			// encrypt the 3rd entry : (X[1-x], Y[1-y])
			aes_key[0] = _mm_xor_si128(aes_key[0], cct.m_R);

			KDF256((uint8_t*)&aes_plaintext, (uint8_t*)&aes_ciphertext, (uint8_t*)aes_key);
			aes_ciphertext = _mm_and_si128(aes_ciphertext, cct.m_clear_mask);
			//bit = current_gate.m_table[0x03^de_garbled_ix];
			bit = (current_gate->truth_table>>(3-(0x03^de_garbled_ix)))&0x01;
			aes_ciphertext = _mm_xor_si128(aes_ciphertext, Z[bit]);
			_mm_storeu_si128(reinterpret_cast<__m128i*>(&tmp[0]), aes_ciphertext);
			cct.m_o_bufr.insert(cct.m_o_bufr.end(), tmp.begin(), tmp.begin()+Env::key_size_in_bytes());
		}

		if (current_gate->tag == TAG_OUTPUT_A)
		{
			cct.m_o_bufr.push_back(_mm_extract_epi8(current_zero_key, 0) & 0x01); // permutation bit
			cct.m_gen_out_ix++;
		}
		else if (current_gate->tag == TAG_OUTPUT_B)
		{
			cct.m_o_bufr.push_back(_mm_extract_epi8(current_zero_key, 0) & 0x01); // permutation bit
			cct.m_evl_out_ix++;
		}
	}

	cct.m_gate_ix++;
	return &current_zero_key;
}

void * evl_next_gate(struct PCFState *st, struct PCFGate *current_gate)
{
	garbled_circuit_t &cct = *reinterpret_cast<garbled_circuit_t*>(get_external_state(st));

	static __m128i current_key;
	__m128i a;
	static Bytes tmp;

	if (current_gate->tag == TAG_INPUT_A)
	{
		Bytes::const_iterator it = cct.m_i_bufr_ix;
		tmp.assign(it, it+Env::key_size_in_bytes());
		tmp.resize(16, 0);
		current_key = _mm_loadu_si128(reinterpret_cast<__m128i*>(&tmp[0]));

		cct.m_gen_inp_ix++;
	}
	else if (current_gate->tag == TAG_INPUT_B)
	{
		uint32_t evl_inp_ix = current_gate->wire1;

		uint8_t bit = cct.m_evl_inp.get_ith_bit(evl_inp_ix);
		Bytes::const_iterator it = cct.m_i_bufr_ix + bit*Env::key_size_in_bytes();

		tmp = (*cct.m_ot_keys)[evl_inp_ix];
		tmp.resize(16, 0);
		current_key = _mm_loadu_si128(reinterpret_cast<__m128i*>(&tmp[0]));

		tmp.assign(it, it+Env::key_size_in_bytes());
		tmp.resize(16, 0);
		a = _mm_loadu_si128(reinterpret_cast<__m128i*>(&tmp[0]));

		current_key = _mm_xor_si128(current_key, a);

		cct.m_i_bufr_ix += Env::key_size_in_bytes()*2;
		cct.m_evl_inp_ix++;
	}
	else
	{
#ifdef FREE_XOR
		if (current_gate->truth_table == 0x06)
		{
			current_key = _mm_xor_si128
			(
				*reinterpret_cast<__m128i*>(get_wire_key(st, current_gate->wire1)),
				*reinterpret_cast<__m128i*>(get_wire_key(st, current_gate->wire2))
			);
		}
		else
#endif
		{
			__m128i aes_key[2], aes_plaintext, aes_ciphertext;

			aes_plaintext = _mm_set1_epi64x(cct.m_gate_ix);

			aes_key[0] = *reinterpret_cast<__m128i*>(get_wire_key(st, current_gate->wire1));
			aes_key[1] = *reinterpret_cast<__m128i*>(get_wire_key(st, current_gate->wire2));

			const uint8_t perm_x = _mm_extract_epi8(aes_key[0], 0) & 0x01;
			const uint8_t perm_y = _mm_extract_epi8(aes_key[1], 0) & 0x01;

			KDF256((uint8_t*)&aes_plaintext, (uint8_t*)&aes_ciphertext, (uint8_t*)aes_key);
			aes_ciphertext = _mm_and_si128(aes_ciphertext, cct.m_clear_mask);
			uint8_t garbled_ix = (perm_y<<1)|perm_x;

#ifdef GRR
			if (garbled_ix == 0)
			{
				current_key = _mm_load_si128(&aes_ciphertext);
			}
			else
			{
				Bytes::const_iterator it = cct.m_i_bufr_ix+(garbled_ix-1)*Env::key_size_in_bytes();
				tmp.assign(it, it+Env::key_size_in_bytes());
				tmp.resize(16, 0);
				a = _mm_loadu_si128(reinterpret_cast<__m128i*>(&tmp[0]));
				current_key = _mm_xor_si128(aes_ciphertext, a);
			}
			cct.m_i_bufr_ix += 3*Env::key_size_in_bytes();
#else
			it = cct.m_i_bufr_ix + garbled_ix*Env::key_size_in_bytes();
			tmp.assign(it, it+Env::key_size_in_bytes());
			tmp.resize(16, 0);
			current_key = _mm_loadu_si128(reinterpret_cast<__m128i*>(&tmp[0]));
			current_key = _mm_xor_si128(current_key, aes_ciphertext);

			cct.m_i_bufr_ix += 4*Env::key_size_in_bytes();
#endif
		}

		if (current_gate->tag == TAG_OUTPUT_A)
		{
			if (cct.m_gen_out.size()*8 <= cct.m_gen_out_ix)
			{
				// dynamically grown output array
				cct.m_gen_out.resize((cct.m_gen_out.size()+1)*2, 0);
			}
                        //assert(*cct.m_i_bufr_ix < 2);
			uint8_t out_bit = _mm_extract_epi8(current_key, 0) & 0x01;
			out_bit ^= *cct.m_i_bufr_ix;
			cct.m_gen_out.set_ith_bit(cct.m_gen_out_ix, out_bit);
			cct.m_i_bufr_ix++;

			cct.m_gen_out_ix++;
		}
		else if (current_gate->tag == TAG_OUTPUT_B)
		{
			if (cct.m_evl_out.size()*8 <= cct.m_evl_out_ix)
			{
				// dynamically grown output array
				cct.m_evl_out.resize((cct.m_evl_out.size()+1)*2, 0);
			}

			uint8_t out_bit = _mm_extract_epi8(current_key, 0) & 0x01;
			out_bit ^= *cct.m_i_bufr_ix;
			cct.m_evl_out.set_ith_bit(cct.m_evl_out_ix, out_bit);
			cct.m_i_bufr_ix++;

			cct.m_evl_out_ix++;
		}
	}

	update_hash(cct, cct.m_i_bufr);
	cct.m_gate_ix++;

	return &current_key;
}

