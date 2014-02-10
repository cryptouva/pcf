#ifndef BETTERYAO4_H_
#define BETTERYAO4_H_

#include "YaoBase.h"
#include "GarbledCct3.h"
#include "garbled_circuit_m.h"

class BetterYao4 : public YaoBase
{
public:
	BetterYao4(EnvParams &params);
	virtual ~BetterYao4() {}

	virtual void start();

	void oblivious_transfer();
	void cut_and_choose();
	void cut_and_choose2();
	void consistency_check();
	void circuit_evaluate();

private:
	void ot_init();
	void ot_random(); // sender has m pairs of l-bit strings, and receiver has m bits
	void cut_and_choose2_ot();
	void cut_and_choose2_precomputation();
	void cut_and_choose2_evl_circuit(size_t ix);
	void cut_and_choose2_chk_circuit(size_t ix);

	Bytes flip_coins(size_t len);

	void proc_gen_out();
	void proc_evl_out();

	// variables for IKNP03 OT-extension implementation
	G                               m_ot_g[2];
	G                               m_ot_h[2];

	size_t                          m_ot_bit_cnt;
	Bytes                           m_ot_recv_bits;
	vector<Bytes>                   m_ot_send_pairs;
	vector<Bytes>                   m_ot_out;

	vector<vector<Bytes> >          m_ot_keys; // ot output

	// variables for cut-and-choose
	Bytes                           m_chks;
	Bytes                           m_all_chks;

	// variables for Yao protocol
	vector<Bytes>                   m_gen_inp_masks;
	vector<Bytes>                   m_coms;
	vector<Bytes>                   m_rnds;
    vector<GarbledCct3>              m_ccts;
	vector<garbled_circuit_m_t>     m_gcs; 

    // variables for Gen's input check
    vector<Bytes>                   m_gen_inp_hash;
	vector<vector<Bytes> >          m_gen_inp_decom;
	vector<Bytes>                   m_matrix;

	vector<Prng>					m_prngs;
	uint32_t                        m_gen_inp_cnt;
	uint32_t                        m_evl_inp_cnt;
};


#endif /* BETTERYAO4_H_ */
