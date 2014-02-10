#ifndef GARBLED_CIRCUIT_H_
#define GARBLED_CIRCUIT_H_

#include <emmintrin.h>

#include "Env.h"
#include "Prng.h"
#include "Hash.h"

extern "C" {
#include "pcflib.h"
}

typedef struct
{
	Bytes               m_bufr;
	Hash                m_hash;

	__m128i             m_R;

	const std::vector<Bytes>  *m_ot_keys;

	Prng                m_prng;

	uint64_t            m_gate_ix;

	uint32_t            m_gen_inp_ix;
	uint32_t            m_evl_inp_ix;
	uint32_t            m_gen_out_ix;
	uint32_t            m_evl_out_ix;

	__m128i             m_clear_mask;

	Bytes               m_gen_inp_mask;
	Bytes               m_gen_inp;
	Bytes               m_evl_inp;
	Bytes               m_gen_out;
	Bytes               m_evl_out;

	Bytes               m_o_bufr;
	Bytes               m_i_bufr;
	Bytes::iterator     m_i_bufr_ix;

	struct PCFState    *m_st;
	__m128i             m_const_wire[2]; // keys for constant 0 and 1
}
garbled_circuit_t;

void gen_init(garbled_circuit_t &cct, const std::vector<Bytes> &keys, const Bytes &gen_inp_mask, const Bytes &seed);
void evl_init(garbled_circuit_t &cct, const std::vector<Bytes> &keys, const Bytes &masked_gen_inp, const Bytes &seed);

inline void trim_output(garbled_circuit_t &cct)
{
	cct.m_gen_out.resize((cct.m_gen_out_ix+7)/8);
	cct.m_evl_out.resize((cct.m_evl_out_ix+7)/8);
}

inline void recv(garbled_circuit_t &cct, const Bytes &i_data)
{
	cct.m_i_bufr.clear();
	cct.m_i_bufr += i_data;
        //assert(cct.m_i_bufr.size() > 0);
	cct.m_i_bufr_ix = cct.m_i_bufr.begin();
}

inline const Bytes send(garbled_circuit_t &cct)
{
	Bytes o_data;
	o_data.swap(cct.m_o_bufr);
	return o_data;
}

#define  _mm_extract_epi8(x, imm) \
	((((imm) & 0x1) == 0) ?   \
	_mm_extract_epi16((x), (imm) >> 1) & 0xff : \
	_mm_extract_epi16( _mm_srli_epi16((x), 8), (imm) >> 1))

Bytes KDF128(const Bytes &in, const Bytes &key);
Bytes KDF256(const Bytes &in, const Bytes &key);

void KDF128(const uint8_t *in, uint8_t *out, const uint8_t *key);
void KDF256(const uint8_t *in, uint8_t *out, const uint8_t *key);

void set_const_key(garbled_circuit_t &cct, byte c, const Bytes &key);
const Bytes &get_const_key(garbled_circuit_t &cct, byte c, byte b);

#ifdef __CPLUSPLUS
extern "C" {
#endif

void *copy_key(void *);
void delete_key(void *);

void *gen_next_gate(struct PCFState *st, struct PCFGate *gate);
void *evl_next_gate(struct PCFState *st, struct PCFGate *gate);

#ifdef __CPLUSPLUS
}
#endif

#endif
