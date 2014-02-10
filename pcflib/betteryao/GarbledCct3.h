#ifndef GARBLEDCCT3_H_
#define GARBLEDCCT3_H_

#include <emmintrin.h>

#include "Env.h"
#include "Prng.h"
#include "Hash.h"

class GarbledCct3
{
	void init();

public:
	GarbledCct3() : m_w(0), m_pass_chk(true) { init(); }
	~GarbledCct3() { delete [] m_w; }

	void gen_init(const std::vector<Bytes> &keys, const Bytes &gen_inp_mask, const Bytes &seed);
	void gen_next_gate(const Gate &g);

	void com_init(const std::vector<Bytes> &keys, const Bytes &gen_inp_mask, const Bytes &seed);
	void com_next_gate(const Gate &g);

	void evl_init(const std::vector<Bytes> &keys, const Bytes &masked_gen_inp, const Bytes &evl_inp);
	void evl_next_gate(const Gate &g);

	void gen_next_gen_inp_com(const Bytes &row, size_t kx);
	void evl_next_gen_inp_com(const Bytes &row, size_t kx);

	bool pass_check() const;
private:
	void update_hash(const Bytes &data);

	Bytes               m_bufr;
	Hash                m_hash;

	__m128i             m_R;

	const std::vector<Bytes>  *m_ot_keys;

	Prng                m_prng;

	uint64_t            m_gate_ix;

	uint32_t            m_gen_inp_hash_ix;
	uint32_t            m_gen_inp_ix;
	uint32_t            m_evl_inp_ix;
	uint32_t            m_gen_out_ix;
	uint32_t            m_evl_out_ix;

	__m128i *           m_w;
	__m128i             m_clear_mask;

public:
	size_t            	m_max_map_size;

	Bytes               m_gen_inp_mask;
	Bytes               m_evl_inp;
	Bytes               m_gen_out;
	Bytes               m_evl_out;

	std::vector<Bytes>  m_gen_inp_com;
	std::vector<Bytes>  m_gen_inp_decom;
	Bytes               m_gen_inp_hash;

private:
	bool                m_pass_chk;
	Bytes               m_o_bufr;
	Bytes               m_i_bufr;
	Bytes::iterator     m_i_bufr_ix;

public:
	const Bytes hash()
	{
#ifdef RAND_SEED
		return m_hash.sig(Env::k());
#else
		return m_bufr;
#endif
	}

	void recv(const Bytes &i_data)
	{
		m_i_bufr.clear();
		m_i_bufr += i_data;
		m_i_bufr_ix = m_i_bufr.begin();
	}

	const Bytes send()
	{
		static Bytes o_data;
		o_data.swap(m_o_bufr);
		m_o_bufr.clear();
		return o_data;
	}
};

#define  _mm_extract_epi8(x, imm) \
	((((imm) & 0x1) == 0) ?   \
	_mm_extract_epi16((x), (imm) >> 1) & 0xff : \
	_mm_extract_epi16( _mm_srli_epi16((x), 8), (imm) >> 1))

void KDF128(const uint8_t *in, uint8_t *out, const uint8_t *key);
void KDF256(const uint8_t *in, uint8_t *out, const uint8_t *key);

#endif /* GARBLEDCCT3_H_ */
