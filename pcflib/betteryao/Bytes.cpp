#include <iostream>
//#include <openssl/sha.h>

#include "Bytes.h"
#include "Hash.h"

Bytes Bytes::hash(size_t bits) const
{
	return Hash(*this).sig(bits);
}

namespace
{
const char HEX_TABLE[16] =
{
	'0', '1', '2', '3', '4', '5', '6', '7',
	'8', '9', 'A', 'B', 'C', 'D', 'E', 'F'
};
}

std::string Bytes::to_hex() const
{
	std::string out;
	out.reserve(this->size()*2);

	for (const_iterator it = this->begin(); it != this->end(); it++)
	{
		out.push_back(HEX_TABLE[(*it) / 16]);
		out.push_back(HEX_TABLE[(*it) % 16]);
	}
	return out;
}

namespace
{
const int REVERSE_HEX_TABLE[256] =
{
		-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
		-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
		-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
		 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, -1, -1, -1, -1, -1, -1,
		-1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1,
		-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
		-1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1,
		-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
		-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
		-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
		-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
		-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
		-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
		-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
		-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
		-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
};

const int HEX_EXP[2] = { 16, 1 };
}

void Bytes::from_hex(const std::string &s)
{
	this->clear();
	this->resize((s.size()+1)/2, 0); // zero out *this

	for (size_t ix = 0; ix < s.size(); ix++)
	{
		if (REVERSE_HEX_TABLE[s[ix]] == -1)
		{
            std::cerr << "Invalid hex format: " << s;
		}
		(*this)[ix/2] += REVERSE_HEX_TABLE[s[ix]]*HEX_EXP[ix%2];
	}
  // // Better way:  just read the hex nibbles one-by-one, straight up
  // uint8_t q;
  // for(int i = 0; i < s.size(); i++)
  //   {
  //     q = s[i];
  //     for(int j = 0; j < 4; j++)
  //       {
  //         this->set_ith_bit(i * 4 + j, q & 0x01);
  //         q = q >> 1;
  //       }
  //   }
}

std::vector<Bytes> Bytes::split(const size_type chunk_len) const
{
	assert(this->size() % chunk_len == 0);

	std::vector<Bytes> chunks(this->size()/chunk_len);

	const_iterator it = this->begin();
	for (int ix = 0; ix < chunks.size(); ix++, it += chunk_len)
		chunks[ix] = Bytes(it, it+chunk_len);

	return chunks;
}

// return chunks[0]||chunks[1]||...||chunks[chunks.size()-1]
void Bytes::merge(const std::vector<Bytes> &chunks)
{
	if (chunks.size() == 0) { return; }

	this->clear();
	this->reserve(chunks.size()*chunks[0].size());

	for (size_t ix = 0; ix < chunks.size(); ix++) { *this += chunks[ix]; }
}

