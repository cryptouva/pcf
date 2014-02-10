#ifndef HASH_H_
#define HASH_H_

#include <openssl/sha.h>
#include "Bytes.h"

class Hash
{
	SHA256_CTX m_sha256;

public:
	Hash()
	{
		init();
	}

	Hash(const Bytes &msg)
	{
		init();
		update(msg);
	}

	void init()
	{
		SHA256_Init(&m_sha256);
	}

	void update(const Bytes &msg)
	{
		SHA256_Update(&m_sha256, &msg.at(0), msg.size());
	}

	Bytes sig(size_t bits)
	{
		const byte MASK[8] =
		{
			0xFF, 0x01, 0x03, 0x07, 0x0F, 0x1F, 0x3F, 0x7F
		};

		assert((0 < bits) && (bits <= 256));

		Bytes hash(SHA256_DIGEST_LENGTH);
		SHA256_Final(&hash.at(0), &m_sha256);

		hash.resize((bits+7)/8);
		hash.back() &= MASK[bits % 8]; // clear the extra bits

		return hash;
	}
};

#endif /* HASH_H_ */
