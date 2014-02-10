#ifndef BYTES_H_
#define BYTES_H_

#include <stdint.h>
#include <cassert>
#include <string>
#include <vector>

typedef uint8_t byte;

class Bytes : public std::vector<byte>
{
	typedef uint64_t        chunk_t;
	typedef chunk_t *       chunk_iterator;
	typedef const chunk_t * const_chunk_iterator;

	enum { CHUNK_SIZE = sizeof(chunk_t) };

public:
	typedef byte *          iterator;
	typedef const byte *    const_iterator;

	Bytes() {}

	// a byte array of size n
	Bytes(uint64_t n) : std::vector<byte>(n) {}

	// a byte array containing n copies of b
	Bytes(uint64_t n, byte b) : std::vector<byte>(n, b) {}

	// merge vector<Bytes> into Bytes.
	Bytes(const std::vector<Bytes> &chunks) { merge(chunks); }

	// optimized constructor: copy in chunks, if possible
	Bytes(const_iterator first, const_iterator last) :
		std::vector<byte>(last - first)
	{
		fast_copy(first, last, this->begin());
	}

	const Bytes &operator =(const Bytes &rhs)
	{
		assign(rhs.begin(), rhs.end());
		return *this;
	}

	const Bytes &operator +=(const Bytes &rhs)
	{
		insert(this->end(), rhs.begin(), rhs.end());
		return *this;
	}

	// pos needs to be inside [this->begin(), this->end())
	void insert(iterator pos, const_iterator first, const_iterator last)
	{
		size_type old_size = pos - this->begin();
		size_type new_size = old_size + (last - first);

		resize(new_size); // pos is invalidated
		fast_copy(first, last, this->begin() + old_size);
	}

	void assign(size_type n, const value_type v)
	{
		std::vector<byte>::assign(n, v);
	}

	// [begin, end) needs to be a valid range
	void assign(const_iterator first, const_iterator last)
	{
		this->resize(last - first);
		fast_copy(first, last, this->begin());
	}

	iterator begin() { return &(*this)[0]; }
	const_iterator begin() const { return &(*this)[0]; }

	iterator end() { return &(*this)[0]+size(); }
	const_iterator end() const { return &(*this)[0]+size(); }

	// Optimized operator==: divide an array into multi-byte chunks
	// plus possible head section and tail section.
	bool operator ==(const Bytes &rhs) const
	{
		if (this->size() != rhs.size()) { return false; }

		bool ret = true;
		const_iterator first1 = this->begin();
		const_iterator last1 = this->end();
		const_iterator first2 = rhs.begin();

		if ((first1 - first2) % CHUNK_SIZE == 0)
		{
			// first and result can be aligned with boundaries
			while ((uintptr_t)(void*)first1 % CHUNK_SIZE)
			{
				if (first1 == last1) return ret; // reach the end and return
				ret &= (*first1++ == *first2++);
			} // first and result are both aligned with chunk boundaries

			const_chunk_iterator first1_chunk =
				reinterpret_cast<const_chunk_iterator>(first1);
			const_chunk_iterator last1_chunk =
				reinterpret_cast<const_chunk_iterator>
					(last1 - (uintptr_t)(void*)last1 % CHUNK_SIZE);
			const_chunk_iterator first2_chunk =
				reinterpret_cast<const_chunk_iterator>(first2);

			// compare in chunks
			while (first1_chunk != last1_chunk) { ret &= (*first1_chunk++ == *first2_chunk++); }

			first1 = reinterpret_cast<const_iterator>(first1_chunk);
			first2 = reinterpret_cast<const_iterator>(first2_chunk);
		}

		// compare the rest in bytes
		while (first1 != last1) { ret &= (*first1++ == *first2++); }

		return ret;
	}

	// Optimized operator^=: process in chunks, if possible
	const Bytes &operator ^=(const Bytes &rhs)
	{
		assert(this->size() == rhs.size());

		const_iterator first = rhs.begin();
		const_iterator last = rhs.end();
		iterator result = this->begin();

		if ((first - result) % CHUNK_SIZE == 0)
		{
			// first and result can be aligned with chunk boundaries
			while ((uintptr_t)(void*)first % CHUNK_SIZE)
			{
				if (first == last) return *this; // reach the end and return
				*result++ ^= *first++;
			} // first and result are both aligned with chunk boundaries

			const_chunk_iterator first_chunk =
				reinterpret_cast<const_chunk_iterator>(first);
			const_chunk_iterator last_chunk =
				reinterpret_cast<const_chunk_iterator>
					(last - (uintptr_t)(void*)last % CHUNK_SIZE);
			chunk_iterator result_chunk =
				reinterpret_cast<chunk_iterator>(result);

			// copy in chunks
			while (first_chunk != last_chunk) { *result_chunk++ ^= *first_chunk++; }

			first = reinterpret_cast<const_iterator>(first_chunk);
			result = reinterpret_cast<iterator>(result_chunk);
		}

		// copy the rest in bytes
		while (first != last) { *result++ ^= *first++; }

		return *this;
	}

	byte get_ith_bit(uint64_t ix) const
	{
		assert(ix < (this->size()*8LL));
		return ((*this)[ix/8] >> (ix%8)) & 0x01;
	}

	void set_ith_bit(uint64_t ix, byte bit)
	{
		assert(ix < (this->size()*8LL));
		assert(bit <= 1);

		const byte CLEAR_MASK[8] = { 0xFE, 0xFD, 0xFB, 0xF7, 0xEF, 0xDF, 0xBF, 0x7F };

		(*this)[ix/8] &= CLEAR_MASK[ix%8];
		(*this)[ix/8] |= bit << (ix%8);
	}

	std::string to_hex() const;
	void from_hex(const std::string &s);

	Bytes hash(size_t bits) const;
	std::vector<Bytes> split(const size_t chunk_len) const;

private:
	void merge(const std::vector<Bytes> &chunks);

	//
	// Copy in chunks, if possible.
	//
	// [first, last) and [result, result + (last-first)) need to be valid ranges
	//
	static void
	fast_copy(const_iterator first, const_iterator last, iterator result)
	{
/*
		if ((first - result) % CHUNK_SIZE == 0)
		{
			// first and result can be aligned with chunk boundaries
			while ((uintptr_t)(void*)first % CHUNK_SIZE)
			{
				if (first == last) return; // reach the end and return
				*result++ = *first++;
			} // first and result are both aligned with chunk boundaries

			const_chunk_iterator first_chunk =
				reinterpret_cast<const_chunk_iterator>(first);
			const_chunk_iterator last_chunk =
				reinterpret_cast<const_chunk_iterator>
					(last - (uintptr_t)(void*)last % CHUNK_SIZE);
			chunk_iterator result_chunk =
				reinterpret_cast<chunk_iterator>(result);

			// copy in chunks
			while (first_chunk != last_chunk) { *result_chunk++ = *first_chunk++; }

			first = reinterpret_cast<const_iterator>(first_chunk);
			result = reinterpret_cast<iterator>(result_chunk);
		}
*/
		// copy the rest in bytes
		while (first != last) { *result++ = *first++; }
	}
};

inline Bytes operator^ (const Bytes &lhs, const Bytes &rhs)
{
	assert(lhs.size() == rhs.size());
	Bytes ret(lhs);
	ret ^= rhs;
	return ret;
}

inline Bytes operator+ (const Bytes &lhs, const Bytes &rhs)
{
	Bytes ret;
	ret.reserve(lhs.size() + rhs.size());
	ret += lhs;
	ret += rhs;
	return ret;
}
#endif /* BYTES_H_ */
