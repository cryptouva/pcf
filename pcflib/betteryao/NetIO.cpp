#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <errno.h>

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include "NetIO.h"

const int CHUNK_SIZE = 1024;

Socket::Socket() : m_socket(socket(AF_INET, SOCK_STREAM, IPPROTO_TCP))
{}

Socket::~Socket()
{
	shutdown(m_socket, SHUT_RDWR);
    close(m_socket);
}

void Socket::write_bytes(const Bytes &bytes)
{
  //  assert(bytes.size() > 0);
	uint32_t sz = htonl(bytes.size());

	send(m_socket, &sz, sizeof(sz), 0);
	sz = bytes.size();

	size_t ix = 0;
	for (; sz > CHUNK_SIZE; sz -= CHUNK_SIZE, ix += CHUNK_SIZE)
			send(m_socket, &bytes[ix], CHUNK_SIZE, 0);
	send(m_socket, &bytes[ix], bytes.size()-ix, 0);
}

inline void my_read(int socket, void *data, size_t n)
{
	for (size_t ix = 0; ix != n; )
		ix += recv(socket, reinterpret_cast<char*>(data)+ix, n-ix, 0);
}

Bytes Socket::read_bytes()
{
        uint32_t sz, ix;
        my_read(m_socket, &sz, 4);
        sz = ntohl(sz);

        Bytes bytes(sz);
        for (ix = 0; sz > CHUNK_SIZE; sz -= CHUNK_SIZE, ix += CHUNK_SIZE)
        	my_read(m_socket, &bytes[0]+ix, CHUNK_SIZE);

        my_read(m_socket, &bytes[0]+ix, bytes.size()-ix);

        //        assert(bytes.size() > 0);

        return bytes;
}

void Socket::write_string(const std::string &str)
{
	byte *ptr = (byte *)(str.c_str());
	write_bytes(Bytes(ptr, ptr+str.size()));
}

std::string Socket::read_string()
{
	Bytes bytes = read_bytes();
	char *ptr = reinterpret_cast<char*>(&bytes[0]);
	return std::string(ptr, bytes.size());
}

const int MAX_SLEEP = 1024;

ClientSocket::ClientSocket(const char *host_ip, size_t port)
{
	struct sockaddr_in addr;
	int res;

	if (-1 == m_socket)
	{
		perror("cannot create socket");
		exit(EXIT_FAILURE);
	}

	memset(&addr, 0, sizeof(addr));

	addr.sin_family = AF_INET;
	addr.sin_port = htons(port);
	res = inet_pton(AF_INET, host_ip, &addr.sin_addr);

	if (0 > res)
	{
		perror("error: first parameter is not a valid address family");
		close(m_socket);
		exit(EXIT_FAILURE);
	}
	else if (0 == res)
	{
		perror("error: second parameter does not contain valid IP address");
		close(m_socket);
		exit(EXIT_FAILURE);
	}

	// exponential backoff algorithm
	for (int sec = 1; sec < MAX_SLEEP; sec <<= 1)
	{
		int ret;
		if (0 == (ret = connect(m_socket, (struct sockaddr *)&addr, sizeof(addr))))
		{
			return;
		}

		sleep(sec);

		// state of m_socket is unspecified after failure. need to recreate
		close(m_socket);
		m_socket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
	}

	perror("connect failed");
	close(m_socket);
	exit(EXIT_FAILURE);
}

ServerSocket::ServerSocket(size_t port)
{
	struct sockaddr_in addr;

	if(-1 == m_socket)
	{
		perror("can not create socket");
		exit(EXIT_FAILURE);
	}

	memset(&addr, 0, sizeof addr);

	addr.sin_family = AF_INET;
	addr.sin_port = htons(port);
	addr.sin_addr.s_addr = INADDR_ANY;

        int trueflag = -1;

        if(setsockopt(m_socket, SOL_SOCKET, SO_REUSEADDR, &trueflag, sizeof(int)) < 0)
          perror("Could  not set socket option?");

	// exponential backoff algorithm
	for (int sec = 1; sec < MAX_SLEEP; sec <<= 1)
	{
          if(0 == bind(m_socket,(struct sockaddr *)&addr, sizeof addr))
            {
              //              perror("error bind failed");
              //     close(m_socket);
              //              exit(EXIT_FAILURE);
              break;
            }
          sleep(sec);
        }

	// exponential backoff algorithm
	for (int sec = 1; sec < MAX_SLEEP; sec <<= 1)
	{
          if(0 == listen(m_socket, 10))
            {
              //     perror("error listen failed");
              //close(m_socket);
              //exit(EXIT_FAILURE);
              break;
            }
          sleep(sec);
        }
}

Socket *ServerSocket::accept()
{
	int socket = ::accept(m_socket, NULL, NULL);

	if(socket < 0)
	{
		fprintf(stderr, "accept() failed: %s\n", strerror(errno));
		close(m_socket);
		exit(EXIT_FAILURE);
	}

	m_sockets.push_back(socket);

	return new Socket(socket);
}

ServerSocket::~ServerSocket() {}
