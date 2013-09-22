#include <iomanip>

#include <netdb.h>
#include <arpa/inet.h>
#include <mpi.h>
#include <cstring>

#include "YaoBase.h"

#include <log4cxx/logger.h>
static log4cxx::LoggerPtr logger(log4cxx::Logger::getLogger("YaoBase.cpp"));


inline std::string get_IP()
{
	// get local IP and display
	char hostname[1024];
	gethostname(hostname, 1024);
	struct hostent *host = gethostbyname(hostname);
	const std::string local_IP = inet_ntoa(*((struct in_addr *)host->h_addr_list[0]));
	return local_IP;
}


YaoBase::YaoBase(EnvParams &params)
{
	init_cluster(params);
#if defined EVL_CODE || defined GEN_CODE
	init_network(params); // no need in simulation mode
#endif
	init_environ(params);
	init_private(params);

	// [WARNING] no access to params after this point. use Env instead.

	// display local IP
	EVL_BEGIN
		LOG4CXX_INFO(logger, "EVL (" << Env::group_rank() << ") is at " << get_IP());
	EVL_END

	GEN_BEGIN
		LOG4CXX_INFO(logger, "GEN (" << Env::group_rank() << ") is at " << get_IP());
	GEN_END

	if (!Env::is_root())
		return;

	LOG4CXX_INFO(logger, "========================================================");
	LOG4CXX_INFO(logger, "Starting Yao protocol");
	LOG4CXX_INFO(logger, "========================================================");
}

void YaoBase::init_cluster(EnvParams &params)
{
	// inquire world info
	MPI_Comm_rank(MPI_COMM_WORLD, &params.wrld_rank);
	MPI_Comm_size(MPI_COMM_WORLD, &params.node_amnt);

#if defined GEN_CODE || defined EVL_CODE
	params.node_rank = params.wrld_rank;
	m_mpi_comm = MPI_COMM_WORLD;
#else
	if (params.node_amnt % 2 != 0)
	{
		LOG4CXX_FATAL(logger, "statistical parameter s needs to be an even number in the Simulation mode");
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	// divide the world into two groups of the same size (Simulation mode)
	MPI_Comm_split(MPI_COMM_WORLD, params.wrld_rank % 2, params.wrld_rank, &m_mpi_comm);

	// inquire info of the new group
	MPI_Comm_rank(m_mpi_comm, &params.node_rank);
	MPI_Comm_size(m_mpi_comm, &params.node_amnt);
#endif
}


void YaoBase::init_network(EnvParams &params)
{
#if !(defined GEN_CODE || defined EVL_CODE) // just in case (shouldn't reach this point to begin with)
	return;
#endif

	const int IP_SERVER_PORT = params.port_base;
	const int PORT = params.port_base + params.node_rank+1;
	Bytes send, recv;

	// get local IP
	char hostname[1024];
	gethostname(hostname, 1024);
	struct hostent *host = gethostbyname(hostname);
	const std::string local_ip = inet_ntoa(*((struct in_addr *)host->h_addr_list[0]));

	EVL_BEGIN
		// collect IPs from slaves and send them the evaluator via IP server
		send.resize(sizeof(struct in_addr));
		memcpy(&send[0], host->h_addr_list[0], send.size());

		recv.resize(sizeof(struct in_addr)*params.node_amnt); // only used by node 0
		MPI_Gather(&send[0], send.size(), MPI_BYTE, &recv[0], send.size(), MPI_BYTE, 0, m_mpi_comm);

		if (params.node_rank == 0)
		{
			//ServerSocket ip_exchanger(Env::IP_SERVER_PORT);
			ServerSocket ip_exchanger(IP_SERVER_PORT);
			Socket *sock = ip_exchanger.accept();
			sock->write_bytes(recv); // send slaves' IPs to remote
		}

		LOG4CXX_INFO(logger, "EVL (" << params.node_rank << ":" << local_ip << ") is listening at port " << PORT);
		params.server = new ServerSocket(PORT);
		params.remote = params.server->accept();
		LOG4CXX_INFO(logger, "EVL (" << params.node_rank << ":" << local_ip << ") is connected at port " << PORT);
	EVL_END

	GEN_BEGIN
		// receive IPs from the generator via IP server and forward them to slaves
		if (params.node_rank == 0)
		{
			//ClientSocket ip_exchanger(params.ipserve_addr, Env::IP_SERVER_PORT);
			ClientSocket ip_exchanger(params.ipserve_addr, IP_SERVER_PORT);
			send = ip_exchanger.read_bytes(); // receive the generator's slaves' IPs
		}

		recv.resize(sizeof(struct in_addr));
		MPI_Scatter(&send[0], recv.size(), MPI_BYTE, &recv[0], recv.size(), MPI_BYTE, 0, m_mpi_comm);

		std::string remote_ip = inet_ntoa(*((struct in_addr *)&recv[0]));
		LOG4CXX_INFO(logger, "GEN (" << params.node_rank << ":" << local_ip << ") is connecting (" <<  remote_ip << ") at port " << PORT);
		params.remote = new ClientSocket(remote_ip.c_str(), PORT);
		LOG4CXX_INFO(logger, "GEN (" << params.node_rank << ":" << local_ip << ") succeeded connecting");
	GEN_END
}


void YaoBase::init_environ(EnvParams &params)
{
	if (params.secu_param % 8 != 0 || params.secu_param > 128)
	{
		LOG4CXX_FATAL(logger, "security parameter k needs to be a multiple of 8 less than or equal to 128");
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	if (params.stat_param % params.node_amnt != 0)
	{
		LOG4CXX_FATAL(logger, "statistical  parameter s needs to be a multiple of cluster size");
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	// # of copies of a circuit each node is responsible for
	params.node_load = params.stat_param/params.node_amnt;

	//if (!params.circuit.load_binary(params.circuit_file))
	//{
	//	LOG4CXX_FATAL(logger, "circuit parsing failed");
	//	MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	//}

	Env::init(params);

	// synchronize claw-free collections
	ClawFree claw_free;
	claw_free.init();
	Bytes bufr(claw_free.size_in_bytes());

	if (Env::is_root())
	{
		EVL_BEGIN
			bufr = claw_free.to_bytes();
			EVL_SEND(bufr);
		EVL_END

		GEN_BEGIN
			bufr = GEN_RECV();
		GEN_END
	}

	// synchronize claw-free collections to the root evaluator's
	MPI_Bcast(&bufr[0], bufr.size(), MPI_BYTE, 0, m_mpi_comm);
	Env::claw_free_from_bytes(bufr);
}


void YaoBase::init_private(EnvParams &params)
{
	static byte MASK[8] = { 0xFF, 0x01, 0x03, 0x07, 0x0F, 0x1F, 0x3F, 0x7F};

	std::ifstream private_file(params.private_file);
	std::string input;

	if (!private_file.is_open())
	{
		LOG4CXX_FATAL(logger, "file open failed: " << params.private_file);
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	EVL_BEGIN // evaluator
		private_file >> input;          // 1st line is the evaluator's input
		m_evl_inp.from_hex(input);
		//m_evl_inp.resize((Env::circuit().evl_inp_cnt()+7)/8);
		//m_evl_inp.back() &= MASK[Env::circuit().evl_inp_cnt()%8];
	EVL_END

	GEN_BEGIN // generator
		private_file >> input >> input; // 2nd line is the generator's input
		m_gen_inp.from_hex(input);
		//m_gen_inp.resize((Env::circuit().gen_inp_cnt()+7)/8);
		//m_gen_inp.back() &= MASK[Env::circuit().gen_inp_cnt()%8];
	GEN_END

	private_file.close();
}


YaoBase::~YaoBase()
{
	Env::destroy();

	int res;
	MPI_Comm_compare(m_mpi_comm, MPI_COMM_WORLD, &res);
	if (res == MPI_UNEQUAL) MPI_Comm_free(&m_mpi_comm); // MPI_COMM_WORLD can't be freed
}


Bytes YaoBase::recv_data(int src_node)
{
	MPI_Status status;

	uint32_t comm_sz;
	MPI_Recv(&comm_sz, 1, MPI_INT, src_node, 0, MPI_COMM_WORLD, &status);

	Bytes recv(comm_sz);
	MPI_Recv(&recv[0], recv.size(), MPI_BYTE, src_node, 0, MPI_COMM_WORLD, &status);

	return recv;
}


void YaoBase::send_data(int dst_node, const Bytes &data)
{
	assert(data.size() < INT_MAX);

	uint32_t comm_sz = data.size();
	MPI_Send(&comm_sz, 1, MPI_INT, dst_node, 0, MPI_COMM_WORLD);

	MPI_Send(const_cast<byte*>(&data[0]), data.size(), MPI_BYTE, dst_node, 0, MPI_COMM_WORLD);
}



inline std::string print_longlong(uint64_t l)
{
	char buf[8];
	std::string str;

	while (l >= 1000)
	{
		sprintf(buf, ",%03d", (int)(l%1000));
		str = buf + str;

		l = l / 1000LL;
	}

	sprintf(buf, "%d", (int)l);
	str = buf + str;

	return str;
}


void YaoBase::step_init()
{
    m_timer_gen = m_timer_evl = m_timer_mpi = m_timer_com = 0;
    m_comm_sz = 0;
}


void YaoBase::step_report(std::string step_name)
{
	double start = MPI_Wtime();
		MPI_Barrier(MPI_COMM_WORLD);
	m_timer_mpi += MPI_Wtime() - start;

	step_report_no_sync(step_name);
}


void YaoBase::step_report_no_sync(std::string step_name)
{
	uint64_t all_comm_sz = 0LL;

	MPI_Reduce(&m_comm_sz, &all_comm_sz, 1, MPI_LONG_LONG_INT, MPI_SUM, 0, m_mpi_comm);

	if (!Env::is_root())
		return;

	m_timer_mpi_vec.push_back(m_timer_mpi);
	m_timer_cmm_vec.push_back(m_timer_com);
	m_step_name_vec.push_back(step_name);
	m_comm_sz_vec.push_back(all_comm_sz);

	EVL_BEGIN
		m_timer_cmp_vec.push_back(m_timer_evl);
		LOG4CXX_INFO(logger, "EVL finish " << step_name << "");
	EVL_END

	GEN_BEGIN
		m_timer_cmp_vec.push_back(m_timer_gen);
		LOG4CXX_INFO(logger, "GEN finish " << step_name << "");
	GEN_END
}


void YaoBase::final_report()
{
	if (!Env::is_root())
		return;

	LOG4CXX_INFO(logger, "========================================================");
	LOG4CXX_INFO(logger, "Yao protocol completed");
	LOG4CXX_INFO(logger, "========================================================");

	std::string name;

	EVL_BEGIN
		name = "EVL";
		LOG4CXX_INFO(logger, "EVL  input: " << m_evl_inp.to_hex() << "");
		LOG4CXX_INFO(logger, "EVL output: " << m_evl_out.to_hex() << "");
	EVL_END

	GEN_BEGIN
		name = "GEN";
		LOG4CXX_INFO(logger, "GEN  input: " << m_gen_inp.to_hex() << "");
		LOG4CXX_INFO(logger, "GEN output: " << m_gen_out.to_hex() << "");
	GEN_END

	for (size_t i = 0; i < m_comm_sz_vec.size(); i++)
	{
		LOG4CXX_INFO
		(
			logger,
			name << " in " << m_step_name_vec[i] << "> " << std::fixed <<
			"  cmp:"  << std::setw(12) << std::setprecision(4) << m_timer_cmp_vec[i] <<
			", cmm:"  << std::setw(12) << std::setprecision(4) << m_timer_cmm_vec[i] <<
			", mpi:"  << std::setw(12) << std::setprecision(4) << m_timer_mpi_vec[i] <<
			", size:" << std::setw(16) << std::setprecision(4) << print_longlong(m_comm_sz_vec[i])
		);
	}
}


