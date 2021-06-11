#include <iostream>
#include <iomanip>
#include <map>
#include <set>
#include <string>
#include <sstream>
#include <stdexcept>
#include <atomic>
using namespace std;

#include <cstring>
#include <cerrno>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <linux/if_packet.h>
#include <linux/if_arp.h>
#include <net/ethernet.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/time.h>
#include <unistd.h>
#include <signal.h>

/// Length in bytes of one Hardware Address.
#define MAC_ADDR_LEN	6

/// Length in bytes of one IP Address.
#define IP_ADDR_LEN		4

/// Defines the IP 0.0.0.1.
#define IP_ONE		htonl( 1 )

/// The maximum number of attempts to resolve a HW Address.
#define MAX_TRIES_FOR_RESOLV	5

// ===============================
// Global variables
// ===============================
atomic<bool> active( true ); ///< Controls the guard() function.



// ===============================
// Data types
// ===============================

/**
 * Stores a MAC Address
 */
struct HWAddr{
	uint8_t hw[MAC_ADDR_LEN];

	/**
	 * Creates an HWAddr object from an array of bytes with length size of 6.
	 *
	 * @param m The array with the 6 bytes of the HW Address.
	 */
	HWAddr( const uint8_t m[] ){
		memcpy( hw, m, MAC_ADDR_LEN );
	}

	bool operator < ( const HWAddr &m ) const {
		return memcmp( hw, m.hw, MAC_ADDR_LEN ) < 0;
	}

	/**
	 * Get a string representation of the HW Address in
	 * the format xx:xx:xx:xx:xx:xx.
	 *
	 * @return The string representation of the HW Address.
	 */
	string toString() const {
		ostringstream out;

		out << hex << setfill( '0' ) << setw( 2 );
		for( int i = 0 ; i < MAC_ADDR_LEN ; ){
			out << static_cast<int>( hw[i] );
			if( hw[i] == 0 )
				out << 0;
			if( ++i != MAC_ADDR_LEN )
				out << ':';
		}
		return out.str();
	}

};

/**
 * The representation of a ARP Frame including the 
 * ethernet header.
 */
struct ARPFrame{
	uint8_t		eth_dst[MAC_ADDR_LEN]; 	///< Ethernet destination address.
	uint8_t		eth_src[MAC_ADDR_LEN]; 	///< Ethernet source address.
	uint16_t	eth_ethertype;			///< Eheternet ethertype.
	uint16_t	hw_type; 				///< ARP hardware type (Ethernet 0x0001).
	uint16_t	protocol;				///< ARP protocol (IP 0x0800).
	uint8_t		hw_len;					///< ARP lenght in bytes of hardware address.
	uint8_t		proto_len;				///< ARP lenght in byte of protocol address.
	uint16_t	opcode;					///< ARP Operation code (0x0001 for request, 0x0002 for reply).
	uint8_t		hw_src[MAC_ADDR_LEN]; 	///< ARP source HW address.
	uint32_t	ip_src;					///< ARP source Protocol address.
	uint8_t		hw_dst[MAC_ADDR_LEN];	///< ARP target HW address.
	uint32_t	ip_dst;					///< ARP target Protocol address.
} __attribute__((__packed__));

/** Represents a key-value table.*/
typedef map< HWAddr, struct in_addr> ARPTable;

/** Stores some info about the netdevice */
struct LocalData{
	int ifindex;					///< Index of the network interface.
	uint32_t ipAddr;				///< IP Address of the network interface.
	uint32_t firstHost;				///< IP Address of the first host in the network.
	uint32_t lastHost;				///< IP Address of the last host in the network.
	uint8_t hwAddr[MAC_ADDR_LEN];	///< Hawrdware Address of the network interface.
};


// ===============================
// Functions
// ===============================

/**
 * Get the local data of the netdevice given
 * the interface name.
 *
 * @param ifname The name of the network interface
 * @return A LocalData object with the local information.
 *
 * @throw runtime_error If some field couldn't be acquired.
 */
LocalData loadLocalData( const char *ifname ) throw( runtime_error ) 
{
	struct ifreq nic;
	int sock = socket( AF_INET, SOCK_STREAM, 0 );
	LocalData data;

	strncpy( nic.ifr_name, ifname, IFNAMSIZ-1 );
	nic.ifr_name[IFNAMSIZ-1] = '\0';

	// Index
	if( ioctl( sock, SIOCGIFINDEX, &nic ) < 0 ){
		close( sock );
		throw runtime_error( string(ifname) + ": " + string(strerror(errno)) );
	}
	data.ifindex = nic.ifr_ifindex;

	// IP Address
	if( ioctl( sock, SIOCGIFADDR, &nic ) < 0 ){
		close( sock );
		throw runtime_error( "Getting IP Address: " + string(strerror(errno)) );
	}
	memcpy( &data.ipAddr, nic.ifr_addr.sa_data + 2, IP_ADDR_LEN );

	// Hw Address
	if( ioctl( sock, SIOCGIFHWADDR, &nic ) < 0 ){
		close( sock );
		throw runtime_error( "Getting HW Address: " + string(strerror(errno)) );
	}
	memcpy( data.hwAddr, nic.ifr_netmask.sa_data, MAC_ADDR_LEN );

	// Broadcast for last host
	if( ioctl( sock, SIOCGIFBRDADDR, &nic ) < 0 ){
		close( sock );
		throw runtime_error( "Getting Broadcast: " + string(strerror(errno)) );
	}
	memcpy( &data.lastHost, nic.ifr_broadaddr.sa_data + 2, IP_ADDR_LEN );

	// First host from the network IP
	struct in_addr aux = { data.ipAddr };
	data.firstHost = inet_netof( aux );
	data.firstHost = (htonl( data.firstHost ) >> 8) + IP_ONE;

	return data;
}

/**
 * Creates a socket for ARP frames.
 *
 * @param ifindex The network interface index to bind the socket.
 * @return The socket descriptor.
 *
 * @throw runtime_error If the socket couldn't be opened (open raw sockets requires
 * root privileges).
 * @throw runtime_error  The maximum time to wait for a response couldn't be configured.
 * @throw runtime_error socket could't bind to the interface.
 */
int initSocket( int ifindex ) throw( runtime_error )
{
	int sockfd;
	struct sockaddr_ll sll;
	struct timeval timer;

	if( (sockfd = socket( AF_PACKET, SOCK_RAW, htons(ETH_P_ARP) )) < 0 )
		throw runtime_error( "socket: " + string(strerror(errno)) );

	timer.tv_sec = 0;
	timer.tv_usec = 100000;
	if( setsockopt( sockfd, SOL_SOCKET, SO_RCVTIMEO, &timer, sizeof(timer) ) < 0 )
		throw runtime_error( strerror(errno) );

	memset( &sll, 0, sizeof(sll) );
	sll.sll_family = AF_PACKET;
	sll.sll_ifindex = ifindex;
	sll.sll_protocol = htons( ETH_P_ARP );

	if( bind( sockfd, (struct sockaddr*) &sll, sizeof(sll) ) < 0 )
		throw runtime_error( strerror(errno) );

	return sockfd;
}

/**
 * Adds a permanent entry to the ARP cache of the system.
 *
 * @param ifname Name of the network interface.
 * @param ip IP Address v4 of the new arp entry.
 * @param hw An HWAddr object that contains the info about the hardware address.
 *
 * @throw runtime_error If the new entry couldn't be added.
 * */
void addARPEntry(const char *ifname, struct in_addr ip, const HWAddr &hw)
	throw( runtime_error )
{
	struct arpreq arp;
	int sock = socket( AF_INET, SOCK_DGRAM, 0 );

	arp.arp_pa.sa_family = AF_INET;
	memcpy( arp.arp_pa.sa_data + 2, &ip.s_addr, IP_ADDR_LEN );
	arp.arp_ha.sa_family = ARPHRD_ETHER;
	memcpy( arp.arp_ha.sa_data, hw.hw, MAC_ADDR_LEN );
	strncpy( arp.arp_dev, ifname, IFNAMSIZ - 1 );
	arp.arp_dev[IFNAMSIZ - 1] = '\0';
	arp.arp_flags = ATF_COM | ATF_PERM;

	if( ioctl( sock, SIOCSARP, &arp ) == -1 ){
		close( sock );
		throw runtime_error( "Add ARP entry: " + string(strerror(errno)) );
	}
	close( sock );
}

/**
 * Makes a scan for ARP entries.
 *
 * @param sfd The ARP socket to send/receive ARP frames.
 * @param ld An LocalData object that contains the local info about the
 * network interface.
 *
 * @return ARPTable that contains the ARP entries in the network.
 *
 * @note The last host is not included in the scan.
 * @see initSocket()
 */
ARPTable scan( int sfd, const LocalData &ld )
{
	struct in_addr host;
	ARPTable table;
	ARPFrame request, reply;
	int attempts;

	// Setting constant values of the request
	memset( request.eth_dst, 0xff, MAC_ADDR_LEN );
	memcpy( request.eth_src, ld.hwAddr, MAC_ADDR_LEN );
	request.eth_ethertype = htons( ETH_P_ARP );
	request.hw_type = htons( ARPHRD_ETHER );
	request.protocol = htons( ETH_P_IP );
	request.hw_len = MAC_ADDR_LEN;
	request.proto_len = IP_ADDR_LEN;
	request.opcode = htons(ARPOP_REQUEST);
	memcpy( request.hw_src, ld.hwAddr, MAC_ADDR_LEN );
	request.ip_src = ld.ipAddr;
	memset( request.hw_dst, 0, MAC_ADDR_LEN );

	// Bucle for hosts
	for( host.s_addr = ld.firstHost ; host.s_addr != ld.lastHost ; host.s_addr += IP_ONE ){
		request.ip_dst = host.s_addr;
		attempts = MAX_TRIES_FOR_RESOLV;
		cout << "Resolving " << inet_ntoa( host ) << '\r';
		cout.flush();
		write( sfd, &request, sizeof(request) ); // Send the request.
		do{
			if( read( sfd, &reply, sizeof(reply) ) > 0 ){ // Receive the reply
				// Verify the reply and sender.
				if( ntohs(reply.opcode) == ARPOP_REPLY && reply.ip_src == host.s_addr ){
					HWAddr hw( reply.hw_src );
					struct in_addr aux = { reply.ip_src };
					table[hw] = aux;
					attempts = 0;
				}
				else // Not the answer what we want.
					attempts--;
			}
			else // Some error or no response.
				attempts = 0;
		}while( attempts );
	}
	cout << endl;
	return table;
}

/**
 * An infinite bucle that analyzes new ARP replies.
 * The bucle stops setting ::active to false.
 *
 * @param sfd The ARP socket for receive ARP replies.
 * @param ifname The name of the interface network.
 * @param table The ARPTable that contains the ARP entries. 
 *
 * @throw runtime_error If a request of add ARP entry failed.
 */
void guard( int sfd, const char *ifname, const ARPTable &table )
{
	ARPFrame reply;
	set<uint32_t> ignored;
	string option;
	bool find;

	while( active ){
		// Receive the data
		if( read( sfd, &reply, sizeof(reply) ) > 0 ){
			// Verify the reply
			if( ntohs(reply.opcode) == ARPOP_REPLY ){
				HWAddr hw( reply.hw_src );
				struct in_addr ip = { reply.ip_src };

				try{
					struct in_addr reg = table.at(hw); // Check our ARP Table for the sender.

					if( reg.s_addr != ip.s_addr &&  // If the MAC doesn't match with the IP
							ignored.find(ip.s_addr) == ignored.end() ){ // ... And it's not ignored
							find = true;

							// Notice to the user
							cout << hw.toString() << " is poisoning " << inet_ntoa(ip) << 
								". Would you like to add a permanent entry to avoid the faking? (Y/N) ";
							getline( cin, option );

							if( option != "N" && option != "n" ){
								find = false;
								for( auto &i : table ){ // Look for the IP Address, if it is.
									if( i.second.s_addr == ip.s_addr ){
										try{
											addARPEntry( ifname, ip, i.first );
											cout << "Entry added" << endl;
											find = true;
										}
										catch( runtime_error &e ){
											cerr << e.what() << endl;
										}
										break;
									}
								}
							}
							if( !find ) // The IP spoofed is not in out ARP Table
								cout << "There's a missing entry. Please run the tool again for a new scan." << endl;
							ignored.insert( ip.s_addr );
						} // End if for ignoring
				}
				// The HW Address of the sender is not in our ARP Table
				catch( out_of_range ){
					cout << "There's a new device. You should try with a new scan." << endl;
				} // The received entry is not in our ARP Table
			} // End if for replies ARP
		} // End if for reading
	} // End while
}

/**
 * Kill signal handler. Change the value of ::active to stop
 * the execution of guard().
 */
void sigKill(int){
	active = false;
}

/**
 * Main function of the program.
 *
 * @param interface_name The name of the network interface to use.
 */
int main( int argc, char **argv )
{
	if( argc != 2 ){
		cerr << "Uso:\n\t" << *argv << " interface_name" << endl;
		return 1;
	}

	int sockfd;
	LocalData data;
	ARPTable arpTable;

	try{
		data = loadLocalData( argv[1] );
		sockfd = initSocket( data.ifindex );
	}
	catch( runtime_error &e ){
		cerr << e.what() << endl;
		return 1;
	}


	arpTable = scan( sockfd, data );
	
	// Output the ARP table.
	cout << arpTable.size() << " entries found. "
		"If you think there's missing devices, please run the tool again.\n\n"
		"\tHW Address\t\t\tIP Address\n";
	for( auto &i : arpTable )
		cout << '\t' << i.first.toString() << "\t\t" << inet_ntoa(i.second) << endl; 

	cout << "\nAnalyzing ARP replies. Press CTRL-C to exit\n\n";
	signal( SIGINT, sigKill );
	guard( sockfd, argv[1], arpTable );

	cout << "\rClosing socket..." << endl;
	close( sockfd );
	return 0;
}
