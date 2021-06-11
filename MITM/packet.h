#ifndef PACKET_H_
#define PACKET_H_

//#include <netpacket/packet.h>
#include "lib/packet.h"

#include "rawsock.h"

#include <arpa/inet.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <netinet/ip.h>
#include <netinet/if_ether.h>
#include "lib/iphdr.h"

#include "utils.h"

// due to BSD compatibility
#ifndef ETH_P_IP
#define ETH_P_IP  0x0800
#endif

typedef enum  {PD_1TO2, PD_2TO1, PD_OTHER} pkt_dir_t; // who is sending the packet

typedef enum {PBF_HEX, PBF_CHAR} pb_format_t;

struct eth_packet
{
	uint8_t dest[ETHER_ADDR_LEN];
	uint8_t source[ETHER_ADDR_LEN];
	uint16_t type;
};

struct ip_packet
{
	struct ether_header eth_h;
	struct iphdr ip_h;
} __attribute__((__packed__));

#include "packet_tcp.h"

void dprintpkt_s(int fd, const struct ip_packet * p, size_t p_size);
void dprintpkt_l(int fd, const struct ip_packet * p, size_t p_size);
void dprintbuf_f(int fd, const uint8_t * buf, size_t numbytes, pb_format_t format);
void dprintbuf(int fd, const uint8_t * buf, size_t numbytes);
void printbuf(const uint8_t * buf, size_t numbytes);
int injectpkt(struct ip_packet * p, size_t numbytes, char ** subs, size_t subs_c);

#endif
