#ifndef PACKET_TCP_H_
#define PACKET_TCP_H_

//#include <netpacket/packet.h>
#include <arpa/inet.h>

#include <net/if.h>
#include <netinet/ip.h>
#include <netinet/if_ether.h>
#include <netinet/tcp.h>

#include "utils.h"
#include "packet.h"

struct tcp_packet
{
	struct ether_header eth_h;
	struct iphdr ip_h;
	struct tcphdr tcp_h;
} __attribute__((__packed__));

struct tcp_pseudo_header
{
    uint32_t ip_src;
    uint32_t ip_dst;
    uint8_t zero;
    uint8_t protocol; // 6 for tcp
    uint16_t tcp_len; // tcp header and data len
    struct tcphdr tcph;
} __attribute__((__packed__));

int is_tcppkt(struct ip_packet * p, size_t numbytes);
void modify_tcp_checksum(struct tcp_packet * p, size_t numbytes);

#endif
