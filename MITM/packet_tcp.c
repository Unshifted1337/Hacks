//#include <netpacket/packet.h>
#include <arpa/inet.h>

#include <net/if.h>
#include <netinet/ip.h>
//#include <netinet/ether.h>
#include <netinet/tcp.h>

#include "utils.h"
#include "packet.h"
#include "packet_tcp.h"

int
is_tcppkt(struct ip_packet * p, size_t numbytes)
{
	return p->ip_h.protocol == 6;
}

uint16_t checksum(const uint8_t *buf, size_t size)
{
	uint32_t sum = 0;
	uint32_t i;

	/* Sum buf parts */
	for (i = 0; i < size - 1; i += 2){
		uint16_t word16 = * (uint16_t *) &buf[i];
		sum += word16;
	}

	/* add last if odd number of octets in buf */
	if (size & 1){
		uint16_t word16 = (uint8_t) buf[i];
		sum += word16;
	}

	/* ones complement */
	while (sum >> 16)
		sum = (sum & 0xFFFF) + (sum >> 16);

	/* invert and return */
	return ~ (uint16_t) sum;
}

void
modify_tcp_checksum(struct tcp_packet * p, size_t numbytes)
{
	// create buffer for pseudoheader
	size_t buf_len = numbytes - sizeof(struct ip_packet) +
		sizeof(struct tcp_pseudo_header) - sizeof(struct tcphdr);
	uint8_t buf[buf_len];
	memset(buf, 0, buf_len);

	// create pseudo header
	struct tcp_pseudo_header * ph = (struct tcp_pseudo_header *) buf;

	ph->ip_src = p->ip_h.saddr;
	ph->ip_dst = p->ip_h.daddr;
	ph->zero = 0;
	ph->protocol = 6;
	ph->tcp_len = htons(buf_len - sizeof(struct tcp_pseudo_header) +
		sizeof(struct tcphdr));

	memcpy(&ph->tcph, &p->tcp_h, buf_len - sizeof(struct tcp_pseudo_header) +
		sizeof(struct tcphdr));

	ph->tcph.th_sum = 0;


	// set correct checksum in tcp packet 
	p->tcp_h.th_sum = checksum(buf, buf_len);

}
