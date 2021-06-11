//#include <netpacket/packet.h>
//#define _GNU_SOURCE // HACK

/* due to dprintf function in BSD systems */
#define _WITH_DPRINTF
#include <stdio.h>

#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include <stdlib.h>

#include "packet.h"
#include "packet_tcp.h"

#include "rawsock.h"

/* 
 * write short packet info into fd
 */
void
dprintpkt_s(int fd, const struct ip_packet * p, size_t p_size)
{

	char ipa_s[IPA_STR_LEN];
	char ipa_d[IPA_STR_LEN];
	char hwa_s[HWA_STR_LEN];
	char hwa_d[HWA_STR_LEN];

	hwa_tostr(hwa_s,(uint8_t *) &p->eth_h.ether_shost);
	hwa_tostr(hwa_d,(uint8_t *) &p->eth_h.ether_dhost);
	ipa_tostr(ipa_s,(uint8_t *) &p->ip_h.saddr);
	ipa_tostr(ipa_d,(uint8_t *) &p->ip_h.daddr);

	dprintf(fd, "IP packet: %s > %s, %s > %s",hwa_s, hwa_d, ipa_s, ipa_d);

	dprintf(fd, " Proto:%hhx)", p->ip_h.protocol);

	dprintf(fd, "\n");
}

/*
 * write long packet info into fd
 */
void
dprintpkt_l(int fd, const struct ip_packet * p, size_t p_size)
{

#define w(s) write(fd, s, strlen(s))

	
	/* write short info */
	dprintpkt_s(fd, p, p_size);
	w("---\n");

	/* write long ip header info */
	dprintf(fd, "Ihl: %hhd, ver: %hhd, tos: %hhd, tot_len: %hd, \
id: %hd, frag_off: %hd, ttl: %hhd, check: %hd\n\n",
		p->ip_h.ihl,
		p->ip_h.version,
		p->ip_h.tos,
		ntohs(p->ip_h.tot_len),
		ntohs(p->ip_h.id),
		ntohs(p->ip_h.frag_off),
		p->ip_h.ttl,
		ntohs(p->ip_h.check));

	// write data in both hex and text representation
	w("Data (hex representation):\n");
	dprintbuf_f(fd, (uint8_t *) p + sizeof(struct ip_packet),
		p_size - sizeof(struct ip_packet), PBF_HEX);

	w("Data (text representation):\n");
	dprintbuf_f(fd, (uint8_t *) p + sizeof(struct ip_packet),
		p_size - sizeof(struct ip_packet), PBF_CHAR);

	w("===============================\n\n");

}

/*
 * print buf into file referenced by fd
 * format is print format
 */
void
dprintbuf_f(int fd, const uint8_t * buf, size_t numbytes, pb_format_t format)
{
	int i = 0;
	int w = 16;

	if(format == PBF_CHAR)
		w = 32;

    for(; i < numbytes; i++){
    	if(i%w == 0){
    		dprintf(fd, "%.3d| ",i);
    	}

    	if(format == PBF_CHAR){
	    	if(isprint(buf[i]))
	    		dprintf(fd, "%c", buf[i]);
	    	else
	    		dprintf(fd, ".");
	    } else if(format == PBF_HEX){
	    	dprintf(fd, "%.2x ", buf[i]);
	    } else {
	    	perror("invalid pb format");
	    }

    	if(i%w == w-1)
    		dprintf(fd,"\n");
    }
    dprintf(fd,"\n\n");
}

/* print buffer into fd with hex format (PBF_HEX) */
void
dprintbuf(int fd, const uint8_t * buf, size_t numbytes)
{
	dprintbuf_f(fd, buf, numbytes, PBF_HEX);
}

/* print buffer to stdout */
void
printbuf(const uint8_t * buf, size_t numbytes)
{
	dprintbuf(1, buf, numbytes);
}

/*
 * substitute data in ip packet with substitution pairs
 * (subs, subs_c). If something substituted, set the new checksum.
 * Returns number of substitutions */
int
injectpkt(struct ip_packet * p, size_t numbytes, char ** subs, size_t subs_c)
{
	size_t i = 0;
	int occur = 0;
	uint8_t * pos;

	uint8_t * buf = (uint8_t *) p + sizeof(struct ip_packet);
	size_t buf_len = numbytes - sizeof(struct ip_packet);

	for(; (int)i <(int)subs_c-1; i+=2){
		//printf("Bytes: %lu\n",numbytes - sizeof(struct ip_packet));
		while((pos = mymemmem(buf, buf_len, subs[i], strlen(subs[i]))) != NULL){
		
			if(strlen(subs[i]) == strlen(subs[i+1])){
				occur++;
				memcpy(pos, subs[i+1], strlen(subs[i+1]));

				printf("IP packet injected ('%s' > '%s').\n", subs[i], subs[i+1]);
				//dprintbuf_f(1, pos -2, strlen(subs[i+1])+5, PBF_CHAR);
			} else {
				dprintf(2, "Error: Lengths of strings '%s' and '%s' differ.", subs[i], subs[i+1]);
			}
		}
	}

	// compute correct checksum
	if(occur > 0 && is_tcppkt(p, numbytes)){
		modify_tcp_checksum((struct tcp_packet *) p, numbytes);
		printf("TCP checksum modified.\n");
	}

	return occur;
}
