#include "arp.h"

/* create a binary representation of
 * arp reply packet */
struct arp_packet
create_arp_packet(uint8_t * target_ipa, uint8_t * target_hwa, uint8_t * host_ipa, uint8_t * host_hwa)
{
	struct arp_packet p;

	memcpy(p.eth.dest, target_hwa, 6); // set packet dest addr
	memcpy(p.eth.source, host_hwa, 6); // set packet source addr
	p.eth.type = htons(0x0806);

	p.hwtype = htons(1);
	p.prtype = htons(0x0800);
	p.hwsize = 6;
	p.prsize = 4;
	p.opcode = htons(2); // arp reply

	memcpy(p.sender_hwa, host_hwa, 6);
	memcpy(p.sender_ipa, host_ipa, 4);
	memcpy(p.target_hwa, target_hwa, 6);
	memcpy(p.target_ipa, target_ipa, 4);

	return p;
}
