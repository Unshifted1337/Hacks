#ifndef ARP_H_
#define ARP_H_

#include "packet.h"
#include "utils.h"

struct arp_packet {
	struct eth_packet eth;
	uint16_t hwtype;
	uint16_t prtype;
	uint8_t hwsize;
	uint8_t prsize;
	uint16_t opcode;
	uint8_t sender_hwa[6];
	uint8_t sender_ipa[4];
	uint8_t target_hwa[6];
	uint8_t target_ipa[4];
};

struct arp_packet create_arp_packet(
	uint8_t * target_ipa,
	uint8_t * target_hwa,
	uint8_t * host_ipa,
	uint8_t * host_hwa);

#endif
