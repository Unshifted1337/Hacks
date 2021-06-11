/*
 * mutils.h
 *
 * Intended only for use in files with main() method.
 * Contains methods and global variables that need to be used in
 * both sniffer.c and arpspoof.c
 */

#ifndef MUTILS_H_
#define MUTILS_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
//#include <netinet/ether.h>

#include "packet.h"

/* text representation of addresses */
char * if_name;
char * ipa1_str;
char * hwa1_str;
char * ipa2_str;
char * hwa2_str;

/* addresses binary representation */
uint8_t ipa1[4];
uint8_t hwa1[ETHER_ADDR_LEN];
uint8_t ipa2[4];
uint8_t hwa2[ETHER_ADDR_LEN];
uint8_t hwa_host[ETHER_ADDR_LEN];

/* address for sendto */
struct sockaddr_ll sa;

void process_args(int argc, char ** argv);

#endif
