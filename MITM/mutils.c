#include <string.h>
//#include <netinet/ether.h>

#include "packet.h"
#include "utils.h"

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
//struct sockaddr_ll sa;

/*
 * This method should be called only if there is
 * sufficient number of args. It saves the args into proper
 * global variables and convert them to binary repre-
 * sentation.
 */
void
process_args(int argc, char ** argv)
{

  // save params
  if_name = argv[1];
  ipa1_str = argv[2];
  hwa1_str = argv[3];
  ipa2_str = argv[4];
  hwa2_str = argv[5];

  // parse target and host addresses
  parse_ipa(ipa1, ipa1_str);
  parse_hwa(hwa1, hwa1_str);
  parse_ipa(ipa2, ipa2_str);
  parse_hwa(hwa2, hwa2_str);

  // get local interface mac address
  gethwaddr(hwa_host, if_name);

  // get sockaddr
  //sa = getsockaddr(if_name);

}
