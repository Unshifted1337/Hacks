#include <stdio.h>
//#include <stdbool.h>
#include <string.h>

#include "utils.h"

// parse hardware adress
int
parse_hwa(uint8_t * buf, char * str)
{
	int n = sscanf(str, "%hhx:%hhx:%hhx:%hhx:%hhx:%hhx",
		&buf[0], &buf[1], &buf[2], &buf[3],&buf[4],&buf[5]);

	return n == 6;
}

// parse ip adress
int
parse_ipa(uint8_t * buf, char * str)
{
	int n = sscanf(str, "%hhu.%hhu.%hhu.%hhu",
		&buf[0], &buf[1], &buf[2], &buf[3]);

	return n == 4;
}

// save string representation of hw addr to buf
void hwa_tostr(char * buf, uint8_t * addr)
{
	snprintf(buf, HWA_STR_LEN, "%x:%x:%x:%x:%x:%x",
					addr[0], addr[1], addr[2],
					addr[3], addr[4], addr[5]);
}

// save string representation of ip addr to buf
void ipa_tostr(char * buf, uint8_t * addr)
{
	snprintf(buf, IPA_STR_LEN, "%d.%d.%d.%d",
					addr[0], addr[1], addr[2], addr[3]);
}

void *
mymemmem(void * big, size_t big_len, const void *little, size_t little_len)
{
	void * p;
	int i;

	for(p = big; p+little_len < big+big_len; p++){
		for(i=0; i<little_len; i++){
			if(*((uint8_t *) p+i) != *((uint8_t *) little+i))
				break;
		}

		if(i == little_len){
			return p;
		}
	}

	return NULL;
}
