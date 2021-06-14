#ifndef UTILS_H_
#define UTILS_H_

#include <string.h>
#include <stdio.h>
#include <stdint.h>
//#include <stdbool.h>

#define IPA_STR_LEN 16
#define HWA_STR_LEN 18

int parse_hwa(uint8_t * buf, char * addr);
int parse_ipa(uint8_t * buf, char * addr);
void hwa_tostr(char * buf, uint8_t * addr);
void ipa_tostr(char * buf, uint8_t * addr);
void * mymemmem(void * big, size_t big_len, const void *little, size_t little_len);

#endif
