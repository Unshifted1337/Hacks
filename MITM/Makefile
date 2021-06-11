OBJ = utils.o arp.o packet.o mutils.o packet_tcp.o rawsock.o
DEPS = utils.h arp.h packet.h mutils.h packet_tcp.h rawsock.h

CFLAGS += -Wall -g

all: arpspoof sniffer

arpspoof: arpspoof.o $(OBJ)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ ${@}.o $(OBJ)

sniffer: sniffer.o $(OBJ)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ ${@}.o $(OBJ)

%.o: %.c $(DEPS)
	$(CC) $(CFLAGS) -o $@ -c $<

clean:
	-rm -f arpspoof sniffer ./*.o
