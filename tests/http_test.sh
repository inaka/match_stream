#!/bin/sh
ab -n$1 -c$2 http://$3:$4/1/checkin
ab -n$1 -c$2 http://$3:$4/1/matches
ab -n$1 -c$2 http://$3:$4/1/match/elp-tig-2011-09-10/history