#! /usr/bin/awk -f
{split($1,val,"");for(i=1;i<=length(val);i++){acc[i]+=val[i];}}END{for(i=1;i<length(acc);i++){pow=length(acc)-i-1;if(int(acc[i])<int(NR)/2){eps+=2^pow;}else{gamma+=2^pow;}}print"Part 1: "gamma*eps;}
