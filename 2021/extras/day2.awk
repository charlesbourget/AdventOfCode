#! /usr/bin/awk -f
{split($0,a," ");if(a[1]=="forward"){x+=a[2];z2+=z*a[2]}else if(a[1]=="up"){z-=a[2]}else{z+=a[2]}}END{print"Part 1: "x*z"\nPart 2: "x*z2}