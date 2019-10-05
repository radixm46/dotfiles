#!/bin/sh
# display current memory in GB

mem_val=$(free -m | awk 'NR==2{printf("%.1f GB (%.1d %) \n", $3/1024, $3/$2*100)}')
mem_sign="mem: "

echo -e ${mem_sign} ${mem_val}
