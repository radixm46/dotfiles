#!/bin/sh
# display current memory in GB

mem_val=$(free -m | awk 'NR==2{printf("%.1fG (%.1f%)", $3/1024, $3/$2*100)}')

echo -e ${mem_val}
