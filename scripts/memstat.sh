#!/bin/sh
# display current memory in GB
case ${OSTYPE} in
    darwin*)
        mem_val='unsupported'
        ;;
    linux*)
        mem_val=$(free -m | awk 'NR==2{printf("%.1fG (%.1f%)", $3/1024, $3/$2*100)}')
        ;;
esac


echo ${mem_val}
