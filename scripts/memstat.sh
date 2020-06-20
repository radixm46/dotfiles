#!/bin/bash -e
# display current memory in GB
nerdmem="\UF85A"
case ${OSTYPE} in
    darwin*)
        mem_val='is \UF179'
        ;;
    linux*)
        mem_val=$(free -m | awk 'NR==2{printf("%.1fG (%.1f%)", $3/1024, $3/$2*100)}')
        ;;
    *)
        mem_val='unsupported'
        ;;
esac


echo -e ${nerdmem} ${mem_val}
