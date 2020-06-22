#!/usr/bin/env bash -e
# display current memory in GB
nerdmem="\UF85A"
case $(uname) in
    Darwin)
        mem_val="is \UF179"
        ;;
    Linux*)
        mem_val=$(free -m | awk 'NR==2{printf("%.1fG (%.1f%)", $3/1024, $3/$2*100)}')
        ;;
    *)
        mem_val='unsupported'
        ;;
esac


printf "${nerdmem}"\ "${mem_val}"
