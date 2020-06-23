#!/bin/sh

uptime_formatted=$(uptime | cut -d ',' -f1 | cut -d ' ' -f4,5)

#batt_stat=$($HOME/dotfiles/scripts/batstat.sh)

##date_formatted=$(date +'%Y-%m-%d %H:%M')
date_formatted=$(date +'%H:%M')

memory_usage=$($HOME/dotfiles/scripts/memstat.sh)

stat_separator="\U23B8"


echo -e \
'M: '${memory_usage} ${stat_separator} \
'Up:' ${uptime_formatted} ${stat_separator} \
${batt_stat} ${stat_separator} \
${date_formatted}
# TODO:
# use color
# write fnction
# cpu freq
# fan, temp
# vol, brightness

# network icon "\U1f5A7"
