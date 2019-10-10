#!/bin/bash -e

case ${OSTYPE} in
    darwin*)
        pmset -g batt |\
        awk 'NR==2{print($3, $4)}' |\
        sed -E \
        -e 's/(\;| )//g' \
        -e 's/charging/'⚡'/'\
        -e 's/charged/'✔'/'\
        -e 's/discharging/'🔋'/'
        ;;
    linux*)
        if [ $(</sys/class/power_supply/BAT0/status) = 'Charging' ]
        then
            battery_charging="\U26A1"
        else
            battery_charging="\U1F50B"
        fi
        echo -e $(cat /sys/class/power_supply/BAT0/capacity)'%'${battery_charging}
        ;;
esac
