#!/bin/bash -e

case ${OSTYPE} in
    darwin*)
        pmset -g batt |\
        awk 'NR==2{print($3, $4)}' |\
        sed -E \
        -e 's/(\;| )//g'\
        -e 's/discharging/'🔋'/g'\
        -e 's/charging/'⚡'/g'\
        -e 's/charged/'✔'/'\
        ;;
    linux*)
        if [ $(</sys/class/power_supply/BAT0/status) = 'Charging' ]
        then
            battery_charging="\UF587"
        else
            battery_charging="\UF57D"
        fi
        echo -e $(cat /sys/class/power_supply/BAT0/capacity)'%' ${battery_charging}
        ;;
esac
# if not battery, display fba3
