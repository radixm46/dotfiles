#!/usr/bin/env bash -e

case $(uname) in
    Darwin)
        pmset -g batt |\
        awk 'NR==2{print($3, $4)}' |\
        sed -E \
        -e 's/(\;| )//g'\
        -e 's/discharging/'$(printf '\uF57C')'/g'\
        -e 's/charging/'$(printf '\uF587')'/g'\
        -e 's/charged/'$(printf '\uF583')'/'\
        ;;
    Linux*)
        # check if battery is available
        if [ $(</sys/class/power_supply/BAT0/status) = 'Charging' ]
        then
            battery_charging="\UF587"
        else
            battery_charging="\UF57D"
        fi
        echo -e $(cat /sys/class/power_supply/BAT0/capacity)'%' ${battery_charging}
        ;;
    *)
        printf "unavailable"
esac
# if not battery, display fba3
