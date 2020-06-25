#!/usr/bin/env bash

case $(uname) in
    Darwin)
        pmset -g batt |\
        awk 'NR==2{print($3, $4)}' |\
        sed -E \
        -e 's/(\;| )//g'\
        -e 's/discharging/'\ $(printf '\uF57C')'/g'\
        -e 's/charging/'\ $(printf '\uF587')'/g'\
        -e 's/charged/'\ $(printf '\uF583')'/'\
        ;;
    Linux*)
        # check if battery is available
        if [ -d "/sys/class/power_supply/BAT0" ]
        then
            if [ $(</sys/class/power_supply/BAT0/status) = 'Charging' ]
            then
                battery_stat="\UF587"
            else
                battery_stat="\UF57D"
            fi
            echo -e $(cat /sys/class/power_supply/BAT0/capacity)'%' ${battery_stat}
        else
            printf "\UF590"\ "\UFBA3"
        fi

        ;;
    *)
        printf "\UF00D"
esac
# if not battery, display fba3
