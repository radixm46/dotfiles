#!/usr/bin/env bash

case $(uname) in
    Darwin)
        pmset -g batt |\
        awk 'NR==2{print($4, $3)}' |\
        sed -E \
        -e 's/(\;| )//g'\
        -e 's/discharging/'$(printf '\uF57C')\ '/g'\
        -e 's/charging/'$(printf '\uF587')\ '/g'\
        -e 's/charged/'$(printf '\uF583')\ '/g'\
        -e 's/AC/'$(printf '\uFBA3')\ '/g'\
        ;;
    Linux*)
        # check if battery is available
        batpath="/sys/class/power_supply/BAT0"
        if [ -d ${batpath} ]
        then
            case $(<${batpath}/status) in
                Charging)
                    battery_stat="\UF587"
                ;;
                Discharging)
                    battery_stat="\UF57D"
                    ;;
                Full)
                    battery_stat="\UF578"
                    ;;
                "Not charging")
                    battery_stat="\UF57D"
                    ;;
                *)
                    battery_stat="\UF590"
                    ;;
            esac

            printf ${battery_stat}\ $(< ${batpath}/capacity)'%%'
        else
            printf "\UF590"\ "\UFBA3"
        fi

        ;;
    *)
        printf "\UF00D"
esac
# if not battery, display fba3
