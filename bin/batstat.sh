#!/usr/bin/env bash

# print charging battery sign from nerd fonts
function print_chrg_batt {
    # nerd fonts charging bat stat array (20, 30, 40, 60, 80, 90, 100)
    charging_bat_arr=('\UF585' '\UF586' '\UF587' '\UF588' '\UF589' '\UF58A' \
                               '\UF584' '\UF578' '\UF582')
    if ((0 < "$1")) && (("$1" <= 20)); then bat_idx=0
    elif ((20 < "$1")) && (("$1" <= 30)); then bat_idx=1
    elif ((30 < "$1")) && (("$1" <= 40)); then bat_idx=2
    elif ((40 < "$1")) && (("$1" <= 60)); then bat_idx=3
    elif ((60 < "$1")) && (("$1" <= 80)); then bat_idx=4
    elif ((80 < "$1")) && (("$1" <= 90)); then bat_idx=5
    elif ((90 < "$1")) && (("$1" < 100)); then bat_idx=6
    elif ((100 == "$1")); then bat_idx=7
    else bat_idx=8
    fi
    printf ${charging_bat_arr[bat_idx]}
}

# print non charging battery sign from nerd fonts
function print_non_chrg_batt {
    # nerd fonts non charging bat stat array (10 to 100)
    non_charging_bat_arr=('\UF579' '\UF57A' '\UF57B' '\UF57C' '\UF57D' '\UF57E' \
                                   '\UF57F' '\UF580' '\UF581' '\UF578' '\UF582')
    if ((0 < "$1")) && (("$1" <= 10)); then bat_idx=0;
    elif ((10 < "$1")) && (("$1" <= 20)); then bat_idx=1
    elif ((20 < "$1")) && (("$1" <= 30)); then bat_idx=2
    elif ((30 < "$1")) && (("$1" <= 40)); then bat_idx=3
    elif ((50 < "$1")) && (("$1" <= 60)); then bat_idx=4
    elif ((60 < "$1")) && (("$1" <= 70)); then bat_idx=5
    elif ((70 < "$1")) && (("$1" <= 80)); then bat_idx=6
    elif ((80 < "$1")) && (("$1" <= 90)); then bat_idx=7
    elif ((90 < "$1")) && (("$1" < 100)); then bat_idx=8
    elif ((100 == "$1")); then bat_idx=9
    else bat_idx=10
    fi
    printf ${non_charging_bat_arr[bat_idx]}
}

pwr_conn='\UFBA3'
pwr_discn='\UFBA4'

# printf battery glyph from nerd fonts

case $(uname) in
    Darwin)
        function print_batt_stat() {
            case "$1" in
                'charging')
                    printf "${pwr_conn} $(print_chrg_batt $2) $2%%"
                        ;;
                'discharging')
                    printf "${pwr_discn} $(print_non_chrg_batt $2) $2%%"
                        ;;
                'charged')
                    printf "${pwr_discn} $(print_non_chrg_batt $2) $2%%"
                        ;;
                *)
                    printf "$(print_non_chrg_batt $2) $2%%"
                        ;;
            esac
        }
        print_batt_stat $(pmset -g batt | \
                              awk 'NR==2{print($4 $3)}' | \
                              sed -e 's/;/\ /g' -e 's/%//')
        ;;
    Linux*)
        # check if battery is available
        batpath="/sys/class/power_supply/BAT0"
        if [[ -d ${batpath} ]]; then
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
            printf "\UFBA3"\ "\UF590"
        fi

        ;;
    *)
        printf "\UF00D"
esac
