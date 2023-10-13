#!/usr/bin/env bash
set -euo pipefail

pwr_conn='\UF06A5'
pwr_discn='\UF06A6'
# print charging battery sign from nerd fonts
# (arg1: int(battery capacity), arg2: bool(charging status))
function pr_batt_cap() {
    case "$2" in
        'true')
            local batt_signs=(
                '\UF089C' '\UF0086' '\UF0087' '\UF0088' '\UF089D'
                '\UF0089' '\UF089E' '\UF008A' '\UF008B' '\UF0085'
            ) ;;
        'false')
            local batt_signs=(
                '\UF007A' '\UF007B' '\UF007C' '\UF007D' '\UF007E'
                '\UF007F' '\UF0080' '\UF0081' '\UF0082' '\UF0079'
            ) ;;
    esac
    local batt_idx=$(($1 / 10))

    if ((0 <= batt_idx)) && ((batt_idx <= 10)); then
        printf "${batt_signs[batt_idx]}"
    else
        printf '\UF0083'
    fi
}

# printf battery glyph from nerd fonts
case $(uname) in
    Darwin)
        function print_mac_batt_stat() {
            case "$2" in
                'AC')
                    printf "${pwr_conn} $(pr_batt_cap $1 false) $1%%"
                        ;;
                'charging')
                    printf "${pwr_conn} $(pr_batt_cap $1 true) $1%%"
                        ;;
                'discharging')
                    printf "${pwr_discn} $(pr_batt_cap $1 false) $1%%"
                        ;;
                'charged')
                    printf "${pwr_conn} $(pr_batt_cap $1 false) $1%%"
                        ;;
                *)
                    printf "${pwr_conn} $(pr_batt_cap $1 false) $1%%"
                        ;;
            esac
        }
        print_mac_batt_stat $(pmset -g batt \
                              | awk 'NR==2{print($3 $4)}' \
                              | sed -e 's/;//g' -e 's/%/\ /')
        ;;
    Linux*)
        function print_batt_stat () {
            case "$2" in
                'Charging')
                    printf "${pwr_conn} $(pr_batt_cap $1 true) $1%%"
                    ;;
                'Discharging')
                    printf "${pwr_discn} $(pr_batt_cap $1 false) $1%%"
                    ;;
                'Full')
                    printf "${pwr_conn} $(pr_batt_cap $1 false) $1%%"
                    ;;
                'Not charging')
                    printf "${pwr_conn} $(pr_batt_cap $1 false) $1%%"
                    ;;
                'Unknown')
                    printf "${pwr_conn} $(pr_batt_cap $1 false) $1%%"
                    ;;
            esac
        }
        # check if battery is available
        function list_batt_stat () {
            for bpath in '/sys/class/power_supply/'BAT*; do
                [[ -L ${bpath} && -d ${bpath} ]] && \
                    print_batt_stat "$(<${bpath}/capacity)" "$(<${bpath}/status)";
            done
        }
        list_batt_stat || printf ${pwr_conn}\ '\UF0091'
        ;;
    *)
        printf '\UF00D'
esac
