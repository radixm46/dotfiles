#!/usr/bin/env bash
set -euo pipefail

pwr_conn='\UFBA3'
pwr_discn='\UFBA4'
# print charging battery sign from nerd fonts
# (arg1: int(battery capacity), arg2: bool(charging status))
function pr_batt_cap() {
    case "$2" in
        'true')
        # nerd fonts charging batt signs (20, 20, 20, 30, 40, 40, 60, 80, 90, 100)
            local batt_signs=(
                '\UF585' '\UF585' '\UF585'
                '\UF586'
                '\UF587' '\UF587'
                '\UF588' '\UF588'
                '\UF589' '\UF58A'
                '\UF584')
                ;;
        'false')
        # nerd fonts non charging batt signs (00 to 100)
            local batt_signs=(
                '\UF58D' '\UF579' '\UF57A' '\UF57B' '\UF57C'
                '\UF57D' '\UF57E' '\UF57F' '\UF580' '\UF581' '\UF578')
                ;;
    esac
    local batt_idx=$(($1 / 10))

    if ((0 <= batt_idx)) && ((batt_idx <= 10)); then
        printf "${batt_signs[batt_idx]}"
    else
        printf '\UF582'
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
        list_batt_stat || printf ${pwr_conn}\ '\UF590'
        ;;
    *)
        printf '\UF00D'
esac
