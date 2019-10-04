#!/bin/bash -e

battery_capacity=$(cat /sys/class/power_supply/BAT0/capacity)
# battery_charging=$(cat /sys/class/power_supply/BAT0/status)
if [ $(</sys/class/power_supply/BAT0/status) = 'Charging' ]
then
    battery_charging="\U26A1" #"\U1f50C"
else
    battery_charging="\U1F50B"
fi

echo -e ${battery_charging} ${battery_capacity} '%'
# TODO: support macOS
