#!/bin/bash -e

battery_capacity=$(cat /sys/class/power_supply/BAT0/capacity)
if [ $(</sys/class/power_supply/BAT0/status) = 'Charging' ]
then
    battery_charging="\U26A1"
    #battery_charging="\U1f50C"
else
    battery_charging="\U1F50B"
fi

echo -e ${battery_capacity}'%'${battery_charging}
