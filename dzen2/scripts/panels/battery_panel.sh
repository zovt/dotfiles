#!/usr/bin/sh

source `dirname "$0"`/../colors.sh

BAT1=`acpi | head -n1 | sed "s/.* \([0-9].*\)%.*/\1/"`
BAT2=`acpi | tail -n1 | sed "s/.* \([0-9].*\)%.*/\1/"`
BAT1REMAINING=`acpi | head -n1 | sed "s/.* \([0-9].* remaining\).*/\1/"`
echo -e "  ^fg($InfoLabel)Battery 1: ^fg()$BAT1%, $BAT1REMAINING"\
    | dzen2 -fn "$Font" -p 2 -x 67 -y -100 -w 400 -h 30 -u -bg \#777766 -fg \#FFFFFF -ta l &
echo -e "  ^fg($InfoLabel)Battery 2: ^fg()$BAT2%"\
    | dzen2 -fn "$Font" -p 2 -x 67 -y -70 -w 400 -h 30 -u -bg \#777766 -fg \#FFFFFF -ta l &

