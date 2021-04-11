#!/bin/bash
setxkbmap -model pc104 -layout us,gr -variant ,, -option grp:alt_shift_toggle &
nitrogen --set-scaled /usr/share/backgrounds/Xplo_by_Hugo_Cliff.png &
nm-applet &
volumeicon &
picom --config ~/.config/picom.conf &
