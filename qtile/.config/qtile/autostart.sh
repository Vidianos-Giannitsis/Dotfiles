#!/bin/bash
setxkbmap -model pc104 -layout us,gr -variant ,, -option grp:alt_shift_toggle &
nitrogen --set-scaled ~/Pictures/Sol_manjaro.png &
nm-applet &
volumeicon &
picom --config ~/.config/picom.conf &
