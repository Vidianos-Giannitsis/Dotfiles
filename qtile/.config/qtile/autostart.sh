#!/bin/bash
setxkbmap -model pc104 -layout us,gr -variant ,, -option grp:alt_shift_toggle &
nitrogen --set-scaled ~/Pictures/aurora.jpg &
nm-applet &
picom --config ~/.config/picom.conf &
