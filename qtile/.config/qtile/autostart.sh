#!/bin/bash
setxkbmap -model pc104 -layout us,gr -variant ,, -option grp:alt_shift_toggle &
volumeicon &
nm-applet &
feh --bg-fill /usr/share/backgrounds/arcolinux/time-abstract.jpg &
picom --config ~/.config/picom.conf &
lxpolkit &
emacs --daemon &
blueberry-tray &
dunst &
xfce4-power-manager &
gammy &
