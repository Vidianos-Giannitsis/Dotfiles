#!/bin/sh
setxkbmap -model pc104 -layout us,gr -variant ,, -option grp:alt_shift_toggle &
picom --config ~/.config/picom.conf &
lxpolkit &

exec dbus-launch --exit-with-session emacs -mm --debug-init -l ~/.emacs.d/Desktop.el
