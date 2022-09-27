xinput --set-prop "TPPS/2 IBM TrackPoint" "libinput Accel Speed" 1
xmodmap ~/.xmodmap.conf
xset r rate 150 200
setxkbmap -layout us -variant mac
keyboard_plug &
sxhkd &
setbg &
mpd &
dunst &
emacs --daemon
compton &
exec i3
