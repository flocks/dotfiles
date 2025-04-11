#!/bin/bash
# shell script to prepend i3status with more stuff

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

i3status -c ~/.config/i3/i3status.conf | while :
do
	read line
	nbGithub=$(notmuch search tag:github and tag:unread | wc -l)
	nbUnreadList=$(notmuch search tag:list and tag:unread | wc -l)
	nbUnread=$(notmuch search tag:inbox and tag:unread | wc -l)

	# unreadList=$([ "$nbUnreadList" -eq 0 ] && echo "" || echo "👥 $nbUnreadList |")
	unread=$([ "$nbUnread" -eq 0 ] && echo "" || echo "📨 $nbUnread |")
	# github=$([ "$nbGithub" -eq 0 ] && echo "" || echo "💻 $nbGithub |")

	echo "$unreadList $unread $github $line" || exit 1
done
