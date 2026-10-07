#!/bin/bash

set -xe
start="---custom---"
end="---end---"
content=$(sudo -u flocks gpg2 -q --for-your-eyes-only -d ./host.base.gpg)

if  grep -q -e "$start" /etc/hosts;
then
	sed -i "/^${start}$/,/^${end}$/d" /etc/hosts
fi
echo -e "$start\n$content\n$end\n" >> /etc/hosts
   
