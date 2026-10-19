#!/bin/bash

set -xe
start="# custom "
end="# end custom"
content=$(sudo -u flocks gpg2 -q --for-your-eyes-only -d ./host.base.gpg)

if grep -q -e "$start" /etc/hosts;
then
	sudo sed -i "/^${start}$/,/^${end}$/d" /etc/hosts
fi
echo -e "$start\n$content\n$end\n" | sudo tee /etc/hosts /etc/hosts
   
