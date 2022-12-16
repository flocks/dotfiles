#!/bin/bash


fzf -m --bind alt-a:select-all,ctrl-d:deselect-all,ctrl-t:toggle-all | xargs nvim

