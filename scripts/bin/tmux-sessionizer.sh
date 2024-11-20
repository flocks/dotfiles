#!/usr/bin/env bash

sessions=$(tmux list-sessions | awk -F: '{print $1}')
num_sessions="1\n2\n3\n4\n"
choices=$(echo -e "$sessions\n$num_sessions" | sort -u)


if [[ $# -eq 1 ]]; then
    selected=$1
else
    selected=$(echo "$choices" | fzf)
fi

if [[ -z $selected ]]; then
    exit 0
fi
# Get the name for the tmux session (use short name if available)
selected_name=$(basename "$selected" | tr . _)

tmux_running=$(pgrep tmux)

# Start a new session or attach to it
if [[ -z $TMUX ]] && [[ -z $tmux_running ]]; then
    tmux new-session -s $selected_name
    exit 0
fi

if ! tmux has-session -t=$selected_name 2> /dev/null; then
    tmux new-session -ds $selected_name -c "$selected"
fi

tmux switch-client -t $selected_name

