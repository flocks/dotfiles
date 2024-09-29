#!/usr/bin/env bash

# mapping of favorite folders
declare -A short_names
short_names["front"]="/home/fteissier/ledger/ledger-vault-front"
short_names["vjs"]="/home/fteissier/ledger/vault-ts"
short_names["gate"]="/home/fteissier/ledger/ledger-vault-api"
short_names["dl"]="/home/fteissier/Downloads"
short_names["screenshots"]="/home/fteissier/screenshots"
short_names["home"]="/home/fteissier"
short_names["dot"]="/home/fteissier/dotfiles"
short_names["revault"]="/home/fteissier/ledger/revault"
short_names["minivault"]="/home/fteissier/ledger/minivault"
short_names["live"]="/home/fteissier/ledger/ledger-live"
short_names["apdu"]="/home/fteissier/ledger/vault-apdu-connector"
short_names["bake"]="/home/fteissier/ledger/vault-ts/apps/cli/src/manifests"

# Collect folders, excluding those that have short names
# folders=$(find ~/ledger -mindepth 1 -maxdepth 1 -type d)
folders=""

# Remove folders that have short names from the find output
for short_name_path in "${short_names[@]}"; do
    folders=$(echo "$folders" | grep -v "^$short_name_path$")
done

# Append short names to the list of folders
folder_list=""
for folder in $folders; do
    folder_list+="$folder"$'\n'
done

# Add the short names to the list
for name in "${!short_names[@]}"; do
    folder_list+="$name"$'\n'
done

sessions=$(tmux list-sessions | awk -F: '{print $1}')
choices=$(echo -e "$sessions\n$folder_list" | sort -u)


# Select folder
if [[ $# -eq 1 ]]; then
    selected=$1
else
    selected=$(echo "$choices" | fzf)
fi

if [[ -z $selected ]]; then
    exit 0
fi

# Map short names back to full paths if a short name was selected
if [[ -n ${short_names[$selected]} ]]; then
    selected=${short_names[$selected]}
fi

# Get the name for the tmux session (use short name if available)
selected_name=$(basename "$selected" | tr . _)
for name in "${!short_names[@]}"; do
    if [[ ${short_names[$name]} == "$selected" ]]; then
        selected_name=$name
        break
    fi
done


tmux_running=$(pgrep tmux)

# Start a new session or attach to it
if [[ -z $TMUX ]] && [[ -z $tmux_running ]]; then
    tmux new-session -s $selected_name -c "$selected"
    exit 0
fi

if ! tmux has-session -t=$selected_name 2> /dev/null; then
    tmux new-session -ds $selected_name -c "$selected"
fi

tmux switch-client -t $selected_name

