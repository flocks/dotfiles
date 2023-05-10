#!/bin/bash

function locate_dominating_file() {
  local file="$1"
  local target="$2"
  local dir="$(dirname "$file")"

  if [[ -f "$dir/$target" || -d "$dir/$target" ]]; then
    echo "$dir"
    return 0
  fi
  while [[ "$dir" != "/" ]]; do
    dir="$(dirname "$dir")"
    if [[ -f "$dir/$target" || -d "$dir/$target" ]]; then
      echo "$dir"
      return 0
    fi
  done
  return 1
}

function list_packages() {
  local dir="$1"
  for subdir in "$dir"/**/*; do
    if [[ -d "$subdir" && -f "$subdir/package.json" && ! "$subdir" =~ node_modules ]]; then
      echo "$subdir"
    fi
    if [[ -d "$subdir" && ! "$subdir" =~ node_modules ]]; then
      list_packages "$subdir"
    fi
  done
}

function main() {
	current=$(pwd)
	root=$(locate_dominating_file $(pwd) ".git")
	local choice
	if [[ "$root" == "$current" || "$root" == "" ]]; then
		choice=$(list_packages "$current" | fzf)
	else
		choice=$(locate_dominating_file $(pwd) "package.json")
	fi
	cd "$choice"
}

main
