#!/bin/bash

# link all the dotfiles
for f in dots/*; do
  echo "creating a symlink to $f"
  read -p "> Are you sure? (y/n) " -n 1 -r
  echo
  if [[ $REPLY == "y" ]]
  then
    ln -s "$HOME/dotfiles/dots/$(basename $f)" "$HOME/.$(basename $f)"
  fi
done

#link the config folder
echo "creating the symlink to config folder"
  read -p "> Are you sure? (y/n) " -n 1 -r
  echo
  if [[ $REPLY == "y" ]]
  then
    ln -s "$HOME/dotfiles/config" "$HOME/.config"
  fi

#link the bin folder
echo "creating the symlink to bin folder"
  read -p "> Are you sure? (y/n) " -n 1 -r
  echo
  if [[ $REPLY == "y" ]]
  then
    ln -s "$HOME/dotfiles/bin" "$HOME/bin"
  fi
