#!/bin/bash

code_extensions=".html .css .js .ts .jsx .tsx .go .sql .sh .el"

find_command="find . -type f \( "
for ext in $code_extensions; do
    find_command+=" -name '*$ext' -o"
done
find_command="${find_command% -o}" # Remove the trailing -o
find_command+=" \)"

eval $find_command
