#!/bin/bash
# Quickly updates origin

msg="Quick meaningless update"
if ! [ -z "$1" ]; then
    msg=$1
fi

echo -e "\e[35mAdding\e[0m"
git add .
echo -e "\e[35mStatus:\e[0m"
git status
echo -e "\e[35mCommiting:\e[0m"
git commit -m "$msg"
echo -e "\e[35mPushing:\e[0m"
git push origin main
echo -e "\e[34mDone\e[0m"