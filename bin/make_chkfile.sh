#!/bin/sh
case "$(uname)" in
    Darwin) fcopt='bh' ;;
    Linux*) fcopt='b' ;;
    *) fcopt='b' ;;
esac

printf '> Checking... '${1}'\n';
if [ ! -e ${1}  ]; then
    printf '==> NOT EXIST\n';
elif file -${fcopt} ${1} | grep -q 'symbolic link'; then
    printf "==> $(file -${fcopt} ${1})\n";
elif file -${fcopt} ${1} | grep -q 'directory\|file'; then
    printf '==> FILE or DIRECTORY\n';
else
    printf '==> NOT VALID\n';
fi
