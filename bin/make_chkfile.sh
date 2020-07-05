#!/bin/sh
printf '> Checking... '${1}'\n'; \
if [ ! -e ${1}  ]; then \
    printf '==> NOT EXIST\n'; \
elif file -b ${1} | grep -q 'symbolic link'; then \
    printf "==> $(file -b ${1})\n"; \
elif file -b ${1} | grep -q 'directory\|file'; then \
    printf '==> FILE or DIRECTORY\n'; \
else
    printf '==> NOT VALID\n'; \
fi
