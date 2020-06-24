#!/bin/sh
printf 'Checking: '${1}; \
if [ ! -e ${1}  ]; then \
    printf '\t\t NOT EXIST\n'; \
elif file -b ${1} | grep -q 'symbolic link'; then \
    printf "\t $(file -b ${1})\n"; \
elif file -b ${1} | grep -q 'directory\|file'; then \
    printf '\t\t FILE or DIRECTORY\n'; \
else
    printf '\t\t NOT VALID\n'; \
fi
