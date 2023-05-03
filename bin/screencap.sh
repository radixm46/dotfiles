#!/usr/bin/env bash
set -euo pipefail

# dependency check
for cmd in grim slurp wl-copy notify-send; do
    if ! command -v "${cmd}" > /dev/null 2>&1; then
        echo "Error: ${cmd} is not installed."
        exit 1
    fi
done

# targetdir
dir="${HOME}/Pictures/Screenshots"
[ ! -d "${dir}" ] && mkdir -p "${dir}"

# options
trim=false
clipboard=false
while getopts "tc" opt; do
    case "${opt}" in
        t) trim=true ;;
        c) clipboard=true ;;
        *) exit 1 ;;
    esac
done

# trim
if $trim; then
    geometry=$(slurp) || exit 1 # if geometry not acquired, quit
else
    geometry=""
fi

# handle screenshot data
if ${clipboard}; then
    # configure temp file for clipboard image
    TMPDIR="${HOME}/.local/tmp"
    [ ! -d "${TMPDIR}" ] && mkdir -p "${TMPDIR}"
    trap 'rm -f "${tmpfile}"' EXIT

    if [ -z "${geometry}" ]; then
        cap_copy() {
            grim - | wl-copy
        }
        cap_copy || exit 1
    else
        cap_copy_trim() {
            grim -g "$geometry" - | wl-copy
        }
        cap_copy_trim || exit 1
    fi
    tmpfile=$(mktemp -p "${TMPDIR}")
    wl-paste > "${tmpfile}"
    notify-send "Image captured!" "Copied to clipboard! 📋" \
                --urgency="low" --category="screenshot" \
                --hint=string:image-path:"${tmpfile}"
else
    imgfile="grim $(date '+%Y-%m-%d %H.%M.%S').png"
    imgpath="${dir}/${imgfile}"
    if [ -z "${geometry}" ]; then
        grim "${imgpath}" || exit 1
    else
        grim -g "${geometry}" "${imgpath}" || exit 1
    fi
    notify-send "Image saved!" "${imgpath}" \
                --urgency="low" --category="screenshot" \
                --hint=string:image-path:"${imgpath}"
fi
