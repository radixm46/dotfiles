#!/bin/sh
function is_available() { hash "$1" >/dev/null 2>&1; return $?; }

case "$(uname)" in
    Darwin)
        if is_available terminal-notifier; then
            terminal-notifier -title "$1" -subtitle 'New Mail' \
                          -message "$2 new messages, $3 unread." \
                          -sound default -activate 'com.apple.Terminal'
        fi
        ;;
    Linux*)
        if is_available notify-send; then
            notify-send "$1 New Mail" \
                        "$2 new messages, $3 unread." \
                        --icon=dialog-information
        fi
        ;;
esac
