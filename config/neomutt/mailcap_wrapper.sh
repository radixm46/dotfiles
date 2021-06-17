#!/bin/sh
# define type by argument
file_type=$1
file_name=$2

case "$(uname)" in
    Darwin)
        function macos_open () {
            printf "opening ${file_name}\n o) open\n p) preview\n f) firefox\n e) emacs\n >" 1>&2
            read cmd
            case ${cmd:='o'} in
                o) exec open ${file_name} ;;
                p) exec open ${file_name} -a preview.app ;;
                f) exec open ${file_name} -a firefox.app ;;
                e) exec emacsclient -a emacs \
                        -e '(xwidget-webkit-browse-url "file://'"${file_name}"'")' ;;
                *) printf 'not defined option, exit' ;;
            esac
        }

        case "${file_type}" in
            docx) exec open ${file_name} ;;
            xlsx) exec open ${file_name} ;;
            zip) exec open ${file_name} ;;
            *) macos_open ;;
        esac
        ;;
    Linux*)
        # require xdg-open
        function linux_open_with_xdg () {
            printf "opening ${file_name}\n o) xdg-open\n e) emacs\n >" 1>&2
            read cmd
            case ${cmd:='o'} in
                o) exec xdg-open ${file_name} ;;
                e) exec emacsclient -a emacs \
                        -e '(xwidget-webkit-browse-url "file://'"${file_name}"'")' ;;
                *) printf 'not defined option, exit' ;;
            esac
        }
        case "${file_type}" in
            html) linux_open_with_xdg ;;
            *) xdg-open ${file_name} ;;
        esac
        ;;
    *)  ;;
esac
