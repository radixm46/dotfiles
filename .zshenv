setopt no_global_rcs #disable path helper on osx


case "$(uname)" in

    Darwin) # OSがMacならば
        export PATH="/usr/sbin:/sbin:/usr/local/bin:$PATH" #add path for homebrew

        if [[ -d /Applications/MacVim.app ]]; then # MacVimが存在するならば
            alias vim=/Applications/MacVim.app/Contents/MacOS/Vim
            alias vi=vim
        fi

        ;;

esac

if [[ -e $HOME/.profile ]]; then
    source $HOME/.profile  # load .profile if exists
fi

# check stack installed and add path
if hash stack 2>/dev/null; then
    STACK_PATH=$(stack path --local-bin)
    if [[ -e $STACK_PATH ]]; then
        export PATH="$STACK_PATH:$PATH"
    fi
fi

# check rust environment
if hash cargo 2>/dev/null; then
    CARGO_PATH="$HOME/.cargo/bin"
    if [[ -e $STACK_PATH ]]; then
        export PATH="$CARGO_PATH:$PATH"
    fi
fi

# export LANGUAGE=en_US.UTF-8
# export LC_ALL=en_US.UTF-8
# export LC_CTYPE=en_US.UTF-8
# export LANG=en_US.UTF-8
