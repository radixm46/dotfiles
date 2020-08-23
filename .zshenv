setopt no_global_rcs #disable path helper on osx

if [ -z ${TMUX} ]; then

    case "$(uname)" in

        Darwin)
            # export PATH="/usr/sbin:/sbin:/usr/local/bin:$PATH" # homebrew path
            export PATH="/usr/local/bin:$PATH" # homebrew path
            export PATH="$HOME/Library/Python/3.7/bin:$PATH" # do not commit this line

            if [[ -d /Applications/MacVim.app ]]; then
                alias vim=/Applications/MacVim.app/Contents/MacOS/Vim
                alias vi=vim
            fi

            ;;

    esac

    # check stack installed and add path
    if hash stack 2>/dev/null; then
        STACK_PATH=$(stack path --local-bin)
        if [[ -e $STACK_PATH ]]; then
            export PATH="$STACK_PATH:$PATH"
        fi
    fi

    # check rust environment
    CARGO_PATH="$HOME/.cargo/bin"
    if [[ -e $CARGO_PATH ]]; then
        export PATH="$CARGO_PATH:$PATH"
    fi

    # configure pyenv
    if hash pyenv 2>/dev/null; then
        export PYENV_ROOT="${HOME}/.pyenv"
        if [[ -d "${PYENV_ROOT}" ]]; then
            export PATH="${PYENV_ROOT}/bin:$PATH"
            export PIPENV_VENV_IN_PROJECT=true
            eval "$(pyenv init -)"
            # eval "$(pyenv virtualenv-init -)"
        fi
    fi

    # configure node environment
    if hash node 2>/dev/null; then
        export NPM_CONFIG_PREFIX="${HOME}/.npm-global"
    fi

    # if [[ -e $HOME/.profile ]]; then
    #     source $HOME/.profile  # load .profile if exists
    # fi
fi
