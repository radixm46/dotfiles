typeset -U path PATH

case "$(uname)" in
    Darwin)
        # setopt no_global_rcs #disable path helper
        HBREW_DIR="/usr/local/bin"
        path=($HBREW_DIR(N-/) $path)

        if [[ -d /Applications/MacVim.app ]]; then
            alias vim=/Applications/MacVim.app/Contents/MacOS/Vim
            alias vi=vim
        fi
        ;;
esac

# check stack installed and add path
if hash stack 2>/dev/null; then
    path=($(stack path --local-bin)(N-/) $path)
fi

# check rust environment
if hash cargo 2>/dev/null && [[ -d $HOME/.cargo/bin ]]; then
    path=("$HOME/.cargo/bin"(N-/) $path)
fi

# configure pyenv
if hash pyenv 2>/dev/null; then
    PYENV_ROOT="${HOME}/.pyenv"
    path=($PYENV_ROOT(N-/) $path)
    export PIPENV_VENV_IN_PROJECT=true
    eval "$(pyenv init -)"
fi

# configure node environment
if hash node 2>/dev/null && [[ -d $HOME/.npm-global ]]; then
    export NPM_CONFIG_PREFIX="${HOME}/.npm-global"
fi
