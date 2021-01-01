typeset -U path PATH

function is_available() { hash "$1" >/dev/null 2>&1; return $?; }

case "$(uname)" in
    Darwin)
        function runs_on_ARM64() { [[ $(uname -m) = 'arm64' ]]; }
        function runs_on_X86_64() { [[ $(uname -m) = 'x86_64' ]]; }

        BREW_PATH_OPT='/opt/homebrew/bin'
        BREW_PATH_LOCAL='/usr/local/bin'
        function brew_exists_at_opt() { [[ -d ${BREW_PATH_OPT} ]]; }
        function brew_exists_at_local() { [[ -d ${BREW_PATH_LOCAL} ]]; }


        setopt no_global_rcs #disable path helper
        path=($path /usr/sbin /sbin) # init path

        # switch homebrew dir by arch
        if runs_on_ARM64; then
            path=($BREW_PATH_OPT(N-/) $BREW_PATH_LOCAL(N-/) $path)
            if brew_exists_at_opt && brew_exists_at_local; then
                function onr() { $BREW_PATH_LOCAL/$@; }
                alias rzsh='onr zsh'
            fi
        elif runs_on_X86_64; then
            path=($BREW_PATH_LOCAL(N-/) $path)
        fi

        if [[ -d /Applications/MacVim.app ]]; then
            alias vim=/Applications/MacVim.app/Contents/MacOS/Vim
            alias vi=vim
        fi
        # export CFLAGS="-I$(xcrun --show-sdk-path)/usr/include"
        # export LDFLAGS="-L/usr/local/opt/zlib/lib"
        # export CPPFLAGS="-I/usr/local/opt/zlib/include"

        ;;
esac

# check stack installed and add path
if is_available stack; then
    path=($(stack path --local-bin)(N-/) $path)
fi

# check rust environment
path=("$HOME/.cargo/bin"(N-/) $path)

# configure pyenv
if is_available pyenv; then
    # PYENV_ROOT="${HOME}/.pyenv"
    # path=($PYENV_ROOT/bin(N-/) $path)
    export PIPENV_VENV_IN_PROJECT=true
    eval "$(pyenv init -)"
fi

# configure node environment
if is_available npm && [[ -d $HOME/.npm-global ]]; then
    export NPM_CONFIG_PREFIX="${HOME}/.npm-global"
    path=($NPM_CONFIG_PREFIX/bin(N-/) $path)
fi
