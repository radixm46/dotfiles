typeset -U path PATH

case "$(uname)" in
    Darwin)
        BREW_PATH_OPT='/opt/homebrew/bin'
        BREW_PATH_LOCAL='/usr/local/bin'
        function runs_on_ARM64() { [[ $(uname -m) = 'arm64' ]]; }
        function runs_on_X86_64() { [[ $(uname -m) = 'x86_64' ]]; }
        function brew_exists_at_opt() { [[ -d $BREW_PATH_OPT ]]; }
        function brew_exists_at_local() { [[ -d $BREW_PATH_LOCAL ]]; }

        setopt no_global_rcs #disable path helper
        path=($path /usr/sbin /sbin) # init path

        # switch homebrew dir by arch
        if runs_on_ARM64; then
            if brew_exists_at_opt && brew_exists_at_local; then
                path=($BREW_PATH_OPT(N-/) $BREW_PATH_LOCAL(N-/) $path)
                function onr() { $BREW_PATH_LOCAL/$@; }
                alias rzsh='onr zsh'
            elif brew_exists_at_local; then
                path=($BREW_PATH_LOCAL(N-/) $path)
            elif brew_exists_at_opt; then
                path=($BREW_PATH_OPT(N-/) $path)
            fi
        elif runs_on_X86_64 && brew_exists_at_local; then
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

function is_available() { hash "$1" >/dev/null 2>&1; return $?; }

# check stack installed and add path
if is_available stack; then
    path=($(stack path --local-bin)(N-/) $path)
fi

# check rust environment
if [[ -d $HOME/.cargo/bin ]]; then
    path=("$HOME/.cargo/bin"(N-/) $path)
fi

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
fi
