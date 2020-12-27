typeset -U path PATH

case "$(uname)" in
    Darwin)
        function runs_onArm64() { [ $(uname -m) = 'arm64' ]; }
        function runs_onX86_64() { [ $(uname -m) = 'x86_64' ]; }
        BREW_PATH_OPT='/opt/homebrew/bin'
        BREW_PATH_LOCAL='/usr/local/bin'
        function brew_exists_on_opt() { [[ -d $BREW_PATH_OPT ]]; }
        function brew_exists_on_local() { [[ -d $BREW_PATH_LOCAL ]]; }

        setopt no_global_rcs #disable path helper
        path=($path /usr/sbin /sbin) # init path

        # switch homebrew dir by arch
        if runs_onArm64; then
            if brew_exists_on_opt && brew_exists_on_local; then
                path=($BREW_PATH_OPT(N-/) $BREW_PATH_LOCAL(N-/) $path)
                function onr() { $BREW_PATH_LOCAL/$@; }
            elif brew_exists_on_local; then
                path=($BREW_PATH_LOCAL(N-/) $path)
            elif brew_exists_on_opt; then
                path=($BREW_PATH_OPT(N-/) $path)
            fi
        elif runs_onX86_64 && brew_exists_on_local; then
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
if hash stack 2>/dev/null; then
    path=($(stack path --local-bin)(N-/) $path)
fi

# check rust environment
if hash cargo 2>/dev/null && [[ -d $HOME/.cargo/bin ]]; then
    path=("$HOME/.cargo/bin"(N-/) $path)
fi

# configure pyenv
if hash pyenv 2>/dev/null; then
    # PYENV_ROOT="${HOME}/.pyenv"
    # path=($PYENV_ROOT/bin(N-/) $path)
    export PIPENV_VENV_IN_PROJECT=true
    eval "$(pyenv init -)"
fi

# configure node environment
if hash node 2>/dev/null && [[ -d $HOME/.npm-global ]]; then
    export NPM_CONFIG_PREFIX="${HOME}/.npm-global"
fi
