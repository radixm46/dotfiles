export LANG=ja_JP.UTF-8
export XDG_CONFIG_HOME=~/.config
# export LANGUAGE=en_US.UTF-8
# export LC_ALL=en_US.UTF-8
# export LC_CTYPE=en_US.UTF-8
# ------------------------------------------------------------------------------
# define functions

# check given command is available via 'hash
function is_available() { hash "$1" >/dev/null 2>&1; return $?; }

# setup path, environment
typeset -U path PATH

case "$(uname)" in
    Darwin)
        setopt no_global_rcs #disable path helper
        path=(${path} '/usr/sbin'(N-/) '/sbin'(N-/)) # init path

        # return true if macOS runs on certain arch
        function runs_on_macARM64() { [[ "$(uname -m)" = 'arm64' ]]; }
        function runs_on_macX86_64() { [[ "$(uname -m)" = 'x86_64' ]]; }
        # path of hoembrew on /opt/homebrew
        BREW_PATH_OPT='/opt/homebrew'
        function brew_exists_at_opt() { [[ -d "${BREW_PATH_OPT}/bin" ]]; }
        # path of hoembrew on /usr/local
        BREW_PATH_LOCAL='/usr/local'
        function brew_exists_at_local() { [[ -d "${BREW_PATH_LOCAL}/bin/Homebrew" ]]; }

        # path add BREW_PATH_OPT/zsh to fpath
        path=(${BREW_PATH_OPT}/bin(N-/) ${BREW_PATH_OPT}/sbin(N-/) \
              ${BREW_PATH_LOCAL}/bin(N-/) ${BREW_PATH_LOCAL}/sbin(N-/) \
              ${path})
        # switch homebrew dir by arch
        if runs_on_macARM64 && brew_exists_at_opt; then
            fpath=(${BREW_PATH_OPT}/share/zsh/site-functions(N-/) ${fpath})
            # switch arch
            function x86() { arch -x86_64 $@; }
            function arm() { arch -arm64 $@; }
            # launch zsh on rosetta2
            function zsh_ovrst2() {
                if [[ -e "${BREW_PATH_LOCAL}/bin/zsh" ]]; then
                    x86 "${BREW_PATH_LOCAL}/bin/zsh"
                else
                    x86 '/bin/zsh'
                fi
            }
        elif runs_on_macX86_64 && brew_exists_at_local; then
            path=(\
                ${BREW_PATH_LOCAL}/bin(N-/) ${BREW_PATH_LOCAL}/sbin(N-/) \
                ${path})
        fi

        # if macvim available, alias vim to macvim
        if [[ -d '/Applications/MacVim.app' ]]; then
            alias vim='/Applications/MacVim.app/Contents/MacOS/Vim'
            alias vi='vim'
        fi
        ;;
esac

# check stack installed and add path
if is_available 'stack'; then
    path=($(stack path --local-bin)(N-/) ${path})
fi

# check rust environment
path=("${HOME}/.cargo/bin"(N-/) ${path})

# configure pyenv
if is_available 'pyenv'; then
    export PIPENV_VENV_IN_PROJECT=true
    eval "$(pyenv init --path)"
fi

# configure node environment
if is_available 'npm' && [[ -d "${HOME}/.npm-global" ]]; then
    export NPM_CONFIG_PREFIX="${HOME}/.npm-global"
    path=("${NPM_CONFIG_PREFIX}/bin"(N-/) ${path})
fi

# set manpager as bat or nvim if available
if is_available 'bat'; then
    export MANPAGER="sh -c 'col -bx | bat -l man -p'"
elif is_available 'nvim'; then
    export MANPAGER="nvim -c 'set ft=man' -"
fi


# simple timer function for tea preparation
function ktimer() {
    local wait_min=${1:-'5'}
    local wait_sec=${2:-'0'}
    printf "Counting down %01d:%02d\n" ${wait_min} ${wait_sec}
    local remaining=$((60 * ${wait_min} + ${wait_sec}))
    while [ ${remaining} -gt 0 ]
    do
        if (( ${remaining} > 60 )); then
            printf "\r\033[KRemaining... %01d:%02d" $((remaining / 60)) $((remaining % 60))
        elif (( ${remaining} > 10 )); then
            printf "\r\033[KRemaining... \033[31m%01d:%02d\033[m" $((remaining / 60)) $((remaining % 60))
        else
            printf "\r\033[KRemaining... \033[31m\033[1m%01d:%02d\033[m" $((remaining / 60)) $((remaining % 60))
        fi
        ((remaining-=1))
        sleep 1
    done
    printf "\r\033[KRemaining... \033[31m\033[1mdone!\033[m\n"

    case ${OSTYPE} in
        darwin*)
            (say --voice=Victoria \
                "$(case ${wait_min} in; 0) ;; 1) printf "1 minute" ;; *) printf "%.d minutes" ${wait_min} ;; esac)" \
                "$(case ${wait_sec} in; 0) ;; 1) printf "1 second" ;; *) printf "%.d seconds" ${wait_sec} ;; esac)" \
                "passed" &)
            ;;
        linux*)
            printf "\a" # ring a terminal bell
            ;;
        *);;
    esac
}
