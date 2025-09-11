# shellcheck disable=SC1036 # disable "use parens inside array"
# shellcheck disable=SC2128 # disable "expanding an array without an index"
# shellcheck disable=SC2206 # disable "quote to prevent word split"
# compile when update
[ ! -f "${HOME}/.zshenv.zwc" ] || [ "${HOME}/.zshenv" -nt "${HOME}/.zshenv.zwc" ] && {
    [[ -o interactive ]] && printf 'compile zshenv...'
    zcompile "${HOME}/.zshenv"
    [[ -o interactive ]] && printf 'done!\n'
}

# ------------------------------------------------------------------------------
# define functions
function() {
    if (( $+commands[locale] )); then
        # filter locales containing UTF-8 / utf8
        local -ra loc_u=(${(M)$(locale -a)#*(UTF-8|utf8)*})
        local l
        for pt in ja_JP en_US C; do
            [[ ${l:=${loc_u[(R)$pt*]}} ]] && {
                export LANG=$l
                return 0
            }
        done
    fi
    : ${LANG:=C}
    export LANG
}

# check given command is available via 'hash
function is_available() { hash "$1" >/dev/null 2>&1; return $?; }
export LC_CTYPE=en_JP.UTF-8
export LANGUAGE="ja:en_US:en"
: "${XDG_CONFIG_HOME:=$HOME/.config}"
: "${XDG_CACHE_HOME:=$HOME/.cache}"
: "${XDG_DATA_HOME:=$HOME/.local/share}"
export XDG_CONFIG_HOME XDG_CACHE_HOME XDG_DATA_HOME

# ------------------------------------------------------------------------------
# setup path, environment
typeset -U path PATH

case "$(uname)" in
    Darwin)
        setopt no_global_rcs #disable path helper
        path=(${path} '/usr/sbin'(N-/) '/sbin'(N-/)) # init path

        # return true if macOS runs on certain arch
        function runs_on_macARM64() { [ "$(uname -m)" = 'arm64' ]; }
        function runs_on_macX86_64() { [ "$(uname -m)" = 'x86_64' ]; }

        # path of hoembrew on /opt/homebrew
        function() {
            local BREW_PATH_OPT='/opt/homebrew'
            function brew_exists_at_opt() { [ -d "${BREW_PATH_OPT}/bin" ]; }
            # path of hoembrew on /usr/local/Homebrew
            function brew_exists_at_local() { [ -d '/usr/local/Homebrew' ]; }

            # path add BREW_PATH_OPT/zsh to fpath
            path=(${BREW_PATH_OPT}/bin(N-/) ${BREW_PATH_OPT}/sbin(N-/) \
                                  /usr/local/bin(N-/) /usr/local/sbin(N-/) \
                                  ${path})
            # switch homebrew dir by arch
            if runs_on_macARM64 && brew_exists_at_opt; then
                fpath=(${BREW_PATH_OPT}/share/zsh/site-functions(N-/) ${fpath})
                # switch arch
                # shellcheck disable=SC2068
                function x86() { arch -x86_64 $@; }
                # shellcheck disable=SC2068
                function arm() { arch -arm64 $@; }
                # launch zsh on rosetta2
                function zsh_ovrst2() {
                    if [ -e '/usr/local/bin/zsh' ]; then
                        x86 '/usr/local/bin/zsh'
                    else
                        x86 '/bin/zsh'
                    fi
                }
            elif runs_on_macX86_64 && brew_exists_at_local; then
                path=(/usr/local/bin(N-/) /usr/local/sbin(N-/) \
                                    ${path})
            fi
        }
        ;;
    Linux*)
        function() {
            local BREW_PATH='/home/linuxbrew/.linuxbrew'
            [ -d "${BREW_PATH}" ] && {
                path=(${BREW_PATH}/bin(N-/) ${BREW_PATH}/sbin(N-/) \
                                  ${path})
                fpath=(${BREW_PATH}/share/zsh/site-functions(N-/) ${fpath})
            }
        }
        ;;
esac


# check rust environment
path=("${HOME}/.cargo/bin"(N-/) ${path})
path=("${HOME}/.local/bin"(N-/) ${path})

# configure node environment
is_available 'npm' && [ -d "${HOME}/.npm-global" ] && {
    export NPM_CONFIG_PREFIX="${HOME}/.npm-global"
    path=("${NPM_CONFIG_PREFIX}/bin"(N-/) ${path})
}

# do when interactive with command execution
[[ -o interactive ]] && {

    # check ghcup/stack installed and add path
    function() {
        local ghcup_path="${HOME}/.ghcup/bin"
        if [ -d ${ghcup_path} ]; then
            path=("${ghcup_path}"(N-/) ${path})
        elif is_available 'stack'; then
            path=("$(stack path --local-bin)"(N-/) ${path})
        fi
    }

    is_available 'dotnet' && {
        case ${OSTYPE} in
            darwin* )
                # shellcheck disable=SC2155
                is_available 'brew' && \
                    export DOTNET_ROOT="$(brew --prefix)/opt/dotnet/libexec"
                ;;
            linux* )
                # NOTE: temporary configured for test (not checked yet!!)
                if is_available brew; then
                    export DOTNET_ROOT="/home/linuxbrew/.linuxbrew/opt/dotnet/libexec"
                else
                    :
                fi
                ;;
        esac
    }

    # configure pyenv
    if is_available 'pyenv' || [ -d ${HOME}/.pyenv/bin ]; then
        path=("${HOME}/.pyenv/bin"(N-/) ${path}) # if pyenv dir found at $HOME
        export PIPENV_VENV_IN_PROJECT=true
        eval "$(pyenv init --path)"
    fi
}
