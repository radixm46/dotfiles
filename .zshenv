export LANG=ja_JP.UTF-8
export XDG_CONFIG_HOME=~/.config
# export LANGUAGE=en_US.UTF-8
# export LC_ALL=en_US.UTF-8
# export LC_CTYPE=en_US.UTF-8
# ------------------------------------------------------------------------------
# define functions

# check given command is available via 'hash
function is_available() { hash "$1" >/dev/null 2>&1; return $?; }

# print os glyph from nerd fonts
function print_os_glyph() {
    case "$(uname)" in
        Darwin)
            if runs_on_X86_64; then
                printf $'\UF302 \UF129'
            else
                printf $'\UF302'
            fi
            ;;
        Linux*)
            printf $'\UF83C'
            ;;
        *)
            printf $'\UF841'
            ;;
    esac
}

# weather from wttr.in
function get_weather() {
    local format=""
    while getopts L:d:l:sf: option
    do
        case ${option} in
            l) local target_loc="${OPTARG}"
                ;;
            d) local days="${OPTARG}"  # TODO: chack value(0-3)
                ;;
            L) local lang_="${OPTARG}"
                ;;
            s) local format='%l:+%c++%t++%w++%p++%P'
                ;;
            f) local format=''"${OPTARG}"
                ;;
        esac
    done

    curl -s "wttr.in/${target_loc}"\?"${days:-0qn}A&lang=${lang_:-ja}&format=${format:-}"
}

# setup path, environment
typeset -U path PATH

case "$(uname)" in
    Darwin)
        setopt no_global_rcs #disable path helper
        path=(${path} '/usr/sbin'(N-/) '/sbin'(N-/)) # init path

        # return true if macOS runs on Arm64
        function runs_on_ARM64() { [[ "$(uname -m)" = 'arm64' ]]; }
        # return true if macOS runs on x86_64 or Rosetta2
        function runs_on_X86_64() { [[ "$(uname -m)" = 'x86_64' ]]; }

        # path of hoembrew on /opt
        BREW_PATH_OPT='/opt/homebrew/bin'
        # path of hoembrew on /usr/local
        BREW_PATH_LOCAL='/usr/local/bin'
        # returns true if hoembrew installed on /opt
        function brew_exists_at_opt() { [[ -d ${BREW_PATH_OPT} ]]; }
        # returns true if hoembrew installed on /usr/bin
        function brew_exists_at_local() { [[ -d ${BREW_PATH_LOCAL} ]]; }

        # switch homebrew dir by arch
        if runs_on_ARM64; then
            path=(${BREW_PATH_OPT}(N-/) ${BREW_PATH_LOCAL}(N-/) ${path})
            if brew_exists_at_opt && brew_exists_at_local; then
                # use local brew dir
                function lbr() { ${BREW_PATH_LOCAL}/$@; }
                # launch on rosetta2
                function x86() { arch -x86_64 $@; }
                # launch on arm (native)
                function arm() { arch -arm64 $@; }
                # wrap emacs
                if is_available 'emacs'; then
                    alias emacs='x86 emacs'
                fi
                # launch zsh on local brew
                if [[ -e "${BREW_PATH_LOCAL}/zsh" ]]; then
                    function rzsh() { "${BREW_PATH_LOCAL}/zsh"; }
                fi
            fi
        elif runs_on_X86_64; then
            path=(${BREW_PATH_LOCAL}(N-/) ${path})
        fi

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
    eval "$(pyenv init -)"
fi

# configure node environment
if is_available 'npm' && [[ -d "${HOME}/.npm-global" ]]; then
    export NPM_CONFIG_PREFIX="${HOME}/.npm-global"
    path=("${NPM_CONFIG_PREFIX}/bin"(N-/) ${path})
fi
