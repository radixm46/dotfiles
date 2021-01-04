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
    while getopts d:l:sf: option
    do
        case ${option} in
            d) local days="${OPTARG}"  # TODO: chack value(0-3)
                ;;
            l) local lang_='&lang='"${OPTARG}"
                ;;
            s) local format='&format=%l:+%c++%t++%w++%p++%P'
                ;;
            f) local format='&format='"${OPTARG}"
                ;;
        esac
    done

    shift $((OPTIND - 1))
    local target_loc="$1"

    curl -s 'wttr.in/'"${target_loc}"\?"${days:=1}"'qA'"${lang_}""${format:=}"
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
                    function emacs() { x86 'emacs' $@; }
                fi
                # launch zsh on local brew
                alias rzsh='lbr zsh'
            fi
        elif runs_on_X86_64; then
            path=(${BREW_PATH_LOCAL}(N-/) ${path})
        fi

        if [[ -d '/Applications/MacVim.app' ]]; then
            alias vim='/Applications/MacVim.app/Contents/MacOS/Vim'
            alias vi='vim'
        fi
        # export CFLAGS="-I$(xcrun --show-sdk-path)/usr/include"
        # export LDFLAGS="-L/usr/local/opt/zlib/lib"
        # export CPPFLAGS="-I/usr/local/opt/zlib/include"

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
