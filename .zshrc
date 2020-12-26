export LANG=ja_JP.UTF-8
export XDG_CONFIG_HOME=~/.config
# export LANGUAGE=en_US.UTF-8
# export LC_ALL=en_US.UTF-8
# export LC_CTYPE=en_US.UTF-8
# TODO: should not write here
# ------------------------------------------------------------------------------
# zplugin
# ------------------------------------------------------------------------------
ZINITDIR=$HOME/.zinit
if [ ! -d $ZINITDIR ]; then
    echo 'Installing zplugin...'
    echo "target dir: $ZINITDIR"
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/zdharma/zplugin/master/doc/install.sh)"
fi

source $HOME/.zinit/bin/zplugin.zsh
autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin

zplugin light zsh-users/zsh-autosuggestions
zplugin light zsh-users/zsh-completions
zplugin light zdharma/fast-syntax-highlighting

autoload -Uz compinit
compinit -u

# ------------------------------------------------------------------------------
# enable colors
autoload -Uz colors
colors

# 10ms for key sequences
KEYTIMEOUT=1
# Input/Output
setopt no_flow_control
setopt ignore_eof # zsh not terminate with C-d
setopt interactive_comments # '#' 以降をコメントとして扱う
setopt print_eight_bit
setopt no_beep
# setopt short_loops

# Changing dir
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
# setopt chase_dots
# setopt chase_link
# setopt posix_cd

# expansion and globbing
setopt extended_glob
autoload -Uz select-word-style

# ZLE zsh line editor
zstyle ':zle:*' word-chars " /=;@:{},|"
zstyle ':zle:*' word-style unspecified


# ------------------------------------------------------------------------------
# history configure
HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000
setopt share_history
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt hist_reduce_blanks

# ------------------------------------------------------------------------------
# completions
setopt list_types
setopt auto_menu
setopt auto_param_keys
setopt complete_in_word
setopt always_last_prompt
# ignore cases
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# ../ の後は今いるディレクトリを補完しない
zstyle ':completion:*' ignore-parents parent pwd ..

# sudo の後ろでコマンド名を補完する
zstyle ':completion:*:sudo:*' command-path \
       /usr/local/sbin \
       /usr/local/bin \
       /usr/sbin \
       /usr/bin \
       /sbin \
       /bin \
       # /usr/X11R6/bin

# ps コマンドのプロセス名補完
zstyle ':completion:*:processes' command 'ps x -o pid,s,args'

# ------------------------------------------------------------------------------
# keybinds

bindkey '^R' history-incremental-pattern-search-backward
bindkey -v  # use vim style keybinding

# ------------------------------------------------------------------------------
# alias
function is_available() { hash "$1" >/dev/null 2>&1; return $?; }

case ${OSTYPE} in
    darwin*)
        if is_available exa; then
            LSCOM="exa"
            alias ls='${LSCOM} -F --color=auto'
        else
            LSCOM="ls"
            export CLICOLOR=1
            alias ls='ls -G -F'
        fi
        ;;
    linux*)
        if is_available exa; then
            LSCOM="exa"
        else
            LSCOM="ls"
        fi
        alias ls='${LSCOM} -F --color=auto'
        ;;
esac

alias la="${LSCOM} -a"
alias ll="${LSCOM} -l"

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

alias mkdir='mkdir -p'

if is_available fd; then
    alias find='fd'
fi

# glob aliases
alias -g L='| less'
alias -g G='| grep'

if is_available bat; then
    alias -g B='| bat'
fi

# clipboard manager
if is_available pbcopy; then
    alias -g C='| pbcopy'
elif is_available xsel; then
    alias -g C='| xsel --input --clipboard'
elif is_available putclip; then
    alias -g C='| putclip'
fi

# ------------------------------------------------------------------------------
P_PROM=''

# with nerd fonts
P_LOGIN=$'\UF2BD'
case $(uname) in
    Darwin)
        if [[ $(uname -m) == x86_64 ]]; then
            P_COMPUTER=$'\UF302 \UF129'
        else
            P_COMPUTER=$'\UF302'
        fi
        ;;
    Linux*)
        P_COMPUTER=$'\UF83C'
        ;;
    *)
        P_COMPUTER=$'\UF841'
        ;;
esac
P_BEGINR=$'\UE0C7'
P_BEGIN=$'\UE0C6'
P_MIDTEX=$'\UE0C4'
P_MIDTEXR=$'\UE0C5'
P_END=$'\UE0C6'
P_ENDR=$'\UE0C7'
P_DIR=$'\UF07C'
P_GITBRANCH=$'\UF418'
P_VCSICO=$'\UF7A1'
PROMPT="\
%{${bg[green]}%}%{${fg[red]}%} ${P_LOGIN}%{${reset_color}%}\
%{${bg[green]}%}%{${fg_bold[black]}%} %n %{${reset_color}%}\
%{${bg[blue]}%}%{${fg[green]}%}${P_MIDTEX}%{${reset_color}%}\
%{${bg[blue]}%}%{${fg[red]}%} ${P_COMPUTER}%{${reset_color}%}\
%{${bg[blue]}%}%{${fg_bold[black]}%} %m %{${reset_color}%}\
%{${bg[black]}%}%{${fg[blue]}%}${P_BEGIN}%{${reset_color}%}\
%{${fg[black]}%}${P_MIDTEX}%{${reset_color}%}
%{${bg[black]}%}%{${fg[green]}%} ${P_DIR}%{${reset_color}%}\
%{${bg[black]}%}%{${fg[brblack]}%} %~  %{${reset_color}%}\
%{${fg[black]}%}${P_MIDTEX}%{${reset_color}%}
%{${fg[black]}%}${P_PROM}%{${reset_color}%}%# "

# ------------------------------------------------------------------------------
# vcs_info
autoload -Uz vcs_info
autoload -Uz add-zsh-hook

zstyle ':vcs_info:*' formats \
"%F{black}${P_MIDTEXR}\
%K{black}%F{green} ${P_VCSICO} %F{brblack}%s %F{magenta}${P_ENDR}\
%{${bg[magenta]}%}%{${fg[white]}%} ${P_GITBRANCH}%{${fg[white]}%} %b %f%k%{${reset_color}%}"

zstyle ':vcs_info:*' actionformats \
"%F{black}${P_MIDTEXR}\
%K{black}%F{green} ${P_VCSICO} %F{brblack}%s %F{magenta}${P_ENDR}\
%{${bg[magenta]}%}%{${fg[white]}%} ${P_GITBRANCH}%{${fg[white]}%} %b %a %f%k%{${reset_color}%}"

function _update_vcs_info_msg() {
    LANG=en_US.UTF-8 vcs_info
    RPROMPT="${vcs_info_msg_0_}"
}
add-zsh-hook precmd _update_vcs_info_msg
# ------------------------------------------------------------------------------

get_weather() {
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

alias -g wttr='get_weather'


# ------------------------------------------------------------------------------
#http://qiita.com/b4b4r07/items/01359e8a3066d1c37edc
function is_exists() { type "$1" >/dev/null 2>&1; return $?; }
function is_osx() { [[ $OSTYPE == darwin* ]]; }
function is_screen_running() { [ ! -z "$STY" ]; }
function is_tmux_runnning() { [ ! -z "$TMUX" ]; }
function is_screen_or_tmux_running() { is_screen_running || is_tmux_runnning; }
function shell_has_started_interactively() { [ ! -z "$PS1" ]; }
function is_ssh_running() { [ ! -z "$SSH_CONECTION" ]; }

function tmux_automatically_attach_session()
{
    if is_screen_or_tmux_running; then
        ! is_exists 'tmux' && return 1

        if is_tmux_runnning; then
            echo ""
            echo "${fg_bold[red]}.||.  .. .. ..   ... ...  ... ...${reset_color}"
            echo "${fg_bold[red]} ||    || || ||   ||  ||   '|..' ${reset_color}"
            echo "${fg_bold[red]} ||    || || ||   ||  ||    .|.  ${reset_color}"
            echo "${fg_bold[red]} '|.' .|| || ||.  '|..'|. .|  ||.${reset_color}"
            echo ""
        elif is_screen_running; then
            echo "This is on screen."
        fi
    else
        if shell_has_started_interactively && ! is_ssh_running; then
            if ! is_exists 'tmux'; then
                echo 'Error: tmux command not found' 2>&1
                return 1
            fi

            if tmux has-session >/dev/null 2>&1 && tmux list-sessions | grep -qE '.*]$'; then
                # detached session exists
                tmux list-sessions
                echo -n "Tmux: attach? (y/N/num) "
                read
                if [[ "$REPLY" =~ ^[Yy]$ ]] || [[ "$REPLY" == '' ]]; then
                    tmux attach-session
                    if [ $? -eq 0 ]; then
                        echo "$(tmux -V) attached session"
                        return 0
                    fi
                elif [[ "$REPLY" =~ ^[0-9]+$ ]]; then
                    tmux attach -t "$REPLY"
                    if [ $? -eq 0 ]; then
                        echo "$(tmux -V) attached session"
                        return 0
                    fi
                fi
            fi

            if is_osx && is_exists 'reattach-to-user-namespace'; then
                # on OS X force tmux's default command
                # to spawn a shell in the user's namespace
                tmux_config=$(cat $HOME/.tmux.conf <(echo 'set-option -g default-command "reattach-to-user-namespace -l $SHELL"'))
                tmux -f <(echo "$tmux_config") new-session && echo "$(tmux -V) created new session supported OS X"
            else
                tmux new-session && echo "tmux created new session"
            fi
        fi
    fi
}
tmux_automatically_attach_session
