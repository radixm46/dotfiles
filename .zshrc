# ------------------------------------------------------------------------------
# zplugin
# ------------------------------------------------------------------------------
ZINITDIR=$HOME/.zinit
if [ ! -d $ZINITDIR ]; then
    echo 'Installing zplugin...'
    echo "target dir: $ZINITDIR"
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/zdharma/zplugin/master/doc/install.sh)"
fi

source "${HOME}/.zinit/bin/zplugin.zsh"
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

zstyle ':completion:*' ignore-parents parent pwd ..

zstyle ':completion:*:sudo:*' \
       command-path /sbin /usr/sbin \
       ennviron PATH="/sbin:/usr/sbin:$PATH"

# ps コマンドのプロセス名補完
zstyle ':completion:*:processes' command 'ps x -o pid,s,args'

# ------------------------------------------------------------------------------
# keybinds

bindkey '^R' history-incremental-pattern-search-backward
bindkey -v  # use vim style keybinding

# ------------------------------------------------------------------------------
# alias

# define 'ls' default command
function alias_ls() {
    if is_available 'exa'; then
        alias ls="exa -F --color=always --icons"
        alias lss="exa -F --color=auto"
        alias ll="ls -l --git"
        alias lla="ls -la --git"
        alias la="ls -a"
    else
        case ${OSTYPE} in
            darwin*)
                export CLICOLOR=1
                alias ls="ls -G -F"
                ;;
            linux*)
                alias ls="ls -F --color=auto"
                ;;
            *) ;;
        esac
        alias la="ls -a"
        alias ll="ls -l"
    fi
}

alias_ls

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

alias mkdir='mkdir -p'

if is_available 'fd'; then
    alias find='fd'
fi

# glob aliases
alias -g L='| less'
alias -g G='| grep'

if is_available 'bat'; then
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

if is_available 'emacs'; then
    # launch emacs daemon background and forked
    function emacsd() { (emacs --bg-daemon=$@ &); }
fi

# ------------------------------------------------------------------------------
function generate_prompt() {
    local P_PROM=''
    # with nerd fonts
    local P_LOGIN=$'\UF2BD'
    local P_COMPUTER=$(print_os_glyph)
    local P_BEGINR=$'\UE0C7'
    local P_BEGIN=$'\UE0C6'
    local P_MIDTEX=$'\UE0C4'
    local P_END=$'\UE0C6'
    local P_DIR=$'\UF07C'
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
}
generate_prompt

# ------------------------------------------------------------------------------
# vcs_info
autoload -Uz vcs_info
autoload -Uz add-zsh-hook

function define_vcs_prompt_style {
    local P_GITBRANCH=$'\UF418'
    local P_VCSICO=$'\UF7A1'
    local P_ENDR=$'\UE0C7'
    local P_MIDTEXR=$'\UE0C5'

    zstyle ':vcs_info:*' formats \
"%F{black}${P_MIDTEXR}\
%K{black}%F{green} ${P_VCSICO} %F{brblack}%s %F{magenta}${P_ENDR}\
%{${bg[magenta]}%}%{${fg[white]}%} ${P_GITBRANCH}%{${fg[white]}%} %b %f%k%{${reset_color}%}"

    zstyle ':vcs_info:*' actionformats \
"%F{black}${P_MIDTEXR}\
%K{black}%F{green} ${P_VCSICO} %F{brblack}%s %F{magenta}${P_ENDR}\
%{${bg[magenta]}%}%{${fg[white]}%} ${P_GITBRANCH}%{${fg[white]}%} %b %a %f%k%{${reset_color}%}"
}
define_vcs_prompt_style

function _update_vcs_info_msg() {
    LANG=en_US.UTF-8 vcs_info
    RPROMPT="${vcs_info_msg_0_}"
}
add-zsh-hook precmd _update_vcs_info_msg
# ------------------------------------------------------------------------------

alias wttr='get_weather'

# ------------------------------------------------------------------------------
#http://qiita.com/b4b4r07/items/01359e8a3066d1c37edc
function is_osx() { [[ $OSTYPE == darwin* ]]; }
function is_screen_running() { [ ! -z "$STY" ]; }
function is_tmux_runnning() { [ ! -z "$TMUX" ]; }
function is_screen_or_tmux_running() { is_screen_running || is_tmux_runnning; }
function shell_has_started_interactively() { [ ! -z "$PS1" ]; }
function is_ssh_running() { [ ! -z "$SSH_CONECTION" ]; }

# detect if tmux is running and attach
function tmux_automatically_attach_session()
{
    if is_screen_or_tmux_running; then
        ! is_available 'tmux' && return 1

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
            if ! is_available 'tmux'; then
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

            if is_osx && is_available 'reattach-to-user-namespace'; then
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
