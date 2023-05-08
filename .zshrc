# ----------------------------------------------------------------------------------------
# zinit install
# ----------------------------------------------------------------------------------------
ZDOTDIR="${XDG_CONFIG_HOME}/zsh"
ZINITDIR="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"
function zinit_install() {
    print -P "%F{33}▓▒░ %F{220}Installing DHARMA Initiative Plugin Manager (zdharma-continuum/zinit)…%f"
    command mkdir -p "$(dirname $ZINITDIR)" && command chmod g-rwX $(dirname ${ZINITDIR})
    command git clone https://github.com/zdharma-continuum/zinit.git "${ZINITDIR}" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
            print -P "%F{160}▓▒░ The clone has failed.%f%b"
}
[[ ! -d ${ZINITDIR} ]] && zinit_install
source "${ZINITDIR}/zinit.zsh"
# if failed to load zinit
if [[ $(type zinit) == 'zinit not found' ]]; then
    echo 'Failed to load zinit'
    function zinit() { return 1; }
else
    autoload -Uz _zinit
    (( ${+_comps} )) && _comps[zinit]=_zinit
fi

# load syntax-highilight, additional completions, and autosuggestions --------------------
zinit ice depth=1 && zinit wait lucid for \
      atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
      zdharma-continuum/fast-syntax-highlighting \
      blockf \
      zsh-users/zsh-completions \
      atload"!_zsh_autosuggest_start" \
      zsh-users/zsh-autosuggestions

# load zsh modules -----------------------------------------------------------------------
autoload -Uz \
         chpwd_recent_dirs \
         cdr \
         add-zsh-hook \
         vcs_info
add-zsh-hook chpwd chpwd_recent_dirs

# keybinds -------------------------------------------------------------------------------
bindkey -v  # use vim style keybinding
# 10ms for key sequences
export KEYTIMEOUT=1

# zvm ------------------------------------------------------------------------------------
# init immediately after sourced
ZVM_INIT_MODE=sourcing
function _vi_remap() {
    bindkey -M vicmd '^R' history-incremental-pattern-search-backward
    bindkey -M viins '^F' forward-char
    bindkey -M viins '^B' backward-char
    bindkey -M viins '^I' expand-or-complete
    bindkey -M viins '^H' backward-delete-char
    bindkey -M viins '^D' delete-char
    bindkey -M viins '^A' beginning-of-line
    bindkey -M viins '^E' end-of-line
    bindkey -M viins '^N' menu-expand-or-complete
    bindkey -M viins '^P' reverse-menu-complete
}
zinit ice depth=1 && zinit light jeffreytse/zsh-vi-mode && \
    ZVM_INSERT_MODE_CURSOR=$ZVM_CURSOR_BLOCK && \
    ZVM_NORMAL_MODE_CURSOR=$ZVM_CURSOR_BLOCK && \
    ZVM_LINE_INIT_MODE=$ZVM_MODE_INSERT && \
    _vi_remap # remap after zvm load
function zvm_config() {
    bindkey -M vicmd 'dd' zvm_kill_line
    bindkey -M vicmd 'D'  zvm_forward_kill_line
}

# substring search -----------------------------------------------------------------------
zinit ice depth=1 && zinit light zsh-users/zsh-history-substring-search
bindkey -M vicmd 'N' history-substring-search-up
bindkey -M vicmd 'n' history-substring-search-down

# auto pair brackets ---------------------------------------------------------------------
zinit ice depth=1 && zinit light hlissner/zsh-autopair && autopair-init

# plugins depends on external commands ---------------------------------------------------
zinit ice has'emacs' depth=1 && zinit light Flinner/zsh-emacs # alias
zinit ice has'systemctl' depth=1 && zinit light le0me55i/zsh-systemd # alias
zinit ice has'docker' depth=1 && zinit light akarzim/zsh-docker-aliases #alias

if is_available 'git'; then
    zinit ice depth=1 && zinit light mdumitru/git-aliases # alias
    zinit ice depth=1 && zinit light paulirish/git-open # open homepage of repo
    zinit ice depth=1 && zinit light mollifier/cd-gitroot # move to root dir of repo
fi

if is_available 'fzf'; then
    zinit ice has'jq' depth=1 && zinit light reegnz/jq-zsh-plugin

    # page-up page-down temporary binded like emacs
    export FZF_DEFAULT_OPTS="--multi --cycle --ansi --color=dark --reverse --marker=* --bind 'ctrl-v:page-down' --bind 'alt-v:page-up'"
    function is_tmux_newer_than() { [[ 1 == $((${${$(tmux -V)#tmux}%[a-z]} >= $1)) ]]; }
    function fzf_prev_command() {
        if is_available 'bat'; then
            printf '"bat --color=always --style=header,grid --line-range :100 {}"'
        else
            printf '"cat"'
        fi
    }

    # TODO: write own completion pattern -- how to works with zsh default completion?

    zinit ice depth=1 && zinit light mollifier/anyframe && \
        autoload -Uz anyframe-init && anyframe-init

    alias -g F='| fzf --border=rounded'
    alias -g Fp="| fzf --border=rounded --preview $(fzf_prev_command)"
    if is_available 'tmux' && is_tmux_newer_than '3.2' && [ ! -z ${TMUX} ]; then
        zstyle ":anyframe:selector:" use fzf-tmux
        zstyle ":anyframe:selector:fzf-tmux:" command 'fzf-tmux -h60% -w85%  --select-1'
    elif is_available 'tmux'; then
        zstyle ":anyframe:selector:" use fzf
        zstyle ":anyframe:selector:fzf:" command 'fzf --border=rounded --select-1'
    fi
    # enable anyframe binding with prefix ctrl+f in vi cmd mode
    bindkey -M vicmd '^fb'  anyframe-widget-cdr
    bindkey -M vicmd '^fr'  anyframe-widget-execute-history
    bindkey -M vicmd '^f^r' anyframe-widget-execute-history
    bindkey -M vicmd '^fi'  anyframe-widget-put-history
    bindkey -M vicmd '^f^i' anyframe-widget-put-history
    bindkey -M vicmd '^fk'  anyframe-widget-kill
    bindkey -M vicmd '^f^k' anyframe-widget-kill
    bindkey -M vicmd '^fs'  anyframe-widget-checkout-git-branch
    bindkey -M vicmd '^f^s' anyframe-widget-checkout-git-branch
    bindkey -M vicmd '^fe'  anyframe-widget-insert-git-branch
    bindkey -M vicmd '^f^e' anyframe-widget-insert-git-branch
    if is_available 'ghq'; then
        bindkey -M vicmd '^fg'  anyframe-widget-cd-ghq-repository
        bindkey -M vicmd '^f^g' anyframe-widget-cd-ghq-repository
    fi
fi

# ----------------------------------------------------------------------------------------
# enable colors
autoload -Uz colors && colors

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
autoload -Uz select-word-style

# ZLE zsh line editor
zstyle ':zle:*' word-chars " /=;@:{},|"
zstyle ':zle:*' word-style unspecified

# history configure ----------------------------------------------------------------------
HISTFILE="${ZDOTDIR}/.zsh_history"
HISTSIZE=1000000
SAVEHIST=1000000
setopt share_history
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt hist_reduce_blanks

# completions config ---------------------------------------------------------------------
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=gray,underline' # autosuggest color

# activate compinit
autoload -Uz compinit && compinit -u
zmodload -i zsh/complist

setopt always_last_prompt
setopt auto_menu
setopt auto_param_keys
setopt complete_in_word
setopt extended_glob
setopt globdots
setopt list_types # display file type
setopt magic_equal_subst # complete after equal sign
setopt mark_dirs # add '/' at the end of dir name in completion

zstyle ':completion:*' use-cache on # store cache to ${ZDOTDIR}/.zcompcache
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' # ignore cases
zstyle ':completion:*' ignore-parents parent pwd ..
zstyle ':completion:*' verbose yes
zstyle ':completion:*' completer \
       _expand _complete _match _prefix _approximate _list _history
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-separator '=>'
zstyle ':completion:*' list-dirs-first true
zstyle ':completion:*' list-prompt \
       '%K{blue} %k%F{blue}%U At %l (%p: TAB for more, or type char) %u%f'
zstyle ':completion:*' select-prompt \
       '%K{blue} %k%F{blue}%U At %l (%p: scroll active) %u%f'

zstyle ':completion:*:default' menu select=2 interactive
zstyle -e ':completion:*:default' list-colors \
       'reply=("${PREFIX:+=(#bi)($PREFIX:t)(?)*==34;04=35}:${(s.:.)LS_COLORS}")'
bindkey -M menuselect '^N' down-line-or-history
bindkey -M menuselect '^P' up-line-or-history
bindkey -M menuselect '^S' history-incremental-search-forward
bindkey -M menuselect '^J' accept-and-infer-next-history
bindkey -M menuselect '^H' backward-delete-char
bindkey -M menuselect '^D' delete-char
bindkey -M menuselect '^A' beginning-of-line
bindkey -M menuselect '^E' end-of-line
bindkey -M menuselect ' ' accept-search # when press space, explicitly quit menuselect mode

zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:messages' format '%F{yellow}%d%f'
zstyle ':completion:*:descriptions' format \
       "%K{green} %k%F{green}%U %d %u%f"
zstyle ':completion:*:warnings' format \
       "%K{red} %k%F{red}%U No matches %u%f %F{yellow}for: %d%f"
zstyle ':completion:*:corrections' format \
       "%K{red} %k%F{red}%U %d (errors: %e) %u%f"
zstyle ':completion:*:options' description 'yes'

zstyle ':completion:*:sudo:*' \
       command-path /sbin /usr/sbin \
       ennviron PATH="/sbin:/usr/sbin:$PATH"

# ps コマンドのプロセス名補完
zstyle ':completion:*:processes' command 'ps x -o pid,s,args'

# alias ----------------------------------------------------------------------------------
# define 'ls' default command
function alias_ls() {
    if is_available 'exa'; then
        alias ls="exa -F --color=always --icons"
        alias lss="exa -F --color=auto"
        alias ll="ls -l --time-style=long-iso --git"
        alias lla="ls -la --time-style=long-iso --git"
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

alias ...='cd ../..'
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias mkdir='mkdir -p'

# enable alias with sudo
is_available 'sudo' && \
    alias sudo='sudo '

# glob aliases
alias -g L='| less'
alias -g H='| head'
alias -g T='| tail'
alias -g G='| grep'

is_available 'fd' && \
    alias fd='fd --color=auto'
if is_available 'bat'; then
    alias -g B='| bat'
    alias -g Ba='| bat --show-all'
    alias -g Bap='| bat --show-all --plain'
fi
is_available 'duf' && \
    alias duf='duf -all -style=unicode -theme=dark'

if is_available 'rg'; then
    alias rg='rg --color=auto'
    alias -g R='| rg --color=auto'
fi

# clipboard manager
if [[ ${OSTYPE} == darwin* ]]; then
    is_available 'pbcopy' && alias -g C='| pbcopy'
elif is_available 'wl-copy'; then
    alias -g C='| wl-copy --trim-newline'
elif is_available 'xsel'; then
    alias -g C='| xsel --input --clipboard'
elif is_available 'putclip'; then
    alias -g C='| putclip'
fi

[[ ${OSTYPE} == linux* ]] && \
    is_available 'ip' && \
    alias ip='ip -color'

[[ ${OSTYPE} == darwin* ]] && \
    alias log-darwin='/usr/bin/log'

alias -g TC='COLORTERM=truecolor'

# ----------------------------------------------------------------------------------------
# define functions
# print os glyph from nerd fonts
function print_os_glyph() {
    case "$(uname)" in
        Darwin)
            if runs_on_macX86_64; then
                printf $'\UF302 \UF129'
            else
                printf $'\UF302'
            fi
            ;;
        Linux*)
            if [[ -e '/etc/os-release' ]]; then
                local distro_id=$(for l in $(<'/etc/os-release'); \
                                  do [[ "${l}" == ID* ]] && echo "${${l#'ID='}:Q:l}" \
                                         && break; done)
                case "${distro_id}" in
                    arch*) printf $'\UF303' ;;
                    centos*) printf $'\UF304' ;;
                    debian*) printf $'\UF306' ;;
                    fedora*) printf $'\UF30A' ;;
                    manjaro*) printf $'\UF312' ;;
                    opensuse*) printf $'\UF314' ;;
                    raspbian*) printf $'\UF315' ;;
                    sles*) printf $'\UF314' ;;
                    ubuntu*) printf $'\UF31C' ;;
                    steamos*) printf $'\UF1B6' ;;
                    # slackware) printf $'\UF318' ;;
                    # nixos) printf $'\UF313' ;;
                    # mint) printf $'\UF30F' ;;
                    # alpine) printf $'\UF300' ;;
                    # redhat) printf $'\UF316' ;;
                    *) printf $'\UF83C' ;;
                esac
            else
                printf $'\UF83C'
            fi
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
    curl -s "wttr.in/${target_loc}"\?"${days:=1n}qA&lang=${lang_:=ja}&format=${format:=}"
}
alias wttr='get_weather'

# launch emacs magit from shell
if is_available emacs; then
    function tdired() {
        emacsclient -nw --eval "(dired \"$1\")"
    }
    function dired() {
        emacsclient -c --eval "(dired \"$1\")" &
    }
fi

# ----------------------------------------------------------------------------------------
# config for libvt2
vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
# vterm-clear-scrollback
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi
# vterm-buffer-name-string
function _vterm_chpwd () { print -Pn "\e]2;%m:%2~\a"; }
add-zsh-hook -Uz chpwd _vterm_chpwd

# ----------------------------------------------------------------------------------------
# prompt configure
autoload -Uz promptinit && promptinit
setopt prompt_subst

function prompt_rdm46theme_setup() {
    local _isign=$'\UF10C'
    local _csign=$'\UF111'
    _vi_ins="%{${fg[blue]}%} ${_isign} %{${reset_color}%}"
    _vi_nor="%{${fg[green]}%} ${_csign} %{${reset_color}%}"
    _vi_vis="%{${fg[magenta]}%} ${_csign} %{${reset_color}%}"
    _vi_rep="%{${fg[yellow]}%} ${_isign} %{${reset_color}%}"
    # for zsh jeffreytse/zsh-vi-mode -----------------------------------------------------
    function zvm_after_select_vi_mode() {
        case $ZVM_MODE in
            $ZVM_MODE_NORMAL)
                vi_mode=${_vi_nor} ;;
            $ZVM_MODE_INSERT)
                vi_mode=${_vi_ins} ;;
            $ZVM_MODE_VISUAL)
                vi_mode=${_vi_vis} ;;
            $ZVM_MODE_VISUAL_LINE)
                vi_mode=${_vi_vis} ;;
            $ZVM_MODE_REPLACE)
                vi_mode=${_vi_rep} ;;
        esac
    }
    # for zsh builtin bindkey -v ---------------------------------------------------------
    # if zvm not loaded
    if [[ $(type zvm_init) == 'zvm_init not found' ]]; then
        vi_mode=${_vi_ins}
        function zle-keymap-select {
            vi_mode="${${KEYMAP/vicmd/${_vi_nor}}/(main|viins)/${_vi_ins}}"
            zle reset-prompt
        }
        zle -N zle-keymap-select
        function zle-line-finish { vi_mode=${_vi_ins}; }
        zle -N zle-line-finish
    fi
    local P_PROM=''
    # with nerd fonts
    local P_LOGIN=$'\UF2BD'
    local P_BEGINR=$'\UE0C7'
    local P_BEGIN=$'\UE0C6'
    local P_MIDTEX=$'\UE0C4'
    local P_END=$'\UE0C6'
    local P_DIR=$'\UF07C'
    local P_CLOCK=$'\UF017'
    local P_CONN=$'\UF438'
    [ ! -z ${SSH_CONNECTION} ] && \
        local P_CONN_ST="${fg[red]}${P_CONN}[${fg[black]}${${SSH_CONNECTION%%\ *}/\%/@}${fg[red]}] "

    PROMPT="\
%{${bg[green]}%}%{${fg[red]}%} ${P_LOGIN}%{${reset_color}%}\
%{${bg[green]}%}%{${fg_bold[black]}%} %n %{${reset_color}%}\
\
%{${bg[blue]}%}%{${fg[green]}%}${P_MIDTEX}%{${reset_color}%}\
%{${bg[blue]}%}%{${fg[red]}%} $(print_os_glyph)%{${reset_color}%}\
%{${bg[blue]}%}%{${fg_bold[black]}%} %m ${P_CONN_ST:=}%{${reset_color}%}\
%{${bg[black]}%}%{${fg[blue]}%}${P_BEGIN}%{${reset_color}%}\
%{${fg[black]}%}${P_MIDTEX}%{${reset_color}%}
\
%{${bg[black]}%}%{${fg[green]}%} ${P_CLOCK}%{${reset_color}%}\
%{${bg[black]}%}%{${fg[brblack]}%} %D %T%{${reset_color}%}\
%{${bg[black]}%}%{${fg[green]}%} ${P_DIR}%{${reset_color}%}\
%{${bg[black]}%}%{${fg[brblack]}%} %~  %{${reset_color}%}\
%{${fg[black]}%}${P_MIDTEX}%{${reset_color}%}
\
%{${fg[black]}%}${P_PROM}%{${reset_color}%}"'${vi_mode}'\
'%(?.%{${fg[brblack]}%}.%{${fg[red]}%})%#%{${reset_color}%} '

    PROMPT2="\
%{${bg[black]}%}%{${fg[green]}%} %_ >%{${reset_color}%}\
%{${fg[black]}%}${P_MIDTEX}%{${reset_color}%} "
}

case "$TERM" in
    dumb | emacs)
        PROMPT="%m:%~> "
        unsetopt zle
        ;;
    *) # if $TERM seems not emacs, use theme
        prompt_themes+=( rdm46theme ) &&  prompt rdm46theme
        ;;
esac

# fix prompt when C-c in normal state (does not work if included in theme function)
function TRAPINT() {
    vi_mode=${_vi_ins}
    return $(( 128 + $1 ))
}

# ----------------------------------------------------------------------------------------
# vcs info TODO: include vcs prompt to theme
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

function _update_vcs_info() {
    LANG=en_US.UTF-8 vcs_info
    RPROMPT="${vcs_info_msg_0_}"
}
add-zsh-hook precmd _update_vcs_info


# change tmux sessison default directory
function t-cd() {
    tmux command-prompt -I "${${1}:-${PWD}}"\
        -p "Session dir to:"\
        "attach-session -t . -c %1"
 }

# ----------------------------------------------------------------------------------------
#http://qiita.com/b4b4r07/items/01359e8a3066d1c37edc
function is_osx() { [[ $OSTYPE == darwin* ]]; }
function is_screen_running() { [ ! -z "$STY" ]; }
function is_tmux_running() { [ ! -z "$TMUX" ]; }
function is_screen_or_tmux_running() { is_screen_running || is_tmux_running; }
function shell_has_started_interactively() { [ ! -z "$PS1" ]; }
function is_ssh_running() { [ ! -z "$SSH_CONECTION" ]; }

# detect if tmux is running and attach
function tmux_automatically_attach_session()
{
    if is_screen_or_tmux_running; then
        ! is_available 'tmux' && return 1

        if is_tmux_running; then
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
