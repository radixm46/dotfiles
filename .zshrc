# compile when update
[ ! -f "${HOME}/.zshrc.zwc" -o "${HOME}/.zshrc" -nt "${HOME}/.zshrc.zwc" ] && {
    [[ -o interactive ]] && printf 'compile zshrc...'
    zcompile "${HOME}/.zshrc"
    [[ -o interactive ]] && printf 'done!\n'
}
# ----------------------------------------------------------------------------------------
# zinit install
# ----------------------------------------------------------------------------------------
ZDOTDIR="${XDG_CONFIG_HOME}/zsh"
ZINITDIR="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"

# if NO_ZINIT is set, avoid to load zinit and plugins
[ -z $NO_ZINIT ] && {
    [[ ! -d ${ZINITDIR} ]] && {
    print -P "%F{33}▓▒░ %F{220}Installing DHARMA Initiative Plugin Manager (zdharma-continuum/zinit)…%f"
    command mkdir -p "$(dirname $ZINITDIR)" && command chmod g-rwX $(dirname ${ZINITDIR})
    command git clone https://github.com/zdharma-continuum/zinit.git "${ZINITDIR}" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
            print -P "%F{160}▓▒░ The clone has failed.%f%b"
    }
    source "${ZINITDIR}/zinit.zsh"
}
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
         is-at-least \
         chpwd_recent_dirs \
         cdr \
         add-zsh-hook \
         vcs_info \
         colors &&
    colors
add-zsh-hook chpwd chpwd_recent_dirs

# keybinds -------------------------------------------------------------------------------
bindkey -v  # use vim style keybinding
# 10ms for key sequences
export KEYTIMEOUT=1

# zvm ------------------------------------------------------------------------------------
# init immediately after sourced
ZVM_INIT_MODE=sourcing
zinit ice depth=1 && zinit light jeffreytse/zsh-vi-mode && {
        ZVM_INSERT_MODE_CURSOR=$ZVM_CURSOR_BLOCK
        ZVM_NORMAL_MODE_CURSOR=$ZVM_CURSOR_BLOCK
        ZVM_LINE_INIT_MODE=$ZVM_MODE_INSERT
        function() { # remap after zvm load
            bindkey -M vicmd '^R' history-incremental-pattern-search-backward
            bindkey -M viins '^F' forward-char
            bindkey -M viins '^B' backward-char
            bindkey -M viins '^I' menu-expand-or-complete
            bindkey -M viins '^H' backward-delete-char
            bindkey -M viins '^D' delete-char
            bindkey -M viins '^A' beginning-of-line
            bindkey -M viins '^E' end-of-line
            bindkey -M viins '^N' expand-or-complete
            bindkey -M viins '^P' reverse-menu-complete
        }
    }
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

# alias reminder -------------------------------------------------------------------------
zinit ice depth=1 && zinit light MichaelAquilina/zsh-you-should-use && function() {
        local underline=$'\e[4m'
        export YSU_MESSAGE_FORMAT="${bg[green]} ${reset_color}\
${fg[green]}${underline} Alias found! ${reset_color}  \
${fg_bold[magenta]}\"%command\"${reset_color}${fg[green]} -(%alias_type)-> \
${fg_bold[blue]}\"%alias\"${reset_color}"
    }

# plugins depends on external commands ---------------------------------------------------
zinit ice has'emacs' depth=1 && zinit light Flinner/zsh-emacs # alias
zinit ice has'systemctl' depth=1 && zinit light le0me55i/zsh-systemd # alias
zinit ice has'docker' depth=1 && zinit light akarzim/zsh-docker-aliases #alias

is_available 'git' && {
    zinit ice depth=1 && zinit light mdumitru/git-aliases # alias
    zinit ice depth=1 && zinit light paulirish/git-open # open homepage of repo
    zinit ice depth=1 && zinit light mollifier/cd-gitroot # move to root dir of repo
}

is_available 'fzf' && {
    zinit ice has'jq' depth=1 && zinit light reegnz/jq-zsh-plugin
    zinit ice has'git' depth=1 && zinit light wfxr/forgit

    # page-up page-down temporary binded like emacs
    # NOTE: OneDark theme with bg+border
    export FZF_DEFAULT_OPTS="--multi --cycle --ansi --reverse --marker=* \
 --border=bold --color=dark --color=bg:#282c34,border:#cd00cd \
 --color=fg:-1,hl:#c678dd,fg+:#ffffff,bg+:#4b5263,hl+:#d858fe \
 --color=info:#98c379,prompt:#61afef,pointer:#be5046,marker:#e5c07b,spinner:#61afef,header:#61afef \
 --prompt='> ' --marker='❚' --pointer='▶' \
 --bind 'ctrl-d:page-down' --bind 'ctrl-u:page-up' --bind 'alt-d:preview-page-down' --bind 'alt-u:preview-page-up'"
    fzf_prev_opts="$(
        if is_available 'bat'; then
            printf 'bat --color=always --theme=OneHalfDark --style=header,grid --line-range :100 {}'
        else
            printf '"cat"'
        fi)"

    # TODO: write own completion pattern -- how to works with zsh default completion?

    alias -g F='| fzf --border=rounded'
    alias -g Fp='| fzf --border=rounded --preview=${fzf_prev_opts}'


    is-at-least 0.48 $(fzf --version) &&
        FZF_CTRL_T_COMMAND='' source <(fzf --zsh) # 0.48以降必須

    zinit ice depth=1 && zinit light mollifier/anyframe &&
        autoload -Uz anyframe-init && {
            anyframe-init
            if is_available 'tmux' && is-at-least "$(tmux -V)" '3.2' && [ ! -z "${TMUX}" ]; then
                zstyle ":anyframe:selector:" use fzf-tmux
                zstyle ":anyframe:selector:fzf-tmux:" command 'fzf-tmux -h60% -w85% -- --select-1'
            elif is_available 'tmux'; then
                zstyle ":anyframe:selector:" use fzf
                zstyle ":anyframe:selector:fzf:" command 'fzf --border=rounded --select-1'
                is_available 'tldr' &&
                    function tldr_() {
                        local prev=$( is_available 'mdcat' &&  printf '|mdcat -P')
                        tldr $(tldr --list |
                                   fzf -q "${1=}" --select-1 --border=rounded  --preview "tldr --raw {} ${prev}"
                            ) 2>/dev/null ||
                            printf 'require valid arg\n'
                    }
            fi
            # enable anyframe binding with prefix ctrl+f in vi cmd mode
            bindkey -M vicmd '^fb'  anyframe-widget-cdr
            bindkey -M vicmd '^fs'  anyframe-widget-checkout-git-branch
            bindkey -M vicmd '^f^s' anyframe-widget-checkout-git-branch
            bindkey -M vicmd '^fe'  anyframe-widget-insert-git-branch
            bindkey -M vicmd '^f^e' anyframe-widget-insert-git-branch
            is_available 'ghq' && {
                bindkey -M vicmd '^fg'  anyframe-widget-cd-ghq-repository
                bindkey -M vicmd '^f^g' anyframe-widget-cd-ghq-repository
            }
        }
}

# ----------------------------------------------------------------------------------------
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
bindkey -M menuselect '^I' down-line-or-history
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
alias ..2='cd ../..'
alias ..3='cd ../../..'
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias mkdir='mkdir -p'

# glob aliases
alias -g L='| less'
alias -g H='| head'
alias -g T='| tail'
alias -g G='| grep'

# define 'ls' alias
function() {
    local lscmd="ls"
    for c in eza exa; do
        hash "$c" 2>/dev/null && {
            lscmd="$c"
            break
        }
    done
    if [ ! "$lscmd" = "ls" ]; then
        alias ls="$lscmd -F --color=always --icons"
        alias lss="$lscmd -F --color=auto"
        alias ll='ls -l --time-style=long-iso --git'
        alias lla='ls -la --time-style=long-iso --git'
        alias la='ls -a'
    else
        case ${OSTYPE} in
            darwin*)
                export CLICOLOR=1
                alias ls='ls -hGF'
                ;;
            linux*)
                alias ls='ls -F --color=auto'
                ;;
            *) ;;
        esac
        alias la='ls -a'
        alias ll='ls -l'
    fi
}

# enable alias with sudo
is_available 'sudo' && \
    alias sudo='sudo '

is_available 'fd' && \
    alias fd='fd --color=auto'
is_available 'bat' && {
    alias -g B='| bat'
    alias -g Ba='| bat --show-all'
    alias -g Bap='| bat --show-all --plain'
}

is_available 'duf' && \
    alias duf='duf -all -style=unicode -theme=dark'

is_available 'rg' && {
    alias rg='rg --color=auto'
    alias -g R='| rg --color=auto'
}

case ${OSTYPE} in
    darwin* )
        alias log-darwin='/usr/bin/log'
        is_available 'pbcopy' && alias -g C='| pbcopy'
        ;;
    linux* )
        is_available 'ip' && \
            alias ip='ip -color'

        # clipboard manager
        if is_available 'wl-copy'; then
            alias -g C='| wl-copy --trim-newline'
        elif is_available 'xsel'; then
            alias -g C='| xsel --input --clipboard'
        elif is_available 'putclip'; then
            alias -g C='| putclip'
        fi
        ;;
    *) ;;
esac

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
                    arch*)     printf $'\UF303' ;;
                    centos*)   printf $'\UF304' ;;
                    debian*)   printf $'\UF306' ;;
                    fedora*)   printf $'\UF30A' ;;
                    manjaro*)  printf $'\UF312' ;;
                    opensuse*) printf $'\UF314' ;;
                    raspbian*) printf $'\UF315' ;;
                    sles*)     printf $'\UF314' ;;
                    ubuntu*)   printf $'\UF31C' ;;
                    steamos*)  printf $'\UF1B6' ;;
                    kali*)     printf $'\UF327' ;;
                    ende*)     printf $'\UF322' ;;
                    # slackware) printf $'\UF318' ;;
                    # nixos) printf $'\UF313' ;;
                    # mint) printf $'\UF30F' ;;
                    # redhat) printf $'\UF316' ;;
                    *) printf $'\UF31A' ;;
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
is_available emacs && {
    function tdired() {
        emacsclient -nw --eval "(dired \"$1\")"
    }
    function dired() {
        emacsclient -c --eval "(dired \"$1\")" &
    }
}

# ----------------------------------------------------------------------------------------
# config for libvt2
function is_emacs_vterm_running() { [ ! -z "$EMACS_VTERM_PATH" ]; }
is_emacs_vterm_running && {
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
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
    # vterm-buffer-name-string
    function _vterm_chpwd () { print -Pn "\e]2;%m:%2~\a"; }
    add-zsh-hook -Uz chpwd _vterm_chpwd && _vterm_chpwd
}

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
    function rdm_zvm_update_indicator() {
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
    zvm_after_select_vi_mode_commands+=(rdm_zvm_update_indicator)
    # for zsh builtin bindkey -v ---------------------------------------------------------
    # if zvm not loaded
    [[ $(type zvm_init) == 'zvm_init not found' ]] && {
        vi_mode=${_vi_ins}
        function zle-keymap-select {
            vi_mode="${${KEYMAP/vicmd/${_vi_nor}}/(main|viins)/${_vi_ins}}"
            zle reset-prompt
        }
        zle -N zle-keymap-select
        function zle-line-finish { vi_mode=${_vi_ins}; }
        zle -N zle-line-finish
    }
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
    [ ! -z ${SSH_CONNECTION} ] && local P_CONN_ST="\
${fg[red]}${P_CONN}[${fg[black]}${${SSH_CONNECTION%%\ *}/\%/@}${fg[red]}] "

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
"%(?.%{${fg[brblack]}%}.%{${fg[red]}%})%#%{${reset_color}%} "

    PROMPT2="\
%{${bg[black]}%}%{${fg[green]}%} %_ >%{${reset_color}%}\
%{${fg[black]}%}${P_MIDTEX}%{${reset_color}%} "

    # NOTE: initialize zvm indicator if zvm available on load
    [[ $(type zvm_init) != 'zvm_init not found' ]] && rdm_zvm_update_indicator
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
function() {
    local P_GITBRANCH=$'\UF418'
    local P_VCSICO=$'\UE702'
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
function _update_vcs_info() {
    LANG=en_US.UTF-8 vcs_info
    RPROMPT="${vcs_info_msg_0_}"
}
add-zsh-hook precmd _update_vcs_info


# ----------------------------------------------------------------------------------------
# based on http://qiita.com/b4b4r07/items/01359e8a3066d1c37edc
function is_osx() { [[ $OSTYPE == darwin* ]]; }
function is_screen_running() { [ ! -z "$STY" ]; }
function is_tmux_running() { [ ! -z "$TMUX" ]; }
function is_screen_or_tmux_running() { is_screen_running || is_tmux_running; }
function shell_has_started_interactively() { [ ! -z "$PS1" ]; }
function is_ssh_running() { [ ! -z "$SSH_CONECTION" ]; }

# detect if tmux is running and attach
function() {
    # check tmux or screen available, if not, abort
    ! { is_available 'tmux' || is_available 'screen' } && return 1

    if is_screen_or_tmux_running; then
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
            # if tmux already started
            [ $(tmux list-sessions 2>/dev/null | wc -l) -gt 0 ] && {
                # if emacs vterm running, try to attach session 'vterm'
                is_emacs_vterm_running && {
                    echo "detect emacs vterm!"
                    echo -n "attach / create 'vterm' session? (Yy/n/[N]o tmux): "
                    read
                    case "$REPLY" in
                        [Yy] | "")
                            tmux new-session -A -s "vterm"
                            return 0
                            ;;
                        n)
                            echo "> Trying to attach..."
                            # to next
                            ;;
                        N)
                            echo "> Starts without tmux..."
                            return 0
                            ;;
                        *)
                            echo "> invalid input"
                            ;;
                    esac
                }

                tmux list-sessions
                echo -n "Tmux: attach? (Yy/[n]o/[N]ew/session-name): "
                read
                case "$REPLY" in
                    [Yy] | "")
                        tmux attach-session
                        [ $? -eq 0 ] && {
                            echo "$(tmux -V) attached session"
                            return 0
                        }
                        ;;
                    n)
                        echo "> starts without tmux"
                        return 0
                        ;;
                    N)
                        tmux new-session && echo "> starts new session"
                        return 0
                        ;;
                    *)
                        tmux new-session -A "$REPLY"
                        if [ $? -eq 0 ]; then
                            echo "$(tmux -V) attached session"
                            return 0
                        else
                            echo "> invalid session name!"
                            return 1
                        fi
                        ;;
                esac
            }
            tmux new-session && echo "tmux created new session"
        fi
    fi
}
