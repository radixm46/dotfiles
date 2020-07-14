# 少し凝った zshrc(改)
# License : MIT
# http://mollifier.mit-license.org/

########################################
# 環境変数
export LANG=ja_JP.UTF-8
export XDG_CONFIG_HOME=~/.config

# 色を使用出来るようにする
autoload -Uz colors
colors

# vim 風キーバインドにする
bindkey -v

# 10ms for key sequences
KEYTIMEOUT=1

# ヒストリの設定
HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000

# プロンプト
# 1行表示
# PROMPT="%~ %# "
# P_LOGIN=$'\U23FB'
# P_COMPUTER=$'\U1F5B5 '
#P_BEGIN=$'\U2599\U259E\U259A\U2596\U259D\U2598'
# P_BEGIN=$'\U1F67E  \U2596'
# P_BEGINR=$'\U2596  \U1F67E'
# P_MIDTEX=$'\U2593\U2592\U2591'
# P_MIDTEXR=$'\U2591\U2592\U2593'
# P_DIR=$'\U1F5BF' #$'\U1F5BF'
#P_END=$'\U2599\U259E\U259A\U2596\U259D\U2598'
# P_END=$'\U1F67F \U259D'
# P_ENDR=$'\U2598\U2597 \U1F67F'
P_PROM='' #$'\U259B\U259F'
# P_GITBRANCH=$'\UE0A0'
# P_VCSICO=$'\U1F5D8'

# with nerd fonts
P_LOGIN=$'\UF2C0'
case $(uname) in
    Darwin)
        P_COMPUTER=$'\UF179'
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

# 単語の区切り文字を指定する
autoload -Uz select-word-style
select-word-style default
# ここで指定した文字は単語区切りとみなされる
# / も区切りと扱うので、^W でディレクトリ１つ分を削除できる
zstyle ':zle:*' word-chars " /=;@:{},|"
zstyle ':zle:*' word-style unspecified

########################################
# 補完
# 補完機能を有効にする

#for zsh-completions
fpath=(/usr/local/share/zsh-completions $fpath)

autoload -Uz compinit
compinit -u

# 補完で小文字でも大文字にマッチさせる
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# ../ の後は今いるディレクトリを補完しない
zstyle ':completion:*' ignore-parents parent pwd ..

# sudo の後ろでコマンド名を補完する
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
                   /usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin

# ps コマンドのプロセス名補完
zstyle ':completion:*:processes' command 'ps x -o pid,s,args'


########################################
# vcs_info
autoload -Uz vcs_info
autoload -Uz add-zsh-hook

zstyle ':vcs_info:*' formats \
"%F{black}${P_MIDTEXR}\
%K{black}%F{green} ${P_VCSICO} %F{brblack}%s %F{magenta}${P_ENDR}\
%{${bg[magenta]}%}%{${fg[white]}%} ${P_GITBRANCH}%{${fg[white]}%} %b %f%k%{${reset_color}%}"
#%K{magenta} %F{white}${P_GITBRANCH} %F{black}%b %f%k"
#zstyle ':vcs_info:*' actionformats '%F{red}(%s)-[%b|%a]%f'
zstyle ':vcs_info:*' actionformats \
"%F{black}${P_MIDTEXR}\
%K{black}%F{green} ${P_VCSICO} %F{brblack}%s %F{magenta}${P_ENDR}\
%{${bg[magenta]}%}%{${fg[white]}%} ${P_GITBRANCH}%{${fg[white]}%} %b %a %f%k%{${reset_color}%}"

function _update_vcs_info_msg() {
    LANG=en_US.UTF-8 vcs_info
    RPROMPT="${vcs_info_msg_0_}"
}
add-zsh-hook precmd _update_vcs_info_msg


########################################
# オプション
# 日本語ファイル名を表示可能にする
setopt print_eight_bit

# beep を無効にする
setopt no_beep

# フローコントロールを無効にする
setopt no_flow_control

# Ctrl+Dでzshを終了しない
setopt ignore_eof

# '#' 以降をコメントとして扱う
setopt interactive_comments

# ディレクトリ名だけでcdする
setopt auto_cd

# cd したら自動的にpushdする
setopt auto_pushd

# 重複したディレクトリを追加しない
setopt pushd_ignore_dups

# 同時に起動したzshの間でヒストリを共有する
setopt share_history

# 同じコマンドをヒストリに残さない
setopt hist_ignore_all_dups

# スペースから始まるコマンド行はヒストリに残さない
setopt hist_ignore_space

# ヒストリに保存するときに余分なスペースを削除する
setopt hist_reduce_blanks

# 高機能なワイルドカード展開を使用する
setopt extended_glob

########################################
# zplugin
########################################
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
#zplugin light zsh-users/zsh-completions
zplugin light zdharma/fast-syntax-highlighting

# autoload -U compinit
# (compinit -u &)


########################################
# キーバインド

# ^R で履歴検索をするときに * でワイルドカードを使用出来るようにする
bindkey '^R' history-incremental-pattern-search-backward

########################################
# alias

function is_available() { hash "$1" >/dev/null 2>&1; return $?; }

if is_available exa; then
    LSCOM="exa"
else
    LSCOM="ls"
fi

case ${OSTYPE} in
    darwin*)
        export CLICOLOR=1
        alias ls='ls -G -F'
        ;;
    linux*)
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

eval `tset -s xterm-24bits`

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
