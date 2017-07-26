setopt no_global_rcs #disable path helper on osx

case "$(uname)" in

    Darwin) # OSがMacならば
        if [[ -d /Applications/MacVim.app ]]; then # MacVimが存在するならば
            alias vim=/Applications/MacVim.app/Contents/MacOS/Vim
            alias vi=vim
            export PATH="/usr/sbin:/sbin:/usr/local/bin:$PATH" #add path for homebrew
            export PATH="/Users/MotohiroIto/anaconda/bin:$PATH"
            #export PATH="$PYENV_ROOT/bin:$PATH"
            #export PYENV_ROOT="$HOME/.pyenv"
            #eval "$(pyenv init -)" #pyenvもdotfilesで管理、setupで自動で環境つくれるようにとか？
        fi
        ;;
        
    *) ;; # OSがMac以外ならば何もしない
esac

export LANGUAGE=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LANG=en_US.UTF-8
#added by Anaconda 4.4.0 installer
