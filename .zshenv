case "$(uname)" in

    Darwin) # OSがMacならば
        if [[ -d /Applications/MacVim.app ]]; then # MacVimが存在するならば
            alias vim=/Applications/MacVim.app/Contents/MacOS/Vim
            alias vi=vim
            export PYENV_ROOT="$HOME/.pyenv"
            export PATH="$PYENV_ROOT/bin:$PATH"
            eval "$(pyenv init -)" #pyenvもdotfilesで管理、setupで自動で環境つくれるようにとか？
        fi
        ;;
        
    *) ;; # OSがMac以外ならば何もしない
esac
