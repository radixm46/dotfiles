setopt no_global_rcs #disable path helper on osx


case "$(uname)" in

    Darwin) # OSがMacならば
        export PATH="/usr/sbin:/sbin:/usr/local/bin:$PATH" #add path for homebrew

        if [[ -d /Applications/MacVim.app ]]; then # MacVimが存在するならば
            alias vim=/Applications/MacVim.app/Contents/MacOS/Vim
            alias vi=vim
        fi

        ;;

esac

if [[ -e $HOME/.profile ]]; then
    source $HOME/.profile  # load .profile if exists
#
# if [[ -d $HOME/miniconda3/bin ]]; then
#     export PATH="$HOME/miniconda3/bin:$PATH"
# fi #add anaconda path if installed
# export LANGUAGE=en_US.UTF-8
# export LC_ALL=en_US.UTF-8
# export LC_CTYPE=en_US.UTF-8
# export LANG=en_US.UTF-8
#added by Anaconda 4.4.0 installer
