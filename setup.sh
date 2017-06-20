#RED = '\033[0;31m'
#NC = '\033[0m'
 DOT_FILES=(.vimrc .vim .tmux.conf .zshrc .zshenv .tmux)

for file in ${DOT_FILES[@]}
do
    ln -s $HOME/dotfiles/$file $HOME/$file
done

#Check Dein
if [ -e ~/.vim/dein/repos/github.com/Shougo/dein.vim ]; then
    #echo -e "${RED}dein installed!!!${NC}"
    echo "dein installed!!!"
else
    echo "dein not installed!!"
    #gitクローンでdeinインストールを行う
    mkdir -p ~/.vim/dein/repos/github.com/Shougo/dein.vim \
    git clone https://github.com/Shougo/dein.vim.git \
            ~/.vim/dein/respos/github.com/Shougo/dein.vim
fi


#todo
#dein有無で色付け
#zsh、tmux、vimの自動インストールなど
#OS毎にセットアップファイルを切り替える
