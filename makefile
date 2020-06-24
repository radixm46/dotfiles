shellenv=.zshrc .zshenv .tmux.conf

emacs=.emacs.d

swayenv=mako sway waybar swaylock
# switch waybar config mobile or desktop

nvim=nvim

terminals=termite alacritty

misc=mpv

cfg_target=$(shell if [ ! -z ${XDG_CONFIG} ]; \
    then printf ${XDG_CONFIG}'\n'; \
    else printf ${HOME}/.config'\n'; \
    fi)

backup_target=${PWD}/backup

help:
	@echo 'makefile for setup dotfiles'

link-all: check-env \
    link-vim link-neovim link-swayenv link-emacs link-shellenv link-terminals \
    link-misc

link-all-mac: check-env \
    link-vim link-neovim link-emacs link-shellenv

check-env:
	@printf 'current config dir target: $(cfg_target)\n'
	@if [ ! -e $(cfg_target) ]; then \
	    printf '\t target unavailable generating...\n' \
	    mkdir -p $(cfg_target); \
	    fi # should read from user

link-vim: #to vim check target before execute
	@ln -fsv "${PWD}/config/${nvim}/init.vim" "${HOME}/.vimrc"
	@ln -nfsv "${PWD}/config/${nvim}" "${HOME}/.vim"

link-neovim:
	@ln -nfsv "${PWD}/config/$(nvim)" "$(cfg_target)/$(nvim)"
	@# launch nvim backgound and setup nvim -es -v init.vim?

link-swayenv:
	@for cfgfile in $(swayenv); \
	do \
	    ln -nfsv "${PWD}/config/$${cfgfile}" "$(cfg_target)/$${cfgfile}"; \
	done

link-emacs:
	@ln -nfsv "${PWD}/${emacs}" "${HOME}/.emacs.d"
	@# launch emacs here and install fonts, skk-dict, etc

link-shellenv:
	@for cfgfile in $(shellenv); \
	do \
	    ln -svf "${PWD}/$${cfgfile}" "${HOME}/$${cfgfile}"; \
	done

link-terminals:
	@for cfgfile in $(terminals); \
	do \
	    ln -nfsv "${PWD}/config/$${cfgfile}" "$(cfg_target)/$${cfgfile}"; \
	done

link-misc:
	@for cfgfile in $(misc); \
	do \
	    ln -svf "${PWD}/config/$${cfgfile}" "$(cfg_target)/$${cfgfile}"; \
	done
