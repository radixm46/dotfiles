SHELLENV:=.zshrc .zshenv .tmux.conf

EMACS:=.emacs.d

SWAYENV:=mako sway waybar swaylock
# switch waybar config mobile or desktop

NVIM:=nvim

TERMINALS:=termite alacritty

MISC:=mpv

CFG_TARGET:=$(shell if [ ! -z ${XDG_CONFIG} ]; \
    then printf ${XDG_CONFIG}'\n'; \
    else printf ${HOME}/.config'\n'; \
    fi)

BACKUP_TARGET=${HOME}/dotfiles_backup

.PHONY: install
install:
	@echo 'execute "make link-all" or "make link-core" instead install'

link-all: check-env link-vim link-neovim link-swayenv link-emacs link-shellenv \
    link-terminals link-misc

link-all-core: check-env link-vim link-neovim link-emacs link-shellenv

check-env:
	@printf 'Current config target directory:\n\t$(CFG_TARGET)\n'
	@if [ ! -e $(CFG_TARGET) ]; then \
	    printf '\t target unavailable generating...\n'; \
	    mkdir -p $(CFG_TARGET); \
	    fi # should read from user

check: check-env check-vim check-neovim check-swayenv check-emacs \
    check-shellenv check-terminals check-misc

#backup-all: # backup

# vim config ------------------------------------------------------------------
check-vim:
	@bin/make_chkavailable.sh  "${HOME}/.vimrc"
	@bin/make_chkavailable.sh  "${HOME}/.vim"

backup-vim:
	@bin/make_backup.sh "target"

link-vim: #to vim check target before execute
	@ln -fsv "${PWD}/config/${NVIM}/init.vim" "${HOME}/.vimrc"
	@ln -nfsv "${PWD}/config/${NVIM}" "${HOME}/.vim"

# neovim config ----------------------------------------------------------------
check-neovim:
	@bin/make_chkavailable.sh $(CFG_TARGET)/$(NVIM)

link-neovim:
	@ln -nfsv "${PWD}/config/$(NVIM)" "$(CFG_TARGET)/$(NVIM)"
	@# launch nvim backgound and setup nvim -es -v init.vim?

# swayenv config ---------------------------------------------------------------
check-swayenv:
	@for cfgfile in $(SWAYENV); \
	do \
	    bin/make_chkavailable.sh "$(CFG_TARGET)/$${cfgfile}"; \
	done

link-swayenv:
	@for cfgfile in $(SWAYENV); \
	do \
	    ln -nfsv "${PWD}/config/$${cfgfile}" "$(CFG_TARGET)/$${cfgfile}"; \
	done

# emacs config ----------------------------------------------------------------
check-emacs:
	@bin/make_chkavailable.sh "${HOME}/.emacs.d"

link-emacs:
	@ln -nfsv "${PWD}/${EMACS}" "${HOME}/.emacs.d"

# shellenv config --------------------------------------------------------------
check-shellenv:
	@for cfgfile in $(SHELLENV); \
	do \
	    bin/make_chkavailable.sh  "${HOME}/$${cfgfile}"; \
	done

link-shellenv:
	@for cfgfile in $(SHELLENV); \
	do \
	    ln -svf "${PWD}/$${cfgfile}" "${HOME}/$${cfgfile}"; \
	done

# terminals config -------------------------------------------------------------
check-terminals:
	@for cfgfile in $(TERMINALS); \
	do \
	    bin/make_chkavailable.sh "$(CFG_TARGET)/$${cfgfile}"; \
	done

link-terminals:
	@for cfgfile in $(TERMINALS); \
	do \
	    ln -nfsv "${PWD}/config/$${cfgfile}" "$(CFG_TARGET)/$${cfgfile}"; \
	done

# misc config -----------------------------------------------------------------
check-misc:
	@for cfgfile in $(MISC); \
	do \
	    bin/make_chkavailable.sh "$(CFG_TARGET)/$${cfgfile}"; \
	done

link-misc:
	@for cfgfile in $(MISC); \
	do \
	    ln -nsvf "${PWD}/config/$${cfgfile}" "$(CFG_TARGET)/$${cfgfile}"; \
	done


# not finished
TEST:=test
test-hoge: $(addprefix $(HOME)/,$(SWAYENV))
test-hoge2: $(addprefix $(CFG_TARGET)/,$(SHELLENV))

$(CFG_TARGET)/%: $%
	@echo "ln -nsv $(PWD)/config/$(@F) $@"

$(BACKUP_TARGET)/%: %
	@echo "backup"

$(HOME)/%:
	@echo "ln -nsv $(PWD)/$(@F)  $@"
	@#ln -sv "${PWD}/config/${NVIM}/Init.vim" "${HOME}/.vimrc"
