SHELLENV:=.zshrc .zshenv .tmux.conf

SHELLTOOLS:=tig

EMACS:=.emacs.d

SWAYENV:=mako sway waybar swaylock

PAMENV:=.pam_environment

NVIM:=nvim

TERMINALS:=termite alacritty

MISC:=mpv

CFG_TARGET:=$(shell if [ ! -z ${XDG_CONFIG} ]; \
    then printf ${XDG_CONFIG}'\n'; \
    else printf ${HOME}/.config'\n'; \
    fi)

BACKUP_TARGET=${HOME}/dotfiles_backup

CHK_TARGET:=$(PWD)/bin/make_chkfile.sh

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

check: check-env check-vim check-neovim check-swayenv check-pamenv \
       check-emacs check-shellenv check-terminals check-misc

#backup-all: # backup

# vim config ------------------------------------------------------------------
check-vim:
	@$(CHK_TARGET) "${HOME}/.vimrc"
	@$(CHK_TARGET) "${HOME}/.vim"

link-vim:
	@ln -sv "${PWD}/config/${NVIM}/init.vim" "${HOME}/.vimrc"
	@ln -nsv "${PWD}/config/${NVIM}" "${HOME}/.vim"

# neovim config ----------------------------------------------------------------
check-neovim:
	@for cfgfile in $(NVIM); \
	do $(CHK_TARGET) "$(CFG_TARGET)/$${cfgfile}"; \
	done

link-neovim: $(addprefix $(CFG_TARGET)/,$(NVIM))
	@# launch nvim backgound and setup nvim -es -v init.vim?

# swayenv config ---------------------------------------------------------------
check-swayenv: check-pamenv
	@for cfgfile in $(SWAYENV); \
	do $(CHK_TARGET) "$(CFG_TARGET)/$${cfgfile}"; \
	done

link-swayenv: link-pamenv $(addprefix $(CFG_TARGET)/,$(SWAYENV))

check-pamenv:
	@for cfgfile in $(PAMENV); \
	do $(CHK_TARGET) "$(HOME)/$${cfgfile}"; \
	done

link-pamenv: $(addprefix $(HOME)/,$(PAMENV))

# emacs config ----------------------------------------------------------------
check-emacs:
	@for cfgfile in $(EMACS); \
	do $(CHK_TARGET) "$(HOME)/$${cfgfile}"; \
	done

link-emacs: $(addprefix $(HOME)/,$(EMACS))

# shellenv config --------------------------------------------------------------
check-shellenv:
	@for cfgfile in $(SHELLENV); \
	do $(CHK_TARGET) "$(HOME)/$${cfgfile}"; \
	done

link-shellenv: $(addprefix $(HOME)/,$(SHELLENV))

# shelltools config ------------------------------------------------------------

check-shelltools:
	@for cfgfile in $(SHELLTOOLS); \
	do $(CHK_TARGET) "$(CFG_TARGET)/$${cfgfile}"; \
	done

link-shelltools: $(addprefix $(CFG_TARGET)/,$(SHELLTOOLS))

# terminals config -------------------------------------------------------------
check-terminals:
	@for cfgfile in $(TERMINALS); \
	do $(CHK_TARGET) "$(CFG_TARGET)/$${cfgfile}"; \
	done

link-terminals: $(addprefix $(CFG_TARGET)/,$(TERMINALS))

# misc config -----------------------------------------------------------------
check-misc:
	@for cfgfile in $(MISC); \
	do $(CHK_TARGET) "$(CFG_TARGET)/$${cfgfile}"; \
	done

link-misc: $(addprefix $(CFG_TARGET)/,$(MISC))


# rules ----------------------------------------------------------------------
# link to CFG_TARGET
$(CFG_TARGET)/%:
	@ln -nsv $(PWD)/config/$(@F) $@

$(BACKUP_TARGET)/%: %
	@echo "backup"

# link to HOME
$(HOME)/%:
	@ln -nsv $(PWD)/$(@F) $@
