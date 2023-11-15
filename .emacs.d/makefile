EMACS   ?= emacs
COMPILE := @echo [Compile byte] - `date +%Y/%m/%d-%H:%M:%S`; \
	$(EMACS) -Q --batch -f batch-byte-compile

TOP_DIR := ${HOME}/.emacs.d


EARLY  := early-init.elc
PKG    := elisp/initpkg.elc elisp/util.elc
SHARED := elisp/doom.elc elisp/conf-evil.elc elisp/conf-fonts.elc
FEAT   := elisp/conf-ext-fronts.elc elisp/conf-langs.elc elisp/conf-elfeed.elc elisp/conf-org.elc
INIT   := init.elc
LOCAL  := elisp/local.elc
ALL    := $(EARLY) $(PKG) $(SHARED) $(FEAT) $(INIT) $(LOCAL)


.PHONY: default
default: compile-all

early-init.elc: early-init.el
	$(COMPILE) early-init.el

elisp/initpkg.elc: elisp/initpkg.el
	$(COMPILE) elisp/initpkg.el

elisp/util.elc: \
elisp/initpkg.el elisp/util.el
	$(COMPILE) elisp/util.el

elisp/conf-fonts.elc: \
elisp/initpkg.el \
elisp/conf-fonts.el
	$(COMPILE) elisp/conf-fonts.el

elisp/doom.elc: \
elisp/initpkg.el \
elisp/doom.el
	$(COMPILE) elisp/doom.el

elisp/conf-evil.elc: \
elisp/initpkg.el \
elisp/conf-fonts.el \
elisp/conf-evil.el
	$(COMPILE) elisp/conf-evil.el

elisp/conf-ext-fronts.elc: \
elisp/initpkg.el \
elisp/doom.el \
elisp/conf-ext-fronts.el
	$(COMPILE) elisp/conf-ext-fronts.el

elisp/conf-org.elc: \
elisp/initpkg.el \
elisp/conf-fonts.el \
elisp/conf-evil.el \
elisp/conf-org.el
	$(COMPILE) elisp/conf-org.el

elisp/conf-langs.elc: \
elisp/initpkg.el elisp/util.el \
elisp/conf-evil.el \
elisp/conf-langs.el
	$(COMPILE) elisp/conf-langs.el

elisp/conf-elfeed.elc: \
elisp/initpkg.el elisp/util.el \
elisp/doom.el elisp/conf-evil.el \
elisp/conf-ext-fronts.el elisp/conf-fonts.el \
elisp/conf-langs.el elisp/conf-elfeed.el
	$(COMPILE) elisp/conf-elfeed.el

init.elc: \
init.el \
elisp/initpkg.el elisp/util.el \
elisp/conf-fonts.el elisp/conf-evil.el \
elisp/doom.el elisp/conf-org.el
	$(COMPILE) init.el

elisp/local.elc: elisp/local.el elisp/initpkg.el elisp/util.el
	$(COMPILE) elisp/local.el

.PHONY: compile
compile:
	$(COMPILE) $(filter-out $@,$(MAKECMDGOALS))

.PHONY: compile-all
compile-all: $(ALL)

.PHONY: pkgs-backup
pkgs-backup:
	mv ./straight ./straight.bak`date +%Y%m%d%H%M`

.PHONY: clean-elc
clean-elc:
	rm -f *.elc elisp/*.elc

