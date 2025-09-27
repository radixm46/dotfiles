EMACS   ?= emacs
COMPILE := @echo [Compile byte] - `date +%Y/%m/%d-%H:%M:%S`; \
	$(EMACS) -Q --batch -f batch-byte-compile

TOP_DIR := ${HOME}/.emacs.d
ELISP_DIRS := . elisp
ELFILES    := $(foreach d,$(ELISP_DIRS),$(wildcard $(d)/*.el))
ELCS       := $(ELFILES:.el=.elc)

.PHONY: default compile-all clean-elc parens-check byte-compile-werror pkgs-backup
default: compile-all

%.elc: %.el
	$(COMPILE) $<


check-parens:
	@set -euo pipefail; \
	if [ -z "$(ELFILES)" ]; then echo "No .el files"; exit 0; fi; \
	for f in $(ELFILES); do \
	  printf "Checking $$f: "; \
	  $(EMACS) -Q --batch "$$f" --eval \
	  "(condition-case data\
	    (progn (scan-sexps (point-min) (point-max)) (message \"OK!\")) \
	     (scan-error \
	       (goto-char (nth 2 data)) \
	         (message \"%d:%d: Unmatched bracket or quote\" \
	           (line-number-at-pos) (1+ (- (point) (line-beginning-position)))) \
	       (kill-emacs 1)))"; \
	done

compile-all: $(ELCS)

clean-elc:
	@rm -f $(ELCS)

.PHONY: compile
compile:
	$(COMPILE) $(filter-out $@,$(MAKECMDGOALS))

pkgs-backup:
	mv ./straight ./straight.bak-`date +%Y%m%d%H%M`

.PHONY: pkgs-setup
pkgs-setup: init.elc
	@echo [Setup init.el] - `date +%Y/%m/%d-%H:%M:%S`;\
	$(EMACS) --batch --load 'init.elc' \
	  --eval '(unless (string-match-p "NFM.ttf" (shell-command-to-string "fc-list | grep NFM")) (nerd-icons-install-fonts t))' \
	  --eval '(treesit-auto-install-all)' \
	  --eval '(vterm-module-compile)' \
	  --eval '(pdf-tools-install-noverify)'


LOCKFILE_DIR  := straight/versions
.PHONY: lockfile-gen
lockfile-gen: clean-elc lockfile-backup
	@echo [Straight freeze versions] - `date +%Y/%m/%d-%H:%M:%S`
	@FREEZE_EMACS=0 $(MAKE) compile-all
	@FREEZE_EMACS=1 $(EMACS) --batch \
	  --load init.elc
	@echo [Straight freeze versions] - freeze done, rebuild elcs
	$(MAKE) clean-elc
	$(MAKE) compile-all
	  --load init.el
	@echo [Straight freeze versions] - freeze done, rebuild elcs
	$(MAKE) clean-elc
	$(MAKE) compile-all

.PHONY: lockfile-backup
lockfile-backup:
	@if [ -e "$(LOCKFILE_DIR)/default.el" ]; then \
	  BKDATE=`date +%Y%m%d%H%M`; \
	  cp "$(LOCKFILE_DIR)/default.el" "$(LOCKFILE_DIR)/default-$$BKDATE.el"; \
	  echo "lockfile backed up to default-$$BKDATE.el"; \
	else \
	  echo "no lockfile found, nothing to back up."; \
	fi

