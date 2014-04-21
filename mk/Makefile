DEPS = $(wildcard deps/*)
APPS = apps/face apps/server
SUBDIRS := $(DEPS) $(APPS)
#SUBDIRS = deps/shen

MKINCDIR ?= /Users/skif/devel/git.hub.kakaranet/games/mk

indir = /Users/skif/devel/git.hub.kakaranet/games/
outdir = /Users/skif/devel/git.hub.kakaranet/games/

export VERBOSE = @
export DEVNULL = >/dev/null
SILENT = -s

ifeq "$(V)" "1"
VERBOSE =
DEVNULL =
SILENT = 
endif

all: build


clean compile:
	$(VERBOSE)cd $(indir) && \
	for d in $(SUBDIRS); do \
		$(MAKE) $(SILENT) -C $(indir)/$${d} -f $(MKINCDIR)/erlang.mk indir=$(indir)/$${d} outdir=$(outdir)/$${d} subdir=$${d} $@ || break; \
	done

