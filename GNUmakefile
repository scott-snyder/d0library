TOPDIR := .
BINDIR := $(TOPDIR)/bin
ifeq (,$(wildcard $(BINDIR)))
  $(shell mkdir -p $(BINDIR))
endif

F77 := g77
F77_DEB_FLAGS := -g
F77_OPT_FLAGS := -O2
F77_COMMON_FLAGS := -fdollar-ok -fugly-logint -fno-second-underscore \
                    -fno-automatic -fno-backslash -Wno-globals -I$(TOPDIR)

CC := cc
C_DEB_FLAGS := -g
C_OPT_FLAGS := -O2
C_COMMON_FLAGS := -I$(TOPDIR) -I$(TOPDIR)/c_inc

ifeq (,$(VERBOSE))
NOECHO := @
else
NOECHO :=
endif


.PHONY : build-tools
all : build-tools libs
libs :
clean :

include $(wildcard */GNUmakefile.sub)

