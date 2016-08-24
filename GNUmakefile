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
C_COMMON_FLAGS := -I$(TOPDIR) -I$(TOPDIR)/c_inc -D_GNU_SOURCE

ifeq (,$(VERBOSE))
NOECHO := @
else
NOECHO :=
endif


.PHONY : build-tools
all : build-tools libs
libs :
clean :

#include $(wildcard */GNUmakefile.sub)
include unix/GNUmakefile.sub
include b_physics/GNUmakefile.sub
include calor_filter/GNUmakefile.sub
include calor_off/GNUmakefile.sub
include calor_util/GNUmakefile.sub
include cdc_util/GNUmakefile.sub
include cd_util/GNUmakefile.sub
include compack/GNUmakefile.sub
include d0dad/GNUmakefile.sub
include d0geant/GNUmakefile.sub
include d0hplt/GNUmakefile.sub
include d0reco/GNUmakefile.sub
include d0user/GNUmakefile.sub
include dbl3/GNUmakefile.sub
include errmsg_util/GNUmakefile.sub
include event_util/GNUmakefile.sub
