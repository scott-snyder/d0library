TOPDIR := .
BINDIR := $(TOPDIR)/bin
ifeq (,$(wildcard $(BINDIR)))
  $(shell mkdir -p $(BINDIR))
endif

FORT := gfortran
FORT_DEB_FLAGS := -g
FORT_OPT_FLAGS := -O2
FORT_COMMON_FLAGS := -fdollar-ok -fno-second-underscore --std=legacy \
                     -fno-automatic -fno-backslash -I$(TOPDIR) -fd-lines-as-comments

F77 := g77
F77_COMMON_FLAGS := -fdollar-ok -fno-second-underscore \
                    -fno-automatic -fno-backslash -I$(TOPDIR)

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
all : build-tools libs bins
libs :
bins :
clean :

export d0calor_filter__source=calor_filter/source
export d0compack__source=compack/source
export d0errmsg_util__params=errmsg_util/params
export d0inc=inc
export d0lcompack__source=lcompack/source
export d0links=links
export d0offline_util__general=offline_util/general
export d0params=params
export d0program_builder__inc=program_builder/inc
export d0spythia__inc=spythia/inc
export d0util=util
export d0xframe__source=xframe/source
export d0zeb__isa=zeb/isa
export d0zeb__run_head=zeb/run_head
export d0zeb__scal=zeb/scal

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
include fdc_util/GNUmakefile.sub
include graphics_util/GNUmakefile.sub
include isajet/GNUmakefile.sub
include isajet_util/GNUmakefile.sub
include isazeb/GNUmakefile.sub
include lcompack/GNUmakefile.sub
include level0/GNUmakefile.sub
include level1/GNUmakefile.sub
include level2/GNUmakefile.sub
include muon_reco/GNUmakefile.sub
include muon_util/GNUmakefile.sub
include neural/GNUmakefile.sub
include nodi3000/GNUmakefile.sub
include offline_util/GNUmakefile.sub
include physics_util/GNUmakefile.sub
include pixie/GNUmakefile.sub
include program_builder/GNUmakefile.sub
include pythia/GNUmakefile.sub
include qcd/GNUmakefile.sub
include showerlibrary/GNUmakefile.sub
include spythia/GNUmakefile.sub
include srcp_util/GNUmakefile.sub
include stp/GNUmakefile.sub
include top_physics/GNUmakefile.sub
include tpmfarm/GNUmakefile.sub
include trd_util/GNUmakefile.sub
include util/GNUmakefile.sub
include vtx_util/GNUmakefile.sub
include wz/GNUmakefile.sub
include xframe/GNUmakefile.sub
include zebra_util/GNUmakefile.sub
