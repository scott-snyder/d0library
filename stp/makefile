SHELL = /bin/sh
CCFLAGS = -c -dollar -G 0 -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0test)/unix/source -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
LCCFLAGS = -c -dollar -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0test)/unix/source -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
F77FLAGS = -c -nocpp -static -col72 -backslash -G 0 -Olimit 2500 -Wo,-loopunroll,2
DEBUG = -O0 -g2
OPT = -O2
ARFLAGS = crsl
SCRATCH = $(d0library)/admin/tmp/d0librar/mmstomake/stp
FLAVOR = SIUNIX
F77 = f77
.IGNORE:
.SUFFIXES:

opt :\
  cdc.a\
  fdc.a\
  geocal.a\
  geogen.a\
  geotbm.a\
  samus.a\
  tb90_data.a\
  tb90l2.a\
  trd.a\
  vtx.a
cdc.a : $(d0root)/stp/cdc.a
	@ echo cdc.a is up to date
$(d0root)/stp/cdc.a:: $(SCRATCH)/cdc/nothing.nl
$(d0root)/stp/cdc.a::\
  $(d0root)/stp/cdc.a(bldalh.o)\
  $(d0root)/stp/cdc.a(bldalh_0.o)\
  $(d0root)/stp/cdc.a(bldcbd.o)\
  $(d0root)/stp/cdc.a(bldcbd_d0.o)\
  $(d0root)/stp/cdc.a(bldgeh.o)\
  $(d0root)/stp/cdc.a(bldgnh.o)\
  $(d0root)/stp/cdc.a(bldmat.o)\
  $(d0root)/stp/cdc.a(bldnlx.o)\
  $(d0root)/stp/cdc.a(bldpdh.o)\
  $(d0root)/stp/cdc.a(bldrft.o)\
  $(d0root)/stp/cdc.a(bldtmh.o)\
  $(d0root)/stp/cdc.a(bldtmp.o)\
  $(d0root)/stp/cdc.a(bldtva.o)\
  $(d0root)/stp/cdc.a(bldwal.o)\
  $(d0root)/stp/cdc.a(bldxxp.o)\
  $(d0root)/stp/cdc.a(cdwstp.o)\
  $(d0root)/stp/cdc.a(dcstor.o)\
  $(d0root)/stp/cdc.a(drddly.o)\
  $(d0root)/stp/cdc.a(drdgns.o)
	ar $(ARFLAGS) $(d0root)/stp/cdc.a `ls $(SCRATCH)/cdc/a1/*.o 2> /dev/null`
	rmdirp $(SCRATCH)/cdc
$(SCRATCH)/cdc/nothing.nl:
	rmdirp $(SCRATCH)/cdc
	mkdirp $(SCRATCH)/cdc/a1
$(d0root)/stp/cdc.a(bldalh.o):\
  $(d0root)/stp/cdc/bldalh.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdalh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/cdc/bldalh.f ;\
	mv bldalh.o $(SCRATCH)/cdc/a1/bldalh.o ;\
	)
$(d0root)/stp/cdc.a(bldalh_0.o):\
  $(d0root)/stp/cdc/bldalh_0.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdalh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/cdc/bldalh_0.f ;\
	mv bldalh_0.o $(SCRATCH)/cdc/a1/bldalh_0.o ;\
	)
$(d0root)/stp/cdc.a(bldcbd.o):\
  $(d0root)/stp/cdc/bldcbd.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/cdc/bldcbd.f ;\
	mv bldcbd.o $(SCRATCH)/cdc/a1/bldcbd.o ;\
	)
$(d0root)/stp/cdc.a(bldcbd_d0.o):\
  $(d0root)/stp/cdc/bldcbd_d0.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdcbd.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/cdc/bldcbd_d0.f ;\
	mv bldcbd_d0.o $(SCRATCH)/cdc/a1/bldcbd_d0.o ;\
	)
$(d0root)/stp/cdc.a(bldgeh.o):\
  $(d0root)/stp/cdc/bldgeh.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdgeh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/cdc/bldgeh.f ;\
	mv bldgeh.o $(SCRATCH)/cdc/a1/bldgeh.o ;\
	)
$(d0root)/stp/cdc.a(bldgnh.o):\
  $(d0root)/stp/cdc/bldgnh.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/cdc/bldgnh.f ;\
	mv bldgnh.o $(SCRATCH)/cdc/a1/bldgnh.o ;\
	)
$(d0root)/stp/cdc.a(bldmat.o):\
  $(d0root)/stp/cdc/bldmat.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdmat.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/cdc/bldmat.f ;\
	mv bldmat.o $(SCRATCH)/cdc/a1/bldmat.o ;\
	)
$(d0root)/stp/cdc.a(bldnlx.o):\
  $(d0root)/stp/cdc/bldnlx.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdnli.link\
  $(d0library)/links/izdnlo.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/cdc/bldnlx.f ;\
	mv bldnlx.o $(SCRATCH)/cdc/a1/bldnlx.o ;\
	)
$(d0root)/stp/cdc.a(bldpdh.o):\
  $(d0root)/stp/cdc/bldpdh.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/cdc/bldpdh.f ;\
	mv bldpdh.o $(SCRATCH)/cdc/a1/bldpdh.o ;\
	)
$(d0root)/stp/cdc.a(bldrft.o):\
  $(d0root)/stp/cdc/bldrft.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdrft.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/cdc/bldrft.f ;\
	mv bldrft.o $(SCRATCH)/cdc/a1/bldrft.o ;\
	)
$(d0root)/stp/cdc.a(bldtmh.o):\
  $(d0root)/stp/cdc/bldtmh.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/cdc/bldtmh.f ;\
	mv bldtmh.o $(SCRATCH)/cdc/a1/bldtmh.o ;\
	)
$(d0root)/stp/cdc.a(bldtmp.o):\
  $(d0root)/stp/cdc/bldtmp.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/cdc/bldtmp.f ;\
	mv bldtmp.o $(SCRATCH)/cdc/a1/bldtmp.o ;\
	)
$(d0root)/stp/cdc.a(bldtva.o):\
  $(d0root)/stp/cdc/bldtva.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdtvh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/cdc/bldtva.f ;\
	mv bldtva.o $(SCRATCH)/cdc/a1/bldtva.o ;\
	)
$(d0root)/stp/cdc.a(bldwal.o):\
  $(d0root)/stp/cdc/bldwal.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdwal.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/cdc/bldwal.f ;\
	mv bldwal.o $(SCRATCH)/cdc/a1/bldwal.o ;\
	)
$(d0root)/stp/cdc.a(bldxxp.o):\
  $(d0root)/stp/cdc/bldxxp.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdswp.link\
  $(d0library)/links/izddlp.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/cdc/bldxxp.f ;\
	mv bldxxp.o $(SCRATCH)/cdc/a1/bldxxp.o ;\
	)
$(d0root)/stp/cdc.a(cdwstp.o):\
  $(d0root)/stp/cdc/cdwstp.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/cdc/cdwstp.f ;\
	mv cdwstp.o $(SCRATCH)/cdc/a1/cdwstp.o ;\
	)
$(d0root)/stp/cdc.a(dcstor.o):\
  $(d0root)/stp/cdc/dcstor.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/cdc/dcstor.f ;\
	mv dcstor.o $(SCRATCH)/cdc/a1/dcstor.o ;\
	)
$(d0root)/stp/cdc.a(drddly.o):\
  $(d0root)/stp/cdc/drddly.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/cdc/drddly.f ;\
	mv drddly.o $(SCRATCH)/cdc/a1/drddly.o ;\
	)
$(d0root)/stp/cdc.a(drdgns.o):\
  $(d0root)/stp/cdc/drdgns.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/cdc/drdgns.f ;\
	mv drdgns.o $(SCRATCH)/cdc/a1/drdgns.o ;\
	)
fdc.a : $(d0root)/stp/fdc.a
	@ echo fdc.a is up to date
$(d0root)/stp/fdc.a:: $(SCRATCH)/fdc/nothing.nl
$(d0root)/stp/fdc.a::\
  $(d0root)/stp/fdc.a(blfalh.o)\
  $(d0root)/stp/fdc.a(blfalh_d0.o)\
  $(d0root)/stp/fdc.a(blfalh_v2.o)\
  $(d0root)/stp/fdc.a(blfdph.o)\
  $(d0root)/stp/fdc.a(blfdrt.o)\
  $(d0root)/stp/fdc.a(blfdrt_d0.o)\
  $(d0root)/stp/fdc.a(blfdta.o)\
  $(d0root)/stp/fdc.a(blfdta_d0.o)\
  $(d0root)/stp/fdc.a(blfdtb.o)\
  $(d0root)/stp/fdc.a(blfdtb_d0.o)\
  $(d0root)/stp/fdc.a(blfgeh.o)\
  $(d0root)/stp/fdc.a(blfgeh_d0.o)\
  $(d0root)/stp/fdc.a(blfgeh_v2.o)\
  $(d0root)/stp/fdc.a(blfgnh.o)\
  $(d0root)/stp/fdc.a(blflis.o)\
  $(d0root)/stp/fdc.a(blfmat.o)\
  $(d0root)/stp/fdc.a(blfpdh.o)\
  $(d0root)/stp/fdc.a(blftmh.o)\
  $(d0root)/stp/fdc.a(blftmh_d0.o)\
  $(d0root)/stp/fdc.a(blftmh_v2.o)\
  $(d0root)/stp/fdc.a(blfwal.o)\
  $(d0root)/stp/fdc.a(blfwph.o)\
  $(d0root)/stp/fdc.a(blfwta.o)\
  $(d0root)/stp/fdc.a(blfwtb.o)\
  $(d0root)/stp/fdc.a(fdwstp_d0.o)\
  $(d0root)/stp/fdc.a(fdwstp_mc.o)\
  $(d0root)/stp/fdc.a(fstpar.o)\
  $(d0root)/stp/fdc.a(fstpar_v2.o)\
  $(d0root)/stp/fdc.a(inzstp.o)
	ar $(ARFLAGS) $(d0root)/stp/fdc.a `ls $(SCRATCH)/fdc/a1/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/stp/fdc.a `ls $(SCRATCH)/fdc/a2/*.o 2> /dev/null`
	rmdirp $(SCRATCH)/fdc
$(SCRATCH)/fdc/nothing.nl:
	rmdirp $(SCRATCH)/fdc
	mkdirp $(SCRATCH)/fdc/a1
	mkdirp $(SCRATCH)/fdc/a2
$(d0root)/stp/fdc.a(blfalh.o):\
  $(d0root)/stp/fdc/blfalh.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izfalh.link\
  $(d0library)/links/izfgeh.link\
  $(d0library)/links/izfwal.link\
  $(d0library)/links/izfwta.link\
  $(d0library)/links/izfwtb.link\
  $(d0library)/links/izfwph.link\
  $(d0library)/links/izfdrt.link\
  $(d0library)/links/izfdta.link\
  $(d0library)/links/izfdtb.link\
  $(d0library)/links/izfdph.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/blfalh.f ;\
	mv blfalh.o $(SCRATCH)/fdc/a1/blfalh.o ;\
	)
$(d0root)/stp/fdc.a(blfalh_d0.o):\
  $(d0root)/stp/fdc/blfalh_d0.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izfalh.link\
  $(d0library)/links/izfgeh.link\
  $(d0library)/links/izfwal.link\
  $(d0library)/links/izfwta.link\
  $(d0library)/links/izfwtb.link\
  $(d0library)/links/izfwph.link\
  $(d0library)/links/izfdrt.link\
  $(d0library)/links/izfdta.link\
  $(d0library)/links/izfdtb.link\
  $(d0library)/links/izfdph.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/blfalh_d0.f ;\
	mv blfalh_d0.o $(SCRATCH)/fdc/a1/blfalh_d0.o ;\
	)
$(d0root)/stp/fdc.a(blfalh_v2.o):\
  $(d0root)/stp/fdc/blfalh_v2.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izfalh.link\
  $(d0library)/links/izfgeh.link\
  $(d0library)/links/izfwal.link\
  $(d0library)/links/izfwta.link\
  $(d0library)/links/izfwtb.link\
  $(d0library)/links/izfwph.link\
  $(d0library)/links/izfdrt.link\
  $(d0library)/links/izfdta.link\
  $(d0library)/links/izfdtb.link\
  $(d0library)/links/izfdph.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/blfalh_v2.f ;\
	mv blfalh_v2.o $(SCRATCH)/fdc/a1/blfalh_v2.o ;\
	)
$(d0root)/stp/fdc.a(blfdph.o):\
  $(d0root)/stp/fdc/blfdph.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfdrt.link\
  $(d0library)/links/izfdph.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/blfdph.f ;\
	mv blfdph.o $(SCRATCH)/fdc/a1/blfdph.o ;\
	)
$(d0root)/stp/fdc.a(blfdrt.o):\
  $(d0root)/stp/fdc/blfdrt.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfdrt.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/blfdrt.f ;\
	mv blfdrt.o $(SCRATCH)/fdc/a1/blfdrt.o ;\
	)
$(d0root)/stp/fdc.a(blfdrt_d0.o):\
  $(d0root)/stp/fdc/blfdrt_d0.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfdrt.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/blfdrt_d0.f ;\
	mv blfdrt_d0.o $(SCRATCH)/fdc/a1/blfdrt_d0.o ;\
	)
$(d0root)/stp/fdc.a(blfdta.o):\
  $(d0root)/stp/fdc/blfdta.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfdrt.link\
  $(d0library)/links/izfdta.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/blfdta.f ;\
	mv blfdta.o $(SCRATCH)/fdc/a1/blfdta.o ;\
	)
$(d0root)/stp/fdc.a(blfdta_d0.o):\
  $(d0root)/stp/fdc/blfdta_d0.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfdrt.link\
  $(d0library)/links/izfdta.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/blfdta_d0.f ;\
	mv blfdta_d0.o $(SCRATCH)/fdc/a1/blfdta_d0.o ;\
	)
$(d0root)/stp/fdc.a(blfdtb.o):\
  $(d0root)/stp/fdc/blfdtb.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfdrt.link\
  $(d0library)/links/izfdtb.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/blfdtb.f ;\
	mv blfdtb.o $(SCRATCH)/fdc/a1/blfdtb.o ;\
	)
$(d0root)/stp/fdc.a(blfdtb_d0.o):\
  $(d0root)/stp/fdc/blfdtb_d0.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfdrt.link\
  $(d0library)/links/izfdtb.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/blfdtb_d0.f ;\
	mv blfdtb_d0.o $(SCRATCH)/fdc/a1/blfdtb_d0.o ;\
	)
$(d0root)/stp/fdc.a(blfgeh.o):\
  $(d0root)/stp/fdc/blfgeh.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izfgeh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/blfgeh.f ;\
	mv blfgeh.o $(SCRATCH)/fdc/a1/blfgeh.o ;\
	)
$(d0root)/stp/fdc.a(blfgeh_d0.o):\
  $(d0root)/stp/fdc/blfgeh_d0.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izfgeh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/blfgeh_d0.f ;\
	mv blfgeh_d0.o $(SCRATCH)/fdc/a1/blfgeh_d0.o ;\
	)
$(d0root)/stp/fdc.a(blfgeh_v2.o):\
  $(d0root)/stp/fdc/blfgeh_v2.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izfgeh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/blfgeh_v2.f ;\
	mv blfgeh_v2.o $(SCRATCH)/fdc/a1/blfgeh_v2.o ;\
	)
$(d0root)/stp/fdc.a(blfgnh.o):\
  $(d0root)/stp/fdc/blfgnh.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfgnh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/blfgnh.f ;\
	mv blfgnh.o $(SCRATCH)/fdc/a1/blfgnh.o ;\
	)
$(d0root)/stp/fdc.a(blflis.o):\
  $(d0root)/stp/fdc/blflis.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izflis.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/blflis.f ;\
	mv blflis.o $(SCRATCH)/fdc/a1/blflis.o ;\
	)
$(d0root)/stp/fdc.a(blfmat.o):\
  $(d0root)/stp/fdc/blfmat.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfmat.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/blfmat.f ;\
	mv blfmat.o $(SCRATCH)/fdc/a1/blfmat.o ;\
	)
$(d0root)/stp/fdc.a(blfpdh.o):\
  $(d0root)/stp/fdc/blfpdh.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfpdh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/blfpdh.f ;\
	mv blfpdh.o $(SCRATCH)/fdc/a1/blfpdh.o ;\
	)
$(d0root)/stp/fdc.a(blftmh.o):\
  $(d0root)/stp/fdc/blftmh.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izftmh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/blftmh.f ;\
	mv blftmh.o $(SCRATCH)/fdc/a1/blftmh.o ;\
	)
$(d0root)/stp/fdc.a(blftmh_d0.o):\
  $(d0root)/stp/fdc/blftmh_d0.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izftmh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/blftmh_d0.f ;\
	mv blftmh_d0.o $(SCRATCH)/fdc/a1/blftmh_d0.o ;\
	)
$(d0root)/stp/fdc.a(blftmh_v2.o):\
  $(d0root)/stp/fdc/blftmh_v2.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izftmh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/blftmh_v2.f ;\
	mv blftmh_v2.o $(SCRATCH)/fdc/a1/blftmh_v2.o ;\
	)
$(d0root)/stp/fdc.a(blfwal.o):\
  $(d0root)/stp/fdc/blfwal.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfwal.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/blfwal.f ;\
	mv blfwal.o $(SCRATCH)/fdc/a2/blfwal.o ;\
	)
$(d0root)/stp/fdc.a(blfwph.o):\
  $(d0root)/stp/fdc/blfwph.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfwal.link\
  $(d0library)/links/izfwph.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/blfwph.f ;\
	mv blfwph.o $(SCRATCH)/fdc/a2/blfwph.o ;\
	)
$(d0root)/stp/fdc.a(blfwta.o):\
  $(d0root)/stp/fdc/blfwta.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfwal.link\
  $(d0library)/links/izfwta.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/blfwta.f ;\
	mv blfwta.o $(SCRATCH)/fdc/a2/blfwta.o ;\
	)
$(d0root)/stp/fdc.a(blfwtb.o):\
  $(d0root)/stp/fdc/blfwtb.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfwal.link\
  $(d0library)/links/izfwtb.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/blfwtb.f ;\
	mv blfwtb.o $(SCRATCH)/fdc/a2/blfwtb.o ;\
	)
$(d0root)/stp/fdc.a(fdwstp_d0.o):\
  $(d0root)/stp/fdc/fdwstp_d0.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/fdwstp_d0.f ;\
	mv fdwstp_d0.o $(SCRATCH)/fdc/a2/fdwstp_d0.o ;\
	)
$(d0root)/stp/fdc.a(fdwstp_mc.o):\
  $(d0root)/stp/fdc/fdwstp_mc.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/fdwstp_mc.f ;\
	mv fdwstp_mc.o $(SCRATCH)/fdc/a2/fdwstp_mc.o ;\
	)
$(d0root)/stp/fdc.a(fstpar.o):\
  $(d0root)/stp/fdc/fstpar.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdprm2.inc\
  $(d0library)/links/izfalh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/fstpar.f ;\
	mv fstpar.o $(SCRATCH)/fdc/a2/fstpar.o ;\
	)
$(d0root)/stp/fdc.a(fstpar_v2.o):\
  $(d0root)/stp/fdc/fstpar_v2.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdprm2.inc\
  $(d0library)/links/izfalh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/fdc/fstpar_v2.f ;\
	mv fstpar_v2.o $(SCRATCH)/fdc/a2/fstpar_v2.o ;\
	)
$(d0root)/stp/fdc.a(inzstp.o):\
  $(d0root)/stp/geocal/inzstp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/inzstp.f ;\
	mv inzstp.o $(SCRATCH)/fdc/a2/inzstp.o ;\
	)
geocal.a : $(d0root)/stp/geocal.a
	@ echo geocal.a is up to date
$(d0root)/stp/geocal.a:: $(SCRATCH)/geocal/nothing.nl
$(d0root)/stp/geocal.a::\
  $(d0root)/stp/geocal.a(bkclin.o)\
  $(d0root)/stp/geocal.a(bkcmdl.o)\
  $(d0root)/stp/geocal.a(bkcsrv.o)\
  $(d0root)/stp/geocal.a(bkcsyl.o)\
  $(d0root)/stp/geocal.a(bkcthe.o)\
  $(d0root)/stp/geocal.a(bmpipe.o)\
  $(d0root)/stp/geocal.a(cabsrb.o)\
  $(d0root)/stp/geocal.a(cal_aents483.o)\
  $(d0root)/stp/geocal.a(cal_postgeo.o)\
  $(d0root)/stp/geocal.a(cal_pregeo.o)\
  $(d0root)/stp/geocal.a(cal_survey.o)\
  $(d0root)/stp/geocal.a(cal_suple854.o)\
  $(d0root)/stp/geocal.a(cawstp.o)\
  $(d0root)/stp/geocal.a(cc_geom.o)\
  $(d0root)/stp/geocal.a(cc_iuset.o)\
  $(d0root)/stp/geocal.a(cc_ma_gap585.o)\
  $(d0root)/stp/geocal.a(cc_mg_div.o)\
  $(d0root)/stp/geocal.a(cc_sunder942.o)\
  $(d0root)/stp/geocal.a(cc_su_fit407.o)\
  $(d0root)/stp/geocal.a(cc_suebra926.o)\
  $(d0root)/stp/geocal.a(cc_sudule730.o)\
  $(d0root)/stp/geocal.a(cc_thanks767.o)\
  $(d0root)/stp/geocal.a(cc_wruset507.o)\
  $(d0root)/stp/geocal.a(cc_wrials915.o)\
  $(d0root)/stp/geocal.a(cc_wrices809.o)\
  $(d0root)/stp/geocal.a(cc_wrlume617.o)\
  $(d0root)/stp/geocal.a(ccch_late441.o)\
  $(d0root)/stp/geocal.a(ccch_cells.o)\
  $(d0root)/stp/geocal.a(ccch_crack.o)\
  $(d0root)/stp/geocal.a(ccch_late351.o)\
  $(d0root)/stp/geocal.a(ccch_floor.o)\
  $(d0root)/stp/geocal.a(ccch_late593.o)\
  $(d0root)/stp/geocal.a(ccch_geom.o)\
  $(d0root)/stp/geocal.a(ccch_module.o)\
  $(d0root)/stp/geocal.a(ccch_mother.o)\
  $(d0root)/stp/geocal.a(ccch_pipe659.o)\
  $(d0root)/stp/geocal.a(ccch_tout500.o)\
  $(d0root)/stp/geocal.a(ccch_ices337.o)\
  $(d0root)/stp/geocal.a(ccem_late448.o)\
  $(d0root)/stp/geocal.a(ccem_dule768.o)\
  $(d0root)/stp/geocal.a(ccem_cells.o)\
  $(d0root)/stp/geocal.a(ccem_crack.o)\
  $(d0root)/stp/geocal.a(ccem_late358.o)\
  $(d0root)/stp/geocal.a(ccem_uset007.o)\
  $(d0root)/stp/geocal.a(ccem_floor.o)\
  $(d0root)/stp/geocal.a(ccem_late600.o)\
  $(d0root)/stp/geocal.a(ccem_geom.o)\
  $(d0root)/stp/geocal.a(ccem_geom467.o)\
  $(d0root)/stp/geocal.a(ccem_uset597.o)\
  $(d0root)/stp/geocal.a(ccem_module.o)\
  $(d0root)/stp/geocal.a(ccem_mother.o)\
  $(d0root)/stp/geocal.a(ccem_geom566.o)\
  $(d0root)/stp/geocal.a(ccem_uset696.o)\
  $(d0root)/stp/geocal.a(ccem_ices344.o)\
  $(d0root)/stp/geocal.a(ccem_geom469.o)\
  $(d0root)/stp/geocal.a(ccem_uset599.o)\
  $(d0root)/stp/geocal.a(ccfh_late444.o)\
  $(d0root)/stp/geocal.a(ccfh_cells.o)\
  $(d0root)/stp/geocal.a(ccfh_crack.o)\
  $(d0root)/stp/geocal.a(ccfh_late354.o)\
  $(d0root)/stp/geocal.a(ccfh_floor.o)\
  $(d0root)/stp/geocal.a(ccfh_late596.o)\
  $(d0root)/stp/geocal.a(ccfh_geom.o)\
  $(d0root)/stp/geocal.a(ccfh_module.o)\
  $(d0root)/stp/geocal.a(ccfh_mother.o)\
  $(d0root)/stp/geocal.a(ccfh_ices340.o)\
  $(d0root)/stp/geocal.a(ccpad.o)\
  $(d0root)/stp/geocal.a(cedp.o)\
  $(d0root)/stp/geocal.a(celicd.o)\
  $(d0root)/stp/geocal.a(celpos.o)\
  $(d0root)/stp/geocal.a(celpsi.o)\
  $(d0root)/stp/geocal.a(celsha.o)\
  $(d0root)/stp/geocal.a(cenp.o)\
  $(d0root)/stp/geocal.a(cenpcc.o)\
  $(d0root)/stp/geocal.a(cenpeh.o)\
  $(d0root)/stp/geocal.a(ceta.o)\
  $(d0root)/stp/geocal.a(cgeh.o)\
  $(d0root)/stp/geocal.a(chaba.o)\
  $(d0root)/stp/geocal.a(clay.o)\
  $(d0root)/stp/geocal.a(claycc.o)\
  $(d0root)/stp/geocal.a(clayee.o)\
  $(d0root)/stp/geocal.a(clayih.o)\
  $(d0root)/stp/geocal.a(claymg_cc.o)\
  $(d0root)/stp/geocal.a(claymg_ec.o)\
  $(d0root)/stp/geocal.a(claymh.o)\
  $(d0root)/stp/geocal.a(clayoh.o)\
  $(d0root)/stp/geocal.a(clga.o)\
  $(d0root)/stp/geocal.a(clgacc.o)\
  $(d0root)/stp/geocal.a(clgaee.o)\
  $(d0root)/stp/geocal.a(clgaeh.o)\
  $(d0root)/stp/geocal.a(clgi.o)\
  $(d0root)/stp/geocal.a(clgicc.o)\
  $(d0root)/stp/geocal.a(clgiee.o)\
  $(d0root)/stp/geocal.a(clgieh.o)\
  $(d0root)/stp/geocal.a(clgsrc.o)\
  $(d0root)/stp/geocal.a(clrrcp.o)\
  $(d0root)/stp/geocal.a(clyr.o)\
  $(d0root)/stp/geocal.a(cmat.o)\
  $(d0root)/stp/geocal.a(cmsgcc.o)\
  $(d0root)/stp/geocal.a(cmsgec.o)\
  $(d0root)/stp/geocal.a(cold_shrink.o)\
  $(d0root)/stp/geocal.a(cpla.o)\
  $(d0root)/stp/geocal.a(cprzy.o)\
  $(d0root)/stp/geocal.a(creg.o)\
  $(d0root)/stp/geocal.a(crscel.o)\
  $(d0root)/stp/geocal.a(crscel_eh.o)\
  $(d0root)/stp/geocal.a(crscel_oh.o)\
  $(d0root)/stp/geocal.a(crycen.o)\
  $(d0root)/stp/geocal.a(crycnt.o)\
  $(d0root)/stp/geocal.a(crydrw.o)\
  $(d0root)/stp/geocal.a(cryenc.o)\
  $(d0root)/stp/geocal.a(cryend.o)\
  $(d0root)/stp/geocal.a(cryo.o)\
  $(d0root)/stp/geocal.a(cryocc.o)\
  $(d0root)/stp/geocal.a(cryoec.o)\
  $(d0root)/stp/geocal.a(cryout.o)\
  $(d0root)/stp/geocal.a(cryplt.o)\
  $(d0root)/stp/geocal.a(cryprt.o)\
  $(d0root)/stp/geocal.a(cryset.o)\
  $(d0root)/stp/geocal.a(cscn.o)\
  $(d0root)/stp/geocal.a(csfmake.o)\
  $(d0root)/stp/geocal.a(do_em_geom.o)\
  $(d0root)/stp/geocal.a(do_ih_geom.o)\
  $(d0root)/stp/geocal.a(do_mh_geom.o)\
  $(d0root)/stp/geocal.a(do_misc.o)\
  $(d0root)/stp/geocal.a(do_oh_geom.o)\
  $(d0root)/stp/geocal.a(ec_iusets.o)\
  $(d0root)/stp/geocal.a(ec_mixtures.o)\
  $(d0root)/stp/geocal.a(ec_roices597.o)\
  $(d0root)/stp/geocal.a(ec_su_fit409.o)\
  $(d0root)/stp/geocal.a(ecem_geom469.o)\
  $(d0root)/stp/geocal.a(ecem_uset599.o)\
  $(d0root)/stp/geocal.a(ecem_umes824.o)\
  $(d0root)/stp/geocal.a(ecem_geom462.o)\
  $(d0root)/stp/geocal.a(ecem_loor690.o)\
  $(d0root)/stp/geocal.a(ecem_geom568.o)\
  $(d0root)/stp/geocal.a(ecem_uset698.o)\
  $(d0root)/stp/geocal.a(ecem_umes923.o)\
  $(d0root)/stp/geocal.a(ecem_geom595.o)\
  $(d0root)/stp/geocal.a(ecih_geom975.o)\
  $(d0root)/stp/geocal.a(ecih_geom063.o)\
  $(d0root)/stp/geocal.a(ecih_dule769.o)\
  $(d0root)/stp/geocal.a(ecih_geom075.o)\
  $(d0root)/stp/geocal.a(ecih_geom856.o)\
  $(d0root)/stp/geocal.a(ecih_geom215.o)\
  $(d0root)/stp/geocal.a(ecih_geom468.o)\
  $(d0root)/stp/geocal.a(ecih_uset598.o)\
  $(d0root)/stp/geocal.a(ecih_umes823.o)\
  $(d0root)/stp/geocal.a(ecih_geom461.o)\
  $(d0root)/stp/geocal.a(ecih_geom348.o)\
  $(d0root)/stp/geocal.a(ecih_geom567.o)\
  $(d0root)/stp/geocal.a(ecih_uset697.o)\
  $(d0root)/stp/geocal.a(ecih_umes922.o)\
  $(d0root)/stp/geocal.a(ecih_geom355.o)\
  $(d0root)/stp/geocal.a(ecih_geom459.o)\
  $(d0root)/stp/geocal.a(ecmh_dule773.o)\
  $(d0root)/stp/geocal.a(ecmh_geom472.o)\
  $(d0root)/stp/geocal.a(ecmh_uset602.o)\
  $(d0root)/stp/geocal.a(ecmh_umes827.o)\
  $(d0root)/stp/geocal.a(ecmh_geom465.o)\
  $(d0root)/stp/geocal.a(ecmh_geom571.o)\
  $(d0root)/stp/geocal.a(ecmh_uset701.o)\
  $(d0root)/stp/geocal.a(ecmh_umes926.o)\
  $(d0root)/stp/geocal.a(ecmh_unit.o)\
  $(d0root)/stp/geocal.a(ecn_slyze952.o)\
  $(d0root)/stp/geocal.a(ecn_sebra038.o)\
  $(d0root)/stp/geocal.a(ecn_sdule842.o)\
  $(d0root)/stp/geocal.a(ecn_s_rcp521.o)\
  $(d0root)/stp/geocal.a(ecn_tanks879.o)\
  $(d0root)/stp/geocal.a(ecpad.o)\
  $(d0root)/stp/geocal.a(ecpad_mh.o)\
  $(d0root)/stp/geocal.a(ecs_slyze957.o)\
  $(d0root)/stp/geocal.a(ecs_sebra043.o)\
  $(d0root)/stp/geocal.a(ecs_sdule847.o)\
  $(d0root)/stp/geocal.a(ecs_s_rcp526.o)\
  $(d0root)/stp/geocal.a(ecs_tanks884.o)\
  $(d0root)/stp/geocal.a(em_geom.o)\
  $(d0root)/stp/geocal.a(fcwrit.o)\
  $(d0root)/stp/geocal.a(flrsrc.o)\
  $(d0root)/stp/geocal.a(gang_cells.o)\
  $(d0root)/stp/geocal.a(ih_geom.o)\
  $(d0root)/stp/geocal.a(ih_pre_pre.o)\
  $(d0root)/stp/geocal.a(int_date.o)\
  $(d0root)/stp/geocal.a(inzstp.o)\
  $(d0root)/stp/geocal.a(jdepth.o)\
  $(d0root)/stp/geocal.a(lgtype.o)\
  $(d0root)/stp/geocal.a(lnkenp.o)\
  $(d0root)/stp/geocal.a(lnklay.o)\
  $(d0root)/stp/geocal.a(make_icd.o)\
  $(d0root)/stp/geocal.a(make_lv0.o)\
  $(d0root)/stp/geocal.a(make_over431.o)\
  $(d0root)/stp/geocal.a(make_ture670.o)\
  $(d0root)/stp/geocal.a(make_ther543.o)\
  $(d0root)/stp/geocal.a(make_lv0_pmt.o)\
  $(d0root)/stp/geocal.a(make_lv0_set.o)\
  $(d0root)/stp/geocal.a(make_skin841.o)\
  $(d0root)/stp/geocal.a(make_tile318.o)\
  $(d0root)/stp/geocal.a(make_rite443.o)\
  $(d0root)/stp/geocal.a(make_ite2493.o)\
  $(d0root)/stp/geocal.a(mh_geom.o)\
  $(d0root)/stp/geocal.a(mh_mvol.o)\
  $(d0root)/stp/geocal.a(mixmat.o)\
  $(d0root)/stp/geocal.a(modulners515.o)\
  $(d0root)/stp/geocal.a(modulkers508.o)\
  $(d0root)/stp/geocal.a(oh_geom.o)\
  $(d0root)/stp/geocal.a(oh_mvol.o)\
  $(d0root)/stp/geocal.a(pcone.o)\
  $(d0root)/stp/geocal.a(prclay.o)\
  $(d0root)/stp/geocal.a(prclin.o)\
  $(d0root)/stp/geocal.a(prclyr.o)\
  $(d0root)/stp/geocal.a(prcmdl.o)\
  $(d0root)/stp/geocal.a(prcthe.o)\
  $(d0root)/stp/geocal.a(ptrap.o)\
  $(d0root)/stp/geocal.a(rmin_max.o)\
  $(d0root)/stp/geocal.a(sbclex.o)\
  $(d0root)/stp/geocal.a(scal.o)\
  $(d0root)/stp/geocal.a(scnrcp.o)\
  $(d0root)/stp/geocal.a(shpsrc.o)\
  $(d0root)/stp/geocal.a(skin.o)\
  $(d0root)/stp/geocal.a(spnmod.o)\
  $(d0root)/stp/geocal.a(survey.o)\
  $(d0root)/stp/geocal.a(survey_rcp.o)\
  $(d0root)/stp/geocal.a(tower_geom.o)\
  $(d0root)/stp/geocal.a(towgeo.o)\
  $(d0root)/stp/geocal.a(towshp.o)\
  $(d0root)/stp/geocal.a(write_iuset.o)\
  $(d0root)/stp/geocal.a(writeials622.o)\
  $(d0root)/stp/geocal.a(writeices516.o)\
  $(d0root)/stp/geocal.a(write_volume.o)\
  $(d0root)/stp/geocal.a(write_zdiv.o)\
  $(d0root)/stp/geocal.a(wrtzdv.o)\
  $(d0root)/stp/geocal.a(wrtzdv1.o)
	ar $(ARFLAGS) $(d0root)/stp/geocal.a `ls $(SCRATCH)/geocal/a1/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/stp/geocal.a `ls $(SCRATCH)/geocal/a2/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/stp/geocal.a `ls $(SCRATCH)/geocal/a3/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/stp/geocal.a `ls $(SCRATCH)/geocal/a4/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/stp/geocal.a `ls $(SCRATCH)/geocal/a5/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/stp/geocal.a `ls $(SCRATCH)/geocal/a6/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/stp/geocal.a `ls $(SCRATCH)/geocal/a7/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/stp/geocal.a `ls $(SCRATCH)/geocal/a8/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/stp/geocal.a `ls $(SCRATCH)/geocal/a9/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/stp/geocal.a `ls $(SCRATCH)/geocal/a10/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/stp/geocal.a `ls $(SCRATCH)/geocal/a11/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/stp/geocal.a `ls $(SCRATCH)/geocal/a12/*.o 2> /dev/null`
	rmdirp $(SCRATCH)/geocal
$(SCRATCH)/geocal/nothing.nl:
	rmdirp $(SCRATCH)/geocal
	mkdirp $(SCRATCH)/geocal/a1
	mkdirp $(SCRATCH)/geocal/a2
	mkdirp $(SCRATCH)/geocal/a3
	mkdirp $(SCRATCH)/geocal/a4
	mkdirp $(SCRATCH)/geocal/a5
	mkdirp $(SCRATCH)/geocal/a6
	mkdirp $(SCRATCH)/geocal/a7
	mkdirp $(SCRATCH)/geocal/a8
	mkdirp $(SCRATCH)/geocal/a9
	mkdirp $(SCRATCH)/geocal/a10
	mkdirp $(SCRATCH)/geocal/a11
	mkdirp $(SCRATCH)/geocal/a12
$(d0root)/stp/geocal.a(bkclin.o):\
  $(d0root)/stp/geocal/bkclin.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/params/clin.params\
  $(d0library)/links/izclin.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/bkclin.f ;\
	mv bkclin.o $(SCRATCH)/geocal/a1/bkclin.o ;\
	)
$(d0root)/stp/geocal.a(bkcmdl.o):\
  $(d0root)/stp/geocal/bkcmdl.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc\
  $(d0library)/links/izcmdl.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/bkcmdl.f ;\
	mv bkcmdl.o $(SCRATCH)/geocal/a1/bkcmdl.o ;\
	)
$(d0root)/stp/geocal.a(bkcsrv.o):\
  $(d0root)/stp/geocal/bkcsrv.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/bkcsrv.f ;\
	mv bkcsrv.o $(SCRATCH)/geocal/a1/bkcsrv.o ;\
	)
$(d0root)/stp/geocal.a(bkcsyl.o):\
  $(d0root)/stp/geocal/bkcsyl.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc\
  $(d0library)/links/izcsyl.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/bkcsyl.f ;\
	mv bkcsyl.o $(SCRATCH)/geocal/a1/bkcsyl.o ;\
	)
$(d0root)/stp/geocal.a(bkcthe.o):\
  $(d0root)/stp/geocal/bkcthe.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc\
  $(d0library)/links/izcthe.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/bkcthe.f ;\
	mv bkcthe.o $(SCRATCH)/geocal/a1/bkcthe.o ;\
	)
$(d0root)/stp/geocal.a(bmpipe.o):\
  $(d0root)/stp/geocal/bmpipe.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/bmpipe.f ;\
	mv bmpipe.o $(SCRATCH)/geocal/a1/bmpipe.o ;\
	)
$(d0root)/stp/geocal.a(cabsrb.o):\
  $(d0root)/stp/geocal/cabsrb.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cabsrb.f ;\
	mv cabsrb.o $(SCRATCH)/geocal/a1/cabsrb.o ;\
	)
$(d0root)/stp/geocal.a(cal_aents483.o):\
  $(d0root)/stp/geocal/cal_alignments.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clin.params\
  $(d0library)/inc/region.def\
  $(d0library)/links/izclin.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cal_alignments.f ;\
	mv cal_alignments.o $(SCRATCH)/geocal/a1/cal_aents483.o ;\
	)
$(d0root)/stp/geocal.a(cal_postgeo.o):\
  $(d0root)/stp/geocal/cal_postgeo.f\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izscal.link\
  $(d0library)/links/izcgeh.link\
  $(d0library)/links/izcucl.link\
  $(d0library)/links/izcecl.link\
  $(d0library)/links/izcrst.link\
  $(d0library)/links/izcadt.link\
  $(d0library)/links/izccph.link\
  $(d0library)/links/izccpt.link\
  $(d0library)/links/izccpc.link\
  $(d0library)/links/izccua.link\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cal_postgeo.f ;\
	mv cal_postgeo.o $(SCRATCH)/geocal/a1/cal_postgeo.o ;\
	)
$(d0root)/stp/geocal.a(cal_pregeo.o):\
  $(d0root)/stp/geocal/cal_pregeo.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cal_pregeo.f ;\
	mv cal_pregeo.o $(SCRATCH)/geocal/a1/cal_pregeo.o ;\
	)
$(d0root)/stp/geocal.a(cal_survey.o):\
  $(d0root)/stp/geocal/cal_survey.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izscal.link\
  $(d0library)/links/izclin.link\
  $(d0library)/links/izcgeh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cal_survey.f ;\
	mv cal_survey.o $(SCRATCH)/geocal/a1/cal_survey.o ;\
	)
$(d0root)/stp/geocal.a(cal_suple854.o):\
  $(d0root)/stp/geocal/cal_survey_ntuple.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/params/clin.params\
  $(d0library)/params/clga.params\
  $(d0library)/links/izcsyl.link\
  $(d0library)/links/izcmdl.link\
  $(d0library)/links/izcthe.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclga.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cal_survey_ntuple.f ;\
	mv cal_survey_ntuple.o $(SCRATCH)/geocal/a1/cal_suple854.o ;\
	)
$(d0root)/stp/geocal.a(cawstp.o):\
  $(d0root)/stp/geocal/cawstp.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cawstp.f ;\
	mv cawstp.o $(SCRATCH)/geocal/a1/cawstp.o ;\
	)
$(d0root)/stp/geocal.a(cc_geom.o):\
  $(d0root)/stp/geocal/cc_geom.f\
  $(d0library)/inc/material_list.inc\
  $(d0library)/inc/matrix_list.inc\
  $(d0library)/inc/write_unit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cc_geom.f ;\
	mv cc_geom.o $(SCRATCH)/geocal/a1/cc_geom.o ;\
	)
$(d0root)/stp/geocal.a(cc_iuset.o):\
  $(d0root)/stp/geocal/cc_iuset.f\
  $(d0library)/inc/iuset.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cc_iuset.f ;\
	mv cc_iuset.o $(SCRATCH)/geocal/a1/cc_iuset.o ;\
	)
$(d0root)/stp/geocal.a(cc_ma_gap585.o):\
  $(d0root)/stp/geocal/cc_massless_gap.f\
  $(d0library)/inc/scc_massless_gap.inc\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cc_massless_gap.f ;\
	mv cc_massless_gap.o $(SCRATCH)/geocal/a1/cc_ma_gap585.o ;\
	)
$(d0root)/stp/geocal.a(cc_mg_div.o):\
  $(d0root)/stp/geocal/cc_mg_div.f\
  $(d0library)/inc/write_unit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cc_mg_div.f ;\
	mv cc_mg_div.o $(SCRATCH)/geocal/a1/cc_mg_div.o ;\
	)
$(d0root)/stp/geocal.a(cc_sunder942.o):\
  $(d0root)/stp/geocal/cc_survey_cylinder.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clin.params\
  $(d0library)/params/clga.params\
  $(d0library)/links/izcsyl.link\
  $(d0library)/links/izcmdl.link\
  $(d0library)/links/izcthe.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclga.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cc_survey_cylinder.f ;\
	mv cc_survey_cylinder.o $(SCRATCH)/geocal/a1/cc_sunder942.o ;\
	)
$(d0root)/stp/geocal.a(cc_su_fit407.o):\
  $(d0root)/stp/geocal/cc_survey_fit.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clin.params\
  $(d0library)/params/clga.params\
  $(d0library)/links/izcsyl.link\
  $(d0library)/links/izcmdl.link\
  $(d0library)/links/izcthe.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclga.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cc_survey_fit.f ;\
	mv cc_survey_fit.o $(SCRATCH)/geocal/a1/cc_su_fit407.o ;\
	)
$(d0root)/stp/geocal.a(cc_suebra926.o):\
  $(d0root)/stp/geocal/cc_survey_in_zebra.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cc_survey_in_zebra.f ;\
	mv cc_survey_in_zebra.o $(SCRATCH)/geocal/a1/cc_suebra926.o ;\
	)
$(d0root)/stp/geocal.a(cc_sudule730.o):\
  $(d0root)/stp/geocal/cc_survey_module.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clin.params\
  $(d0library)/params/clga.params\
  $(d0library)/links/izcsyl.link\
  $(d0library)/links/izcmdl.link\
  $(d0library)/links/izcthe.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclga.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cc_survey_module.f ;\
	mv cc_survey_module.o $(SCRATCH)/geocal/a2/cc_sudule730.o ;\
	)
$(d0root)/stp/geocal.a(cc_thanks767.o):\
  $(d0root)/stp/geocal/cc_theo_geo_banks.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/links/izcsyl.link\
  $(d0library)/links/izcmdl.link\
  $(d0library)/links/izcthe.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclga.link\
  $(d0library)/links/izclgi.link\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cc_theo_geo_banks.f ;\
	mv cc_theo_geo_banks.o $(SCRATCH)/geocal/a2/cc_thanks767.o ;\
	)
$(d0root)/stp/geocal.a(cc_wruset507.o):\
  $(d0root)/stp/geocal/cc_write_iuset.f\
  $(d0library)/inc/cc_iuset.inc\
  $(d0library)/inc/cc_unit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cc_write_iuset.f ;\
	mv cc_write_iuset.o $(SCRATCH)/geocal/a2/cc_wruset507.o ;\
	)
$(d0root)/stp/geocal.a(cc_wrials915.o):\
  $(d0root)/stp/geocal/cc_write_materials.f\
  $(d0library)/inc/cc_material_list.inc\
  $(d0library)/inc/cc_material.inc\
  $(d0library)/inc/cc_unit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cc_write_materials.f ;\
	mv cc_write_materials.o $(SCRATCH)/geocal/a2/cc_wrials915.o ;\
	)
$(d0root)/stp/geocal.a(cc_wrices809.o):\
  $(d0root)/stp/geocal/cc_write_matrices.f\
  $(d0library)/inc/cc_matrix_list.inc\
  $(d0library)/inc/cc_matrix.inc\
  $(d0library)/inc/cc_unit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cc_write_matrices.f ;\
	mv cc_write_matrices.o $(SCRATCH)/geocal/a2/cc_wrices809.o ;\
	)
$(d0root)/stp/geocal.a(cc_wrlume617.o):\
  $(d0root)/stp/geocal/cc_write_volume.f\
  $(d0library)/inc/cc_srcp_volume.inc\
  $(d0library)/inc/cc_unit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cc_write_volume.f ;\
	mv cc_write_volume.o $(SCRATCH)/geocal/a2/cc_wrlume617.o ;\
	)
$(d0root)/stp/geocal.a(ccch_late441.o):\
  $(d0root)/stp/geocal/ccch_backplate.f\
  $(d0library)/inc/sccch_backplate.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccch_backplate.f ;\
	mv ccch_backplate.o $(SCRATCH)/geocal/a2/ccch_late441.o ;\
	)
$(d0root)/stp/geocal.a(ccch_cells.o):\
  $(d0root)/stp/geocal/ccch_cells.f\
  $(d0library)/inc/sccch_floor.inc\
  $(d0library)/inc/write_unit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccch_cells.f ;\
	mv ccch_cells.o $(SCRATCH)/geocal/a2/ccch_cells.o ;\
	)
$(d0root)/stp/geocal.a(ccch_crack.o):\
  $(d0root)/stp/geocal/ccch_crack.f\
  $(d0library)/inc/sccch_crack.inc\
  $(d0library)/inc/sccch_floor.inc\
  $(d0library)/inc/sccch_endplate.inc\
  $(d0library)/inc/sccch_frontplate.inc\
  $(d0library)/inc/sccch_backplate.inc\
  $(d0library)/inc/sccch_module.inc\
  $(d0library)/inc/cc_material_codes.inc\
  $(d0library)/inc/material.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccch_crack.f ;\
	mv ccch_crack.o $(SCRATCH)/geocal/a2/ccch_crack.o ;\
	)
$(d0root)/stp/geocal.a(ccch_late351.o):\
  $(d0root)/stp/geocal/ccch_endplate.f\
  $(d0library)/inc/sccch_crack.inc\
  $(d0library)/inc/sccch_endplate.inc\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccch_endplate.f ;\
	mv ccch_endplate.o $(SCRATCH)/geocal/a2/ccch_late351.o ;\
	)
$(d0root)/stp/geocal.a(ccch_floor.o):\
  $(d0root)/stp/geocal/ccch_floor.f\
  $(d0library)/inc/sccch_crack.inc\
  $(d0library)/inc/sccch_floor.inc\
  $(d0library)/inc/sccch_module.inc\
  $(d0library)/inc/sccch_mr_beampipe.inc\
  $(d0library)/inc/cc_material_codes.inc\
  $(d0library)/inc/material.inc\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccch_floor.f ;\
	mv ccch_floor.o $(SCRATCH)/geocal/a2/ccch_floor.o ;\
	)
$(d0root)/stp/geocal.a(ccch_late593.o):\
  $(d0root)/stp/geocal/ccch_frontplate.f\
  $(d0library)/inc/sccch_frontplate.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccch_frontplate.f ;\
	mv ccch_frontplate.o $(SCRATCH)/geocal/a2/ccch_late593.o ;\
	)
$(d0root)/stp/geocal.a(ccch_geom.o):\
  $(d0root)/stp/geocal/ccch_geom.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccch_geom.f ;\
	mv ccch_geom.o $(SCRATCH)/geocal/a2/ccch_geom.o ;\
	)
$(d0root)/stp/geocal.a(ccch_module.o):\
  $(d0root)/stp/geocal/ccch_module.f\
  $(d0library)/inc/sccch_module.inc\
  $(d0library)/inc/sccch_mr_beampipe.inc\
  $(d0library)/inc/srcp_volume.inc\
  $(d0library)/inc/write_unit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccch_module.f ;\
	mv ccch_module.o $(SCRATCH)/geocal/a2/ccch_module.o ;\
	)
$(d0root)/stp/geocal.a(ccch_mother.o):\
  $(d0root)/stp/geocal/ccch_mother.f\
  $(d0library)/inc/sccch_mother.inc\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccch_mother.f ;\
	mv ccch_mother.o $(SCRATCH)/geocal/a2/ccch_mother.o ;\
	)
$(d0root)/stp/geocal.a(ccch_pipe659.o):\
  $(d0root)/stp/geocal/ccch_mr_beampipe.f\
  $(d0library)/inc/sccch_mr_beampipe.inc\
  $(d0library)/inc/srcp_volume.inc\
  $(d0library)/inc/matrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccch_mr_beampipe.f ;\
	mv ccch_mr_beampipe.o $(SCRATCH)/geocal/a2/ccch_pipe659.o ;\
	)
$(d0root)/stp/geocal.a(ccch_tout500.o):\
  $(d0root)/stp/geocal/ccch_mr_cutout.f\
  $(d0library)/inc/sccch_floor.inc\
  $(d0library)/inc/sccch_module.inc\
  $(d0library)/inc/sccch_mr_beampipe.inc\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccch_mr_cutout.f ;\
	mv ccch_mr_cutout.o $(SCRATCH)/geocal/a2/ccch_tout500.o ;\
	)
$(d0root)/stp/geocal.a(ccch_ices337.o):\
  $(d0root)/stp/geocal/ccch_rotation_matrices.f\
  $(d0library)/inc/matrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccch_rotation_matrices.f ;\
	mv ccch_rotation_matrices.o $(SCRATCH)/geocal/a2/ccch_ices337.o ;\
	)
$(d0root)/stp/geocal.a(ccem_late448.o):\
  $(d0root)/stp/geocal/ccem_backplate.f\
  $(d0library)/inc/sccem_backplate.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccem_backplate.f ;\
	mv ccem_backplate.o $(SCRATCH)/geocal/a2/ccem_late448.o ;\
	)
$(d0root)/stp/geocal.a(ccem_dule768.o):\
  $(d0root)/stp/geocal/ccem_basic_module.f\
  $(d0library)/inc/sccem_module.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccem_basic_module.f ;\
	mv ccem_basic_module.o $(SCRATCH)/geocal/a2/ccem_dule768.o ;\
	)
$(d0root)/stp/geocal.a(ccem_cells.o):\
  $(d0root)/stp/geocal/ccem_cells.f\
  $(d0library)/inc/sccem_floor.inc\
  $(d0library)/inc/write_unit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccem_cells.f ;\
	mv ccem_cells.o $(SCRATCH)/geocal/a3/ccem_cells.o ;\
	)
$(d0root)/stp/geocal.a(ccem_crack.o):\
  $(d0root)/stp/geocal/ccem_crack.f\
  $(d0library)/inc/sccem_crack.inc\
  $(d0library)/inc/sccem_floor.inc\
  $(d0library)/inc/sccem_endplate.inc\
  $(d0library)/inc/sccem_frontplate.inc\
  $(d0library)/inc/sccem_backplate.inc\
  $(d0library)/inc/sccem_module.inc\
  $(d0library)/inc/cc_material_codes.inc\
  $(d0library)/inc/material.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccem_crack.f ;\
	mv ccem_crack.o $(SCRATCH)/geocal/a3/ccem_crack.o ;\
	)
$(d0root)/stp/geocal.a(ccem_late358.o):\
  $(d0root)/stp/geocal/ccem_endplate.f\
  $(d0library)/inc/sccem_endplate.inc\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccem_endplate.f ;\
	mv ccem_endplate.o $(SCRATCH)/geocal/a3/ccem_late358.o ;\
	)
$(d0root)/stp/geocal.a(ccem_uset007.o):\
  $(d0root)/stp/geocal/ccem_endplate_iuset.f\
  $(d0library)/inc/iuset.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccem_endplate_iuset.f ;\
	mv ccem_endplate_iuset.o $(SCRATCH)/geocal/a3/ccem_uset007.o ;\
	)
$(d0root)/stp/geocal.a(ccem_floor.o):\
  $(d0root)/stp/geocal/ccem_floor.f\
  $(d0library)/inc/sccem_crack.inc\
  $(d0library)/inc/sccem_floor.inc\
  $(d0library)/inc/sccem_module.inc\
  $(d0library)/inc/cc_material_codes.inc\
  $(d0library)/inc/material.inc\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccem_floor.f ;\
	mv ccem_floor.o $(SCRATCH)/geocal/a3/ccem_floor.o ;\
	)
$(d0root)/stp/geocal.a(ccem_late600.o):\
  $(d0root)/stp/geocal/ccem_frontplate.f\
  $(d0library)/inc/sccem_frontplate.inc\
  $(d0library)/inc/sccem_module.inc\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccem_frontplate.f ;\
	mv ccem_frontplate.o $(SCRATCH)/geocal/a3/ccem_late600.o ;\
	)
$(d0root)/stp/geocal.a(ccem_geom.o):\
  $(d0root)/stp/geocal/ccem_geom.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccem_geom.f ;\
	mv ccem_geom.o $(SCRATCH)/geocal/a3/ccem_geom.o ;\
	)
$(d0root)/stp/geocal.a(ccem_geom467.o):\
  $(d0root)/stp/geocal/ccem_homo_geom.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccem_homo_geom.f ;\
	mv ccem_homo_geom.o $(SCRATCH)/geocal/a3/ccem_geom467.o ;\
	)
$(d0root)/stp/geocal.a(ccem_uset597.o):\
  $(d0root)/stp/geocal/ccem_homo_iuset.f\
  $(d0library)/inc/iuset.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccem_homo_iuset.f ;\
	mv ccem_homo_iuset.o $(SCRATCH)/geocal/a3/ccem_uset597.o ;\
	)
$(d0root)/stp/geocal.a(ccem_module.o):\
  $(d0root)/stp/geocal/ccem_module.f\
  $(d0library)/inc/sccem_module.inc\
  $(d0library)/inc/srcp_volume.inc\
  $(d0library)/inc/write_unit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccem_module.f ;\
	mv ccem_module.o $(SCRATCH)/geocal/a3/ccem_module.o ;\
	)
$(d0root)/stp/geocal.a(ccem_mother.o):\
  $(d0root)/stp/geocal/ccem_mother.f\
  $(d0library)/inc/sccem_mother.inc\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccem_mother.f ;\
	mv ccem_mother.o $(SCRATCH)/geocal/a3/ccem_mother.o ;\
	)
$(d0root)/stp/geocal.a(ccem_geom566.o):\
  $(d0root)/stp/geocal/ccem_plate_geom.f\
  $(d0library)/inc/sccem_module.inc\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccem_plate_geom.f ;\
	mv ccem_plate_geom.o $(SCRATCH)/geocal/a3/ccem_geom566.o ;\
	)
$(d0root)/stp/geocal.a(ccem_uset696.o):\
  $(d0root)/stp/geocal/ccem_plate_iuset.f\
  $(d0library)/inc/iuset.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccem_plate_iuset.f ;\
	mv ccem_plate_iuset.o $(SCRATCH)/geocal/a3/ccem_uset696.o ;\
	)
$(d0root)/stp/geocal.a(ccem_ices344.o):\
  $(d0root)/stp/geocal/ccem_rotation_matrices.f\
  $(d0library)/inc/matrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccem_rotation_matrices.f ;\
	mv ccem_rotation_matrices.o $(SCRATCH)/geocal/a3/ccem_ices344.o ;\
	)
$(d0root)/stp/geocal.a(ccem_geom469.o):\
  $(d0root)/stp/geocal/ccem_skin_geom.f\
  $(d0library)/inc/sccem_module.inc\
  $(d0library)/inc/matrix.inc\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccem_skin_geom.f ;\
	mv ccem_skin_geom.o $(SCRATCH)/geocal/a3/ccem_geom469.o ;\
	)
$(d0root)/stp/geocal.a(ccem_uset599.o):\
  $(d0root)/stp/geocal/ccem_skin_iuset.f\
  $(d0library)/inc/iuset.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccem_skin_iuset.f ;\
	mv ccem_skin_iuset.o $(SCRATCH)/geocal/a3/ccem_uset599.o ;\
	)
$(d0root)/stp/geocal.a(ccfh_late444.o):\
  $(d0root)/stp/geocal/ccfh_backplate.f\
  $(d0library)/inc/sccfh_backplate.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccfh_backplate.f ;\
	mv ccfh_backplate.o $(SCRATCH)/geocal/a3/ccfh_late444.o ;\
	)
$(d0root)/stp/geocal.a(ccfh_cells.o):\
  $(d0root)/stp/geocal/ccfh_cells.f\
  $(d0library)/inc/sccfh_floor.inc\
  $(d0library)/inc/write_unit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccfh_cells.f ;\
	mv ccfh_cells.o $(SCRATCH)/geocal/a3/ccfh_cells.o ;\
	)
$(d0root)/stp/geocal.a(ccfh_crack.o):\
  $(d0root)/stp/geocal/ccfh_crack.f\
  $(d0library)/inc/sccfh_crack.inc\
  $(d0library)/inc/sccfh_floor.inc\
  $(d0library)/inc/sccfh_endplate.inc\
  $(d0library)/inc/sccfh_frontplate.inc\
  $(d0library)/inc/sccfh_backplate.inc\
  $(d0library)/inc/sccfh_module.inc\
  $(d0library)/inc/cc_material_codes.inc\
  $(d0library)/inc/material.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccfh_crack.f ;\
	mv ccfh_crack.o $(SCRATCH)/geocal/a3/ccfh_crack.o ;\
	)
$(d0root)/stp/geocal.a(ccfh_late354.o):\
  $(d0root)/stp/geocal/ccfh_endplate.f\
  $(d0library)/inc/sccfh_crack.inc\
  $(d0library)/inc/sccfh_endplate.inc\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccfh_endplate.f ;\
	mv ccfh_endplate.o $(SCRATCH)/geocal/a3/ccfh_late354.o ;\
	)
$(d0root)/stp/geocal.a(ccfh_floor.o):\
  $(d0root)/stp/geocal/ccfh_floor.f\
  $(d0library)/inc/sccfh_crack.inc\
  $(d0library)/inc/sccfh_floor.inc\
  $(d0library)/inc/sccfh_module.inc\
  $(d0library)/inc/cc_material_codes.inc\
  $(d0library)/inc/material.inc\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccfh_floor.f ;\
	mv ccfh_floor.o $(SCRATCH)/geocal/a4/ccfh_floor.o ;\
	)
$(d0root)/stp/geocal.a(ccfh_late596.o):\
  $(d0root)/stp/geocal/ccfh_frontplate.f\
  $(d0library)/inc/sccfh_frontplate.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccfh_frontplate.f ;\
	mv ccfh_frontplate.o $(SCRATCH)/geocal/a4/ccfh_late596.o ;\
	)
$(d0root)/stp/geocal.a(ccfh_geom.o):\
  $(d0root)/stp/geocal/ccfh_geom.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccfh_geom.f ;\
	mv ccfh_geom.o $(SCRATCH)/geocal/a4/ccfh_geom.o ;\
	)
$(d0root)/stp/geocal.a(ccfh_module.o):\
  $(d0root)/stp/geocal/ccfh_module.f\
  $(d0library)/inc/sccfh_module.inc\
  $(d0library)/inc/srcp_volume.inc\
  $(d0library)/inc/write_unit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccfh_module.f ;\
	mv ccfh_module.o $(SCRATCH)/geocal/a4/ccfh_module.o ;\
	)
$(d0root)/stp/geocal.a(ccfh_mother.o):\
  $(d0root)/stp/geocal/ccfh_mother.f\
  $(d0library)/inc/sccfh_mother.inc\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccfh_mother.f ;\
	mv ccfh_mother.o $(SCRATCH)/geocal/a4/ccfh_mother.o ;\
	)
$(d0root)/stp/geocal.a(ccfh_ices340.o):\
  $(d0root)/stp/geocal/ccfh_rotation_matrices.f\
  $(d0library)/inc/matrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccfh_rotation_matrices.f ;\
	mv ccfh_rotation_matrices.o $(SCRATCH)/geocal/a4/ccfh_ices340.o ;\
	)
$(d0root)/stp/geocal.a(ccpad.o):\
  $(d0root)/stp/geocal/ccpad.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clay.def\
  $(d0library)/inc/scparr.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/sccch_floor.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ccpad.f ;\
	mv ccpad.o $(SCRATCH)/geocal/a4/ccpad.o ;\
	)
$(d0root)/stp/geocal.a(cedp.o):\
  $(d0root)/stp/geocal/cedp.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/calgeo.inc\
  $(d0library)/params/cedp.params\
  $(d0library)/links/izcedp.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cedp.f ;\
	mv cedp.o $(SCRATCH)/geocal/a4/cedp.o ;\
	)
$(d0root)/stp/geocal.a(celicd.o):\
  $(d0root)/stp/geocal/celicd.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/params/ceta.params\
  $(d0library)/params/clyr.params\
  $(d0library)/params/clga.params\
  $(d0library)/params/cal_offline.params\
  $(d0library)/links/izclay.link\
  $(d0library)/links/izclnk.link\
  $(d0library)/inc/clay.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/celicd.f ;\
	mv celicd.o $(SCRATCH)/geocal/a4/celicd.o ;\
	)
$(d0root)/stp/geocal.a(celpos.o):\
  $(d0root)/stp/geocal/celpos.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/region.def\
  $(d0library)/params/cedp.params\
  $(d0library)/params/ceta.params\
  $(d0library)/links/izceta.link\
  $(d0library)/links/izclyr.link\
  $(d0library)/params/clyr.params\
  $(d0library)/params/clga.params\
  $(d0library)/inc/clay.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def\
  $(d0library)/params/cal_offline.params\
  $(d0library)/inc/lclyr.inc\
  $(d0library)/inc/rminmax.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/celpos.f ;\
	mv celpos.o $(SCRATCH)/geocal/a4/celpos.o ;\
	)
$(d0root)/stp/geocal.a(celpsi.o):\
  $(d0root)/stp/geocal/celpsi.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clyr.params\
  $(d0library)/inc/clay.def\
  $(d0library)/inc/region.def\
  $(d0library)/params/cal_offline.params\
  $(d0library)/inc/lclyr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/celpsi.f ;\
	mv celpsi.o $(SCRATCH)/geocal/a4/celpsi.o ;\
	)
$(d0root)/stp/geocal.a(celsha.o):\
  $(d0root)/stp/geocal/celsha.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/region.def\
  $(d0library)/params/cedp.params\
  $(d0library)/params/ceta.params\
  $(d0library)/params/clyr.params\
  $(d0library)/params/clga.params\
  $(d0library)/inc/clay.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def\
  $(d0library)/params/cal_offline.params\
  $(d0library)/inc/lclyr.inc\
  $(d0library)/inc/rminmax.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/celsha.f ;\
	mv celsha.o $(SCRATCH)/geocal/a4/celsha.o ;\
	)
$(d0root)/stp/geocal.a(cenp.o):\
  $(d0root)/stp/geocal/cenp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cenp.f ;\
	mv cenp.o $(SCRATCH)/geocal/a4/cenp.o ;\
	)
$(d0root)/stp/geocal.a(cenpcc.o):\
  $(d0root)/stp/geocal/cenpcc.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/calgeo.inc\
  $(d0library)/inc/clga.def\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cenpcc.f ;\
	mv cenpcc.o $(SCRATCH)/geocal/a4/cenpcc.o ;\
	)
$(d0root)/stp/geocal.a(cenpeh.o):\
  $(d0root)/stp/geocal/cenpeh.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/calgeo.inc\
  $(d0library)/inc/clga.def\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cenpeh.f ;\
	mv cenpeh.o $(SCRATCH)/geocal/a4/cenpeh.o ;\
	)
$(d0root)/stp/geocal.a(ceta.o):\
  $(d0root)/stp/geocal/ceta.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/cedp.params\
  $(d0library)/links/izcedp.link\
  $(d0library)/params/ceta.params\
  $(d0library)/links/izceta.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ceta.f ;\
	mv ceta.o $(SCRATCH)/geocal/a4/ceta.o ;\
	)
$(d0root)/stp/geocal.a(cgeh.o):\
  $(d0root)/stp/geocal/cgeh.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/cghead.def\
  $(d0library)/links/izcgeh.link\
  $(d0library)/links/izcsha.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cgeh.f ;\
	mv cgeh.o $(SCRATCH)/geocal/a4/cgeh.o ;\
	)
$(d0root)/stp/geocal.a(chaba.o):\
  $(d0root)/stp/geocal/chaba.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/chaba.f ;\
	mv chaba.o $(SCRATCH)/geocal/a4/chaba.o ;\
	)
$(d0root)/stp/geocal.a(clay.o):\
  $(d0root)/stp/geocal/clay.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/clay.f ;\
	mv clay.o $(SCRATCH)/geocal/a4/clay.o ;\
	)
$(d0root)/stp/geocal.a(claycc.o):\
  $(d0root)/stp/geocal/claycc.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/clay.def\
  $(d0library)/params/cmat.params\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izclga.link\
  $(d0library)/links/izclay.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izcsha.link\
  $(d0library)/links/izclnk.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/claycc.f ;\
	mv claycc.o $(SCRATCH)/geocal/a4/claycc.o ;\
	)
$(d0root)/stp/geocal.a(clayee.o):\
  $(d0root)/stp/geocal/clayee.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/calgeo.inc\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/params/cmat.params\
  $(d0library)/inc/clay.def\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izcsha.link\
  $(d0library)/links/izclga.link\
  $(d0library)/links/izclay.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclnk.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/clayee.f ;\
	mv clayee.o $(SCRATCH)/geocal/a5/clayee.o ;\
	)
$(d0root)/stp/geocal.a(clayih.o):\
  $(d0root)/stp/geocal/clayih.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clga.params\
  $(d0library)/params/cmat.params\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/clay.def\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izclay.link\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izcsha.link\
  $(d0library)/links/izclga.link\
  $(d0library)/links/izclnk.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/clayih.f ;\
	mv clayih.o $(SCRATCH)/geocal/a5/clayih.o ;\
	)
$(d0root)/stp/geocal.a(claymg_cc.o):\
  $(d0root)/stp/geocal/claymg_cc.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/region.def\
  $(d0library)/inc/pi.def\
  $(d0library)/params/clga.params\
  $(d0library)/inc/clay.def\
  $(d0library)/params/cmat.params\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izcmsg.link\
  $(d0library)/links/izclay.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclnk.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/claymg_cc.f ;\
	mv claymg_cc.o $(SCRATCH)/geocal/a5/claymg_cc.o ;\
	)
$(d0root)/stp/geocal.a(claymg_ec.o):\
  $(d0root)/stp/geocal/claymg_ec.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/region.def\
  $(d0library)/inc/pi.def\
  $(d0library)/params/clga.params\
  $(d0library)/inc/clay.def\
  $(d0library)/params/cmat.params\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izcmsg.link\
  $(d0library)/links/izclay.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclnk.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/claymg_ec.f ;\
	mv claymg_ec.o $(SCRATCH)/geocal/a5/claymg_ec.o ;\
	)
$(d0root)/stp/geocal.a(claymh.o):\
  $(d0root)/stp/geocal/claymh.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/params/cmat.params\
  $(d0library)/inc/clay.def\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izclay.link\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izcsha.link\
  $(d0library)/links/izclga.link\
  $(d0library)/links/izclnk.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/claymh.f ;\
	mv claymh.o $(SCRATCH)/geocal/a5/claymh.o ;\
	)
$(d0root)/stp/geocal.a(clayoh.o):\
  $(d0root)/stp/geocal/clayoh.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/params/cmat.params\
  $(d0library)/inc/clay.def\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izclay.link\
  $(d0library)/links/izclga.link\
  $(d0library)/links/izcsha.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izclnk.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/clayoh.f ;\
	mv clayoh.o $(SCRATCH)/geocal/a5/clayoh.o ;\
	)
$(d0root)/stp/geocal.a(clga.o):\
  $(d0root)/stp/geocal/clga.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/clga.f ;\
	mv clga.o $(SCRATCH)/geocal/a5/clga.o ;\
	)
$(d0root)/stp/geocal.a(clgacc.o):\
  $(d0root)/stp/geocal/clgacc.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izclga.link\
  $(d0library)/links/izclgi.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izcsha.link\
  $(d0library)/links/izclnk.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/clgacc.f ;\
	mv clgacc.o $(SCRATCH)/geocal/a5/clgacc.o ;\
	)
$(d0root)/stp/geocal.a(clgaee.o):\
  $(d0root)/stp/geocal/clgaee.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izclga.link\
  $(d0library)/links/izclgi.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izcsha.link\
  $(d0library)/links/izclnk.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/clgaee.f ;\
	mv clgaee.o $(SCRATCH)/geocal/a5/clgaee.o ;\
	)
$(d0root)/stp/geocal.a(clgaeh.o):\
  $(d0root)/stp/geocal/clgaeh.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izclga.link\
  $(d0library)/links/izclgi.link\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izcsha.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclnk.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/clgaeh.f ;\
	mv clgaeh.o $(SCRATCH)/geocal/a5/clgaeh.o ;\
	)
$(d0root)/stp/geocal.a(clgi.o):\
  $(d0root)/stp/geocal/clgi.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/clgi.f ;\
	mv clgi.o $(SCRATCH)/geocal/a5/clgi.o ;\
	)
$(d0root)/stp/geocal.a(clgicc.o):\
  $(d0root)/stp/geocal/clgicc.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clga.params\
  $(d0library)/params/material_types.params\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izclgi.link\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izcsha.link\
  $(d0library)/links/izcreg.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/clgicc.f ;\
	mv clgicc.o $(SCRATCH)/geocal/a5/clgicc.o ;\
	)
$(d0root)/stp/geocal.a(clgiee.o):\
  $(d0root)/stp/geocal/clgiee.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clga.params\
  $(d0library)/params/material_types.params\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izclgi.link\
  $(d0library)/links/izcsha.link\
  $(d0library)/links/izcreg.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/clgiee.f ;\
	mv clgiee.o $(SCRATCH)/geocal/a5/clgiee.o ;\
	)
$(d0root)/stp/geocal.a(clgieh.o):\
  $(d0root)/stp/geocal/clgieh.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclga.link\
  $(d0library)/links/izclgi.link\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izcsha.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/clgieh.f ;\
	mv clgieh.o $(SCRATCH)/geocal/a5/clgieh.o ;\
	)
$(d0root)/stp/geocal.a(clgsrc.o):\
  $(d0root)/stp/geocal/clgsrc.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/scparr.inc\
  $(d0library)/params/clga.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/clgsrc.f ;\
	mv clgsrc.o $(SCRATCH)/geocal/a5/clgsrc.o ;\
	)
$(d0root)/stp/geocal.a(clrrcp.o):\
  $(d0root)/stp/geocal/clrrcp.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/scparr.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/params/clga.params\
  $(d0library)/params/clyr.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/clrrcp.f ;\
	mv clrrcp.o $(SCRATCH)/geocal/a5/clrrcp.o ;\
	)
$(d0root)/stp/geocal.a(clyr.o):\
  $(d0root)/stp/geocal/clyr.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/region.def\
  $(d0library)/params/cedp.params\
  $(d0library)/params/ceta.params\
  $(d0library)/links/izceta.link\
  $(d0library)/links/izclay.link\
  $(d0library)/links/izclyr.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclga.link\
  $(d0library)/params/clyr.params\
  $(d0library)/params/clga.params\
  $(d0library)/inc/clay.def\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/lclyr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/clyr.f ;\
	mv clyr.o $(SCRATCH)/geocal/a5/clyr.o ;\
	)
$(d0root)/stp/geocal.a(cmat.o):\
  $(d0root)/stp/geocal/cmat.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/cmat.params\
  $(d0library)/links/izcmat.link\
  $(d0library)/inc/scparr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cmat.f ;\
	mv cmat.o $(SCRATCH)/geocal/a5/cmat.o ;\
	)
$(d0root)/stp/geocal.a(cmsgcc.o):\
  $(d0root)/stp/geocal/cmsgcc.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/region.def\
  $(d0library)/inc/pi.def\
  $(d0library)/params/clga.params\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izcmsg.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izcsha.link\
  $(d0library)/links/izclnk.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cmsgcc.f ;\
	mv cmsgcc.o $(SCRATCH)/geocal/a5/cmsgcc.o ;\
	)
$(d0root)/stp/geocal.a(cmsgec.o):\
  $(d0root)/stp/geocal/cmsgec.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/region.def\
  $(d0library)/inc/pi.def\
  $(d0library)/params/clga.params\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izcmsg.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izcsha.link\
  $(d0library)/links/izclnk.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cmsgec.f ;\
	mv cmsgec.o $(SCRATCH)/geocal/a5/cmsgec.o ;\
	)
$(d0root)/stp/geocal.a(cold_shrink.o):\
  $(d0root)/stp/geocal/cold_shrink.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cold_shrink.f ;\
	mv cold_shrink.o $(SCRATCH)/geocal/a6/cold_shrink.o ;\
	)
$(d0root)/stp/geocal.a(cpla.o):\
  $(d0root)/stp/geocal/cpla.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cpla.f ;\
	mv cpla.o $(SCRATCH)/geocal/a6/cpla.o ;\
	)
$(d0root)/stp/geocal.a(cprzy.o):\
  $(d0root)/stp/geocal/cprzy.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cprzy.f ;\
	mv cprzy.o $(SCRATCH)/geocal/a6/cprzy.o ;\
	)
$(d0root)/stp/geocal.a(creg.o):\
  $(d0root)/stp/geocal/creg.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/cghead.def\
  $(d0library)/inc/region.def\
  $(d0library)/links/izcreg.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/creg.f ;\
	mv creg.o $(SCRATCH)/geocal/a6/creg.o ;\
	)
$(d0root)/stp/geocal.a(crscel.o):\
  $(d0root)/stp/geocal/crscel.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/clay.def\
  $(d0library)/params/clyr.params\
  $(d0library)/params/cal_offline.params\
  $(d0library)/inc/lclyr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/crscel.f ;\
	mv crscel.o $(SCRATCH)/geocal/a6/crscel.o ;\
	)
$(d0root)/stp/geocal.a(crscel_eh.o):\
  $(d0root)/stp/geocal/crscel_eh.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/clay.def\
  $(d0library)/params/clyr.params\
  $(d0library)/params/cal_offline.params\
  $(d0library)/inc/lclyr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/crscel_eh.f ;\
	mv crscel_eh.o $(SCRATCH)/geocal/a6/crscel_eh.o ;\
	)
$(d0root)/stp/geocal.a(crscel_oh.o):\
  $(d0root)/stp/geocal/crscel_oh.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/clay.def\
  $(d0library)/params/clyr.params\
  $(d0library)/params/cal_offline.params\
  $(d0library)/inc/lclyr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/crscel_oh.f ;\
	mv crscel_oh.o $(SCRATCH)/geocal/a6/crscel_oh.o ;\
	)
$(d0root)/stp/geocal.a(crycen.o):\
  $(d0root)/stp/geocal/crycen.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/crycen.f ;\
	mv crycen.o $(SCRATCH)/geocal/a6/crycen.o ;\
	)
$(d0root)/stp/geocal.a(crycnt.o):\
  $(d0root)/stp/geocal/crycnt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/crycnt.f ;\
	mv crycnt.o $(SCRATCH)/geocal/a6/crycnt.o ;\
	)
$(d0root)/stp/geocal.a(crydrw.o):\
  $(d0root)/stp/geocal/crydrw.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/crydrw.f ;\
	mv crydrw.o $(SCRATCH)/geocal/a6/crydrw.o ;\
	)
$(d0root)/stp/geocal.a(cryenc.o):\
  $(d0root)/stp/geocal/cryenc.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cryenc.f ;\
	mv cryenc.o $(SCRATCH)/geocal/a6/cryenc.o ;\
	)
$(d0root)/stp/geocal.a(cryend.o):\
  $(d0root)/stp/geocal/cryend.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cryend.f ;\
	mv cryend.o $(SCRATCH)/geocal/a6/cryend.o ;\
	)
$(d0root)/stp/geocal.a(cryo.o):\
  $(d0root)/stp/geocal/cryo.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cryo.f ;\
	mv cryo.o $(SCRATCH)/geocal/a6/cryo.o ;\
	)
$(d0root)/stp/geocal.a(cryocc.o):\
  $(d0root)/stp/geocal/cryocc.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/calgeo.inc\
  $(d0library)/inc/clga.def\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cryocc.f ;\
	mv cryocc.o $(SCRATCH)/geocal/a6/cryocc.o ;\
	)
$(d0root)/stp/geocal.a(cryoec.o):\
  $(d0root)/stp/geocal/cryoec.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/calgeo.inc\
  $(d0library)/inc/clga.def\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cryoec.f ;\
	mv cryoec.o $(SCRATCH)/geocal/a6/cryoec.o ;\
	)
$(d0root)/stp/geocal.a(cryout.o):\
  $(d0root)/stp/geocal/cryout.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cryout.f ;\
	mv cryout.o $(SCRATCH)/geocal/a6/cryout.o ;\
	)
$(d0root)/stp/geocal.a(cryplt.o):\
  $(d0root)/stp/geocal/cryplt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cryplt.f ;\
	mv cryplt.o $(SCRATCH)/geocal/a6/cryplt.o ;\
	)
$(d0root)/stp/geocal.a(cryprt.o):\
  $(d0root)/stp/geocal/cryprt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cryprt.f ;\
	mv cryprt.o $(SCRATCH)/geocal/a6/cryprt.o ;\
	)
$(d0root)/stp/geocal.a(cryset.o):\
  $(d0root)/stp/geocal/cryset.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cryset.f ;\
	mv cryset.o $(SCRATCH)/geocal/a6/cryset.o ;\
	)
$(d0root)/stp/geocal.a(cscn.o):\
  $(d0root)/stp/geocal/cscn.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/scparr.inc\
  $(d0library)/params/clga.params\
  $(d0library)/inc/clay.def\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izcsha.link\
  $(d0library)/links/izcscn.link\
  $(d0library)/links/izclay.link\
  $(d0library)/links/izclnk.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/cscn.f ;\
	mv cscn.o $(SCRATCH)/geocal/a6/cscn.o ;\
	)
$(d0root)/stp/geocal.a(csfmake.o):\
  $(d0root)/stp/geocal/csfmake.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/params/cal_offline.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/csfmake.f ;\
	mv csfmake.o $(SCRATCH)/geocal/a7/csfmake.o ;\
	)
$(d0root)/stp/geocal.a(do_em_geom.o):\
  $(d0root)/stp/geocal/do_em_geom.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/do_em_geom.f ;\
	mv do_em_geom.o $(SCRATCH)/geocal/a7/do_em_geom.o ;\
	)
$(d0root)/stp/geocal.a(do_ih_geom.o):\
  $(d0root)/stp/geocal/do_ih_geom.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/do_ih_geom.f ;\
	mv do_ih_geom.o $(SCRATCH)/geocal/a7/do_ih_geom.o ;\
	)
$(d0root)/stp/geocal.a(do_mh_geom.o):\
  $(d0root)/stp/geocal/do_mh_geom.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/do_mh_geom.f ;\
	mv do_mh_geom.o $(SCRATCH)/geocal/a7/do_mh_geom.o ;\
	)
$(d0root)/stp/geocal.a(do_misc.o):\
  $(d0root)/stp/geocal/do_misc.f\
  $(d0library)/params/srcp.def\
  $(d0library)/inc/lksrcp.inc\
  $(d0library)/inc/bfsrcp.inc\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/do_misc.f ;\
	mv do_misc.o $(SCRATCH)/geocal/a7/do_misc.o ;\
	)
$(d0root)/stp/geocal.a(do_oh_geom.o):\
  $(d0root)/stp/geocal/do_oh_geom.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/do_oh_geom.f ;\
	mv do_oh_geom.o $(SCRATCH)/geocal/a7/do_oh_geom.o ;\
	)
$(d0root)/stp/geocal.a(ec_iusets.o):\
  $(d0root)/stp/geocal/ec_iusets.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ec_iusets.f ;\
	mv ec_iusets.o $(SCRATCH)/geocal/a7/ec_iusets.o ;\
	)
$(d0root)/stp/geocal.a(ec_mixtures.o):\
  $(d0root)/stp/geocal/ec_mixtures.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ec_mixtures.f ;\
	mv ec_mixtures.o $(SCRATCH)/geocal/a7/ec_mixtures.o ;\
	)
$(d0root)/stp/geocal.a(ec_roices597.o):\
  $(d0root)/stp/geocal/ec_rot_matrices.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ec_rot_matrices.f ;\
	mv ec_rot_matrices.o $(SCRATCH)/geocal/a7/ec_roices597.o ;\
	)
$(d0root)/stp/geocal.a(ec_su_fit409.o):\
  $(d0root)/stp/geocal/ec_survey_fit.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clin.params\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def\
  $(d0library)/links/izcsyl.link\
  $(d0library)/links/izcmdl.link\
  $(d0library)/links/izcthe.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclga.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ec_survey_fit.f ;\
	mv ec_survey_fit.o $(SCRATCH)/geocal/a7/ec_su_fit409.o ;\
	)
$(d0root)/stp/geocal.a(ecem_geom469.o):\
  $(d0root)/stp/geocal/ecem_homo_geom.f\
  $(d0library)/inc/srcp_volume.inc\
  $(d0library)/inc/material.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecem_homo_geom.f ;\
	mv ecem_homo_geom.o $(SCRATCH)/geocal/a7/ecem_geom469.o ;\
	)
$(d0root)/stp/geocal.a(ecem_uset599.o):\
  $(d0root)/stp/geocal/ecem_homo_iuset.f\
  $(d0library)/inc/iuset.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecem_homo_iuset.f ;\
	mv ecem_homo_iuset.o $(SCRATCH)/geocal/a7/ecem_uset599.o ;\
	)
$(d0root)/stp/geocal.a(ecem_umes824.o):\
  $(d0root)/stp/geocal/ecem_homo_volumes.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecem_homo_volumes.f ;\
	mv ecem_homo_volumes.o $(SCRATCH)/geocal/a7/ecem_umes824.o ;\
	)
$(d0root)/stp/geocal.a(ecem_geom462.o):\
  $(d0root)/stp/geocal/ecem_misc_geom.f\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecem_misc_geom.f ;\
	mv ecem_misc_geom.o $(SCRATCH)/geocal/a7/ecem_geom462.o ;\
	)
$(d0root)/stp/geocal.a(ecem_loor690.o):\
  $(d0root)/stp/geocal/ecem_plate_floor.f\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecem_plate_floor.f ;\
	mv ecem_plate_floor.o $(SCRATCH)/geocal/a7/ecem_loor690.o ;\
	)
$(d0root)/stp/geocal.a(ecem_geom568.o):\
  $(d0root)/stp/geocal/ecem_plate_geom.f\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecem_plate_geom.f ;\
	mv ecem_plate_geom.o $(SCRATCH)/geocal/a7/ecem_geom568.o ;\
	)
$(d0root)/stp/geocal.a(ecem_uset698.o):\
  $(d0root)/stp/geocal/ecem_plate_iuset.f\
  $(d0library)/inc/iuset.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecem_plate_iuset.f ;\
	mv ecem_plate_iuset.o $(SCRATCH)/geocal/a7/ecem_uset698.o ;\
	)
$(d0root)/stp/geocal.a(ecem_umes923.o):\
  $(d0root)/stp/geocal/ecem_plate_volumes.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecem_plate_volumes.f ;\
	mv ecem_plate_volumes.o $(SCRATCH)/geocal/a7/ecem_umes923.o ;\
	)
$(d0root)/stp/geocal.a(ecem_geom595.o):\
  $(d0root)/stp/geocal/ecem_tower_geom.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecem_tower_geom.f ;\
	mv ecem_tower_geom.o $(SCRATCH)/geocal/a7/ecem_geom595.o ;\
	)
$(d0root)/stp/geocal.a(ecih_geom975.o):\
  $(d0root)/stp/geocal/ecih_argon_gap_geom.f\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecih_argon_gap_geom.f ;\
	mv ecih_argon_gap_geom.o $(SCRATCH)/geocal/a7/ecih_geom975.o ;\
	)
$(d0root)/stp/geocal.a(ecih_geom063.o):\
  $(d0root)/stp/geocal/ecih_back_plate_geom.f\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecih_back_plate_geom.f ;\
	mv ecih_back_plate_geom.o $(SCRATCH)/geocal/a8/ecih_geom063.o ;\
	)
$(d0root)/stp/geocal.a(ecih_dule769.o):\
  $(d0root)/stp/geocal/ecih_basic_module.f\
  $(d0library)/inc/ecih_module.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecih_basic_module.f ;\
	mv ecih_basic_module.o $(SCRATCH)/geocal/a8/ecih_dule769.o ;\
	)
$(d0root)/stp/geocal.a(ecih_geom075.o):\
  $(d0root)/stp/geocal/ecih_coarse_abs_geom.f\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecih_coarse_abs_geom.f ;\
	mv ecih_coarse_abs_geom.o $(SCRATCH)/geocal/a8/ecih_geom075.o ;\
	)
$(d0root)/stp/geocal.a(ecih_geom856.o):\
  $(d0root)/stp/geocal/ecih_fine_abs_geom.f\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecih_fine_abs_geom.f ;\
	mv ecih_fine_abs_geom.o $(SCRATCH)/geocal/a8/ecih_geom856.o ;\
	)
$(d0root)/stp/geocal.a(ecih_geom215.o):\
  $(d0root)/stp/geocal/ecih_front_plate_geom.f\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecih_front_plate_geom.f ;\
	mv ecih_front_plate_geom.o $(SCRATCH)/geocal/a8/ecih_geom215.o ;\
	)
$(d0root)/stp/geocal.a(ecih_geom468.o):\
  $(d0root)/stp/geocal/ecih_homo_geom.f\
  $(d0library)/inc/ecih_module.inc\
  $(d0library)/inc/material.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecih_homo_geom.f ;\
	mv ecih_homo_geom.o $(SCRATCH)/geocal/a8/ecih_geom468.o ;\
	)
$(d0root)/stp/geocal.a(ecih_uset598.o):\
  $(d0root)/stp/geocal/ecih_homo_iuset.f\
  $(d0library)/inc/iuset.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecih_homo_iuset.f ;\
	mv ecih_homo_iuset.o $(SCRATCH)/geocal/a8/ecih_uset598.o ;\
	)
$(d0root)/stp/geocal.a(ecih_umes823.o):\
  $(d0root)/stp/geocal/ecih_homo_volumes.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecih_homo_volumes.f ;\
	mv ecih_homo_volumes.o $(SCRATCH)/geocal/a8/ecih_umes823.o ;\
	)
$(d0root)/stp/geocal.a(ecih_geom461.o):\
  $(d0root)/stp/geocal/ecih_misc_geom.f\
  $(d0library)/inc/ecih_module.inc\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecih_misc_geom.f ;\
	mv ecih_misc_geom.o $(SCRATCH)/geocal/a8/ecih_geom461.o ;\
	)
$(d0root)/stp/geocal.a(ecih_geom348.o):\
  $(d0root)/stp/geocal/ecih_mlb_geom.f\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecih_mlb_geom.f ;\
	mv ecih_mlb_geom.o $(SCRATCH)/geocal/a8/ecih_geom348.o ;\
	)
$(d0root)/stp/geocal.a(ecih_geom567.o):\
  $(d0root)/stp/geocal/ecih_plate_geom.f\
  $(d0library)/inc/ecih_module.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecih_plate_geom.f ;\
	mv ecih_plate_geom.o $(SCRATCH)/geocal/a8/ecih_geom567.o ;\
	)
$(d0root)/stp/geocal.a(ecih_uset697.o):\
  $(d0root)/stp/geocal/ecih_plate_iuset.f\
  $(d0library)/inc/iuset.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecih_plate_iuset.f ;\
	mv ecih_plate_iuset.o $(SCRATCH)/geocal/a8/ecih_uset697.o ;\
	)
$(d0root)/stp/geocal.a(ecih_umes922.o):\
  $(d0root)/stp/geocal/ecih_plate_volumes.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecih_plate_volumes.f ;\
	mv ecih_plate_volumes.o $(SCRATCH)/geocal/a8/ecih_umes922.o ;\
	)
$(d0root)/stp/geocal.a(ecih_geom355.o):\
  $(d0root)/stp/geocal/ecih_support_pipe_geom.f\
  $(d0library)/inc/ecih_module.inc\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecih_support_pipe_geom.f ;\
	mv ecih_support_pipe_geom.o $(SCRATCH)/geocal/a8/ecih_geom355.o ;\
	)
$(d0root)/stp/geocal.a(ecih_geom459.o):\
  $(d0root)/stp/geocal/ecih_support_plate_geom.f\
  $(d0library)/inc/srcp_volume.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecih_support_plate_geom.f ;\
	mv ecih_support_plate_geom.o $(SCRATCH)/geocal/a8/ecih_geom459.o ;\
	)
$(d0root)/stp/geocal.a(ecmh_dule773.o):\
  $(d0root)/stp/geocal/ecmh_basic_module.f\
  $(d0library)/inc/srcp_volume.inc\
  $(d0library)/inc/write_unit.inc\
  $(d0library)/inc/matrix.inc\
  $(d0library)/inc/iuset.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecmh_basic_module.f ;\
	mv ecmh_basic_module.o $(SCRATCH)/geocal/a8/ecmh_dule773.o ;\
	)
$(d0root)/stp/geocal.a(ecmh_geom472.o):\
  $(d0root)/stp/geocal/ecmh_homo_geom.f\
  $(d0library)/inc/srcp_volume.inc\
  $(d0library)/inc/write_unit.inc\
  $(d0library)/inc/material.inc\
  $(d0library)/inc/matrix.inc\
  $(d0library)/inc/iuset.inc\
  $(d0library)/inc/ecmh_module.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecmh_homo_geom.f ;\
	mv ecmh_homo_geom.o $(SCRATCH)/geocal/a8/ecmh_geom472.o ;\
	)
$(d0root)/stp/geocal.a(ecmh_uset602.o):\
  $(d0root)/stp/geocal/ecmh_homo_iuset.f\
  $(d0library)/inc/srcp_volume.inc\
  $(d0library)/inc/iuset.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecmh_homo_iuset.f ;\
	mv ecmh_homo_iuset.o $(SCRATCH)/geocal/a8/ecmh_uset602.o ;\
	)
$(d0root)/stp/geocal.a(ecmh_umes827.o):\
  $(d0root)/stp/geocal/ecmh_homo_volumes.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecmh_homo_volumes.f ;\
	mv ecmh_homo_volumes.o $(SCRATCH)/geocal/a8/ecmh_umes827.o ;\
	)
$(d0root)/stp/geocal.a(ecmh_geom465.o):\
  $(d0root)/stp/geocal/ecmh_misc_geom.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecmh_misc_geom.f ;\
	mv ecmh_misc_geom.o $(SCRATCH)/geocal/a8/ecmh_geom465.o ;\
	)
$(d0root)/stp/geocal.a(ecmh_geom571.o):\
  $(d0root)/stp/geocal/ecmh_plate_geom.f\
  $(d0library)/inc/srcp_volume.inc\
  $(d0library)/inc/write_unit.inc\
  $(d0library)/inc/matrix.inc\
  $(d0library)/inc/iuset.inc\
  $(d0library)/inc/ecmh_module.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecmh_plate_geom.f ;\
	mv ecmh_plate_geom.o $(SCRATCH)/geocal/a9/ecmh_geom571.o ;\
	)
$(d0root)/stp/geocal.a(ecmh_uset701.o):\
  $(d0root)/stp/geocal/ecmh_plate_iuset.f\
  $(d0library)/inc/srcp_volume.inc\
  $(d0library)/inc/iuset.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecmh_plate_iuset.f ;\
	mv ecmh_plate_iuset.o $(SCRATCH)/geocal/a9/ecmh_uset701.o ;\
	)
$(d0root)/stp/geocal.a(ecmh_umes926.o):\
  $(d0root)/stp/geocal/ecmh_plate_volumes.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecmh_plate_volumes.f ;\
	mv ecmh_plate_volumes.o $(SCRATCH)/geocal/a9/ecmh_umes926.o ;\
	)
$(d0root)/stp/geocal.a(ecmh_unit.o):\
  $(d0root)/stp/geocal/ecmh_unit.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecmh_unit.f ;\
	mv ecmh_unit.o $(SCRATCH)/geocal/a9/ecmh_unit.o ;\
	)
$(d0root)/stp/geocal.a(ecn_slyze952.o):\
  $(d0root)/stp/geocal/ecn_survey_analyze.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecn_survey_analyze.f ;\
	mv ecn_survey_analyze.o $(SCRATCH)/geocal/a9/ecn_slyze952.o ;\
	)
$(d0root)/stp/geocal.a(ecn_sebra038.o):\
  $(d0root)/stp/geocal/ecn_survey_in_zebra.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecn_survey_in_zebra.f ;\
	mv ecn_survey_in_zebra.o $(SCRATCH)/geocal/a9/ecn_sebra038.o ;\
	)
$(d0root)/stp/geocal.a(ecn_sdule842.o):\
  $(d0root)/stp/geocal/ecn_survey_module.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clin.params\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def\
  $(d0library)/links/izcsyl.link\
  $(d0library)/links/izcmdl.link\
  $(d0library)/links/izcthe.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclga.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecn_survey_module.f ;\
	mv ecn_survey_module.o $(SCRATCH)/geocal/a9/ecn_sdule842.o ;\
	)
$(d0root)/stp/geocal.a(ecn_s_rcp521.o):\
  $(d0root)/stp/geocal/ecn_survey_rcp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecn_survey_rcp.f ;\
	mv ecn_survey_rcp.o $(SCRATCH)/geocal/a9/ecn_s_rcp521.o ;\
	)
$(d0root)/stp/geocal.a(ecn_tanks879.o):\
  $(d0root)/stp/geocal/ecn_theo_geo_banks.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/links/izcsyl.link\
  $(d0library)/links/izcmdl.link\
  $(d0library)/links/izcthe.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclga.link\
  $(d0library)/links/izclgi.link\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecn_theo_geo_banks.f ;\
	mv ecn_theo_geo_banks.o $(SCRATCH)/geocal/a9/ecn_tanks879.o ;\
	)
$(d0root)/stp/geocal.a(ecpad.o):\
  $(d0root)/stp/geocal/ecpad.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clay.def\
  $(d0library)/inc/scparr.inc\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecpad.f ;\
	mv ecpad.o $(SCRATCH)/geocal/a9/ecpad.o ;\
	)
$(d0root)/stp/geocal.a(ecpad_mh.o):\
  $(d0root)/stp/geocal/ecpad_mh.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clay.def\
  $(d0library)/inc/scparr.inc\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecpad_mh.f ;\
	mv ecpad_mh.o $(SCRATCH)/geocal/a9/ecpad_mh.o ;\
	)
$(d0root)/stp/geocal.a(ecs_slyze957.o):\
  $(d0root)/stp/geocal/ecs_survey_analyze.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecs_survey_analyze.f ;\
	mv ecs_survey_analyze.o $(SCRATCH)/geocal/a9/ecs_slyze957.o ;\
	)
$(d0root)/stp/geocal.a(ecs_sebra043.o):\
  $(d0root)/stp/geocal/ecs_survey_in_zebra.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecs_survey_in_zebra.f ;\
	mv ecs_survey_in_zebra.o $(SCRATCH)/geocal/a9/ecs_sebra043.o ;\
	)
$(d0root)/stp/geocal.a(ecs_sdule847.o):\
  $(d0root)/stp/geocal/ecs_survey_module.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clin.params\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def\
  $(d0library)/links/izcsyl.link\
  $(d0library)/links/izcmdl.link\
  $(d0library)/links/izcthe.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclga.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecs_survey_module.f ;\
	mv ecs_survey_module.o $(SCRATCH)/geocal/a9/ecs_sdule847.o ;\
	)
$(d0root)/stp/geocal.a(ecs_s_rcp526.o):\
  $(d0root)/stp/geocal/ecs_survey_rcp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecs_survey_rcp.f ;\
	mv ecs_survey_rcp.o $(SCRATCH)/geocal/a9/ecs_s_rcp526.o ;\
	)
$(d0root)/stp/geocal.a(ecs_tanks884.o):\
  $(d0root)/stp/geocal/ecs_theo_geo_banks.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/links/izcsyl.link\
  $(d0library)/links/izcmdl.link\
  $(d0library)/links/izcthe.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclga.link\
  $(d0library)/links/izclgi.link\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ecs_theo_geo_banks.f ;\
	mv ecs_theo_geo_banks.o $(SCRATCH)/geocal/a9/ecs_tanks884.o ;\
	)
$(d0root)/stp/geocal.a(em_geom.o):\
  $(d0root)/stp/geocal/em_geom.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/em_geom.f ;\
	mv em_geom.o $(SCRATCH)/geocal/a9/em_geom.o ;\
	)
$(d0root)/stp/geocal.a(fcwrit.o):\
  $(d0root)/stp/geocal/fcwrit.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/links/izscal.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/fcwrit.f ;\
	mv fcwrit.o $(SCRATCH)/geocal/a9/fcwrit.o ;\
	)
$(d0root)/stp/geocal.a(flrsrc.o):\
  $(d0root)/stp/geocal/flrsrc.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/scparr.inc\
  $(d0library)/params/clga.params\
  $(d0library)/inc/clay.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/flrsrc.f ;\
	mv flrsrc.o $(SCRATCH)/geocal/a9/flrsrc.o ;\
	)
$(d0root)/stp/geocal.a(gang_cells.o):\
  $(d0root)/stp/geocal/gang_cells.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/lclyr.inc\
  $(d0library)/params/clyr.params\
  $(d0library)/links/izclyr.link\
  $(d0library)/links/izsclr.link\
  $(d0library)/params/cal_offline.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/gang_cells.f ;\
	mv gang_cells.o $(SCRATCH)/geocal/a9/gang_cells.o ;\
	)
$(d0root)/stp/geocal.a(ih_geom.o):\
  $(d0root)/stp/geocal/ih_geom.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ih_geom.f ;\
	mv ih_geom.o $(SCRATCH)/geocal/a10/ih_geom.o ;\
	)
$(d0root)/stp/geocal.a(ih_pre_pre.o):\
  $(d0root)/stp/geocal/ih_pre_pre.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ih_pre_pre.f ;\
	mv ih_pre_pre.o $(SCRATCH)/geocal/a10/ih_pre_pre.o ;\
	)
$(d0root)/stp/geocal.a(int_date.o):\
  $(d0root)/stp/geocal/int_date.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/int_date.f ;\
	mv int_date.o $(SCRATCH)/geocal/a10/int_date.o ;\
	)
$(d0root)/stp/geocal.a(inzstp.o):\
  $(d0root)/stp/geocal/inzstp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/inzstp.f ;\
	mv inzstp.o $(SCRATCH)/geocal/a10/inzstp.o ;\
	)
$(d0root)/stp/geocal.a(jdepth.o):\
  $(d0root)/stp/geocal/jdepth.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/cghead.def\
  $(d0library)/inc/region.def\
  $(d0library)/params/ceta.params\
  $(d0library)/params/cal_offline.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/jdepth.f ;\
	mv jdepth.o $(SCRATCH)/geocal/a10/jdepth.o ;\
	)
$(d0root)/stp/geocal.a(lgtype.o):\
  $(d0root)/stp/geocal/lgtype.f\
  $(d0library)/inc/cghead.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/lgtype.f ;\
	mv lgtype.o $(SCRATCH)/geocal/a10/lgtype.o ;\
	)
$(d0root)/stp/geocal.a(lnkenp.o):\
  $(d0root)/stp/geocal/lnkenp.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/calgeo.inc\
  $(d0library)/inc/clga.def\
  $(d0library)/inc/region.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/lnkenp.f ;\
	mv lnkenp.o $(SCRATCH)/geocal/a10/lnkenp.o ;\
	)
$(d0root)/stp/geocal.a(lnklay.o):\
  $(d0root)/stp/geocal/lnklay.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clga.params\
  $(d0library)/inc/clay.def\
  $(d0library)/links/izclay.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/lnklay.f ;\
	mv lnklay.o $(SCRATCH)/geocal/a10/lnklay.o ;\
	)
$(d0root)/stp/geocal.a(make_icd.o):\
  $(d0root)/stp/geocal/make_icd.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/make_icd.f ;\
	mv make_icd.o $(SCRATCH)/geocal/a10/make_icd.o ;\
	)
$(d0root)/stp/geocal.a(make_lv0.o):\
  $(d0root)/stp/geocal/make_lv0.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/make_lv0.f ;\
	mv make_lv0.o $(SCRATCH)/geocal/a10/make_lv0.o ;\
	)
$(d0root)/stp/geocal.a(make_over431.o):\
  $(d0root)/stp/geocal/make_lv0_cover.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/make_lv0_cover.f ;\
	mv make_lv0_cover.o $(SCRATCH)/geocal/a10/make_over431.o ;\
	)
$(d0root)/stp/geocal.a(make_ture670.o):\
  $(d0root)/stp/geocal/make_lv0_mixture.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/make_lv0_mixture.f ;\
	mv make_lv0_mixture.o $(SCRATCH)/geocal/a10/make_ture670.o ;\
	)
$(d0root)/stp/geocal.a(make_ther543.o):\
  $(d0root)/stp/geocal/make_lv0_mother.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/make_lv0_mother.f ;\
	mv make_lv0_mother.o $(SCRATCH)/geocal/a10/make_ther543.o ;\
	)
$(d0root)/stp/geocal.a(make_lv0_pmt.o):\
  $(d0root)/stp/geocal/make_lv0_pmt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/make_lv0_pmt.f ;\
	mv make_lv0_pmt.o $(SCRATCH)/geocal/a10/make_lv0_pmt.o ;\
	)
$(d0root)/stp/geocal.a(make_lv0_set.o):\
  $(d0root)/stp/geocal/make_lv0_set.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/make_lv0_set.f ;\
	mv make_lv0_set.o $(SCRATCH)/geocal/a10/make_lv0_set.o ;\
	)
$(d0root)/stp/geocal.a(make_skin841.o):\
  $(d0root)/stp/geocal/make_lv0_side_skin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/make_lv0_side_skin.f ;\
	mv make_lv0_side_skin.o $(SCRATCH)/geocal/a10/make_skin841.o ;\
	)
$(d0root)/stp/geocal.a(make_tile318.o):\
  $(d0root)/stp/geocal/make_lv0_tile.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/make_lv0_tile.f ;\
	mv make_lv0_tile.o $(SCRATCH)/geocal/a10/make_tile318.o ;\
	)
$(d0root)/stp/geocal.a(make_rite443.o):\
  $(d0root)/stp/geocal/make_lv0_write.f\
  $(d0library)/inc/matrix.inc\
  $(d0library)/inc/write_unit.inc\
  $(d0library)/inc/matrix_list.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/make_lv0_write.f ;\
	mv make_lv0_write.o $(SCRATCH)/geocal/a10/make_rite443.o ;\
	)
$(d0root)/stp/geocal.a(make_ite2493.o):\
  $(d0root)/stp/geocal/make_lv0_write2.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/make_lv0_write2.f ;\
	mv make_lv0_write2.o $(SCRATCH)/geocal/a10/make_ite2493.o ;\
	)
$(d0root)/stp/geocal.a(mh_geom.o):\
  $(d0root)/stp/geocal/mh_geom.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/mh_geom.f ;\
	mv mh_geom.o $(SCRATCH)/geocal/a10/mh_geom.o ;\
	)
$(d0root)/stp/geocal.a(mh_mvol.o):\
  $(d0root)/stp/geocal/mh_mvol.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/mh_mvol.f ;\
	mv mh_mvol.o $(SCRATCH)/geocal/a11/mh_mvol.o ;\
	)
$(d0root)/stp/geocal.a(mixmat.o):\
  $(d0root)/stp/geocal/mixmat.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/cmat.def\
  $(d0library)/inc/calgeo.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/mixmat.f ;\
	mv mixmat.o $(SCRATCH)/geocal/a11/mixmat.o ;\
	)
$(d0root)/stp/geocal.a(modulners515.o):\
  $(d0root)/stp/geocal/module_corners.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clga.params\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/region.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/module_corners.f ;\
	mv module_corners.o $(SCRATCH)/geocal/a11/modulners515.o ;\
	)
$(d0root)/stp/geocal.a(modulkers508.o):\
  $(d0root)/stp/geocal/module_markers.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/region.def\
  $(d0library)/inc/pi.def\
  $(d0library)/params/clga.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/module_markers.f ;\
	mv module_markers.o $(SCRATCH)/geocal/a11/modulkers508.o ;\
	)
$(d0root)/stp/geocal.a(oh_geom.o):\
  $(d0root)/stp/geocal/oh_geom.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/oh_geom.f ;\
	mv oh_geom.o $(SCRATCH)/geocal/a11/oh_geom.o ;\
	)
$(d0root)/stp/geocal.a(oh_mvol.o):\
  $(d0root)/stp/geocal/oh_mvol.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/oh_mvol.f ;\
	mv oh_mvol.o $(SCRATCH)/geocal/a11/oh_mvol.o ;\
	)
$(d0root)/stp/geocal.a(pcone.o):\
  $(d0root)/stp/geocal/pcone.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/pcone.f ;\
	mv pcone.o $(SCRATCH)/geocal/a11/pcone.o ;\
	)
$(d0root)/stp/geocal.a(prclay.o):\
  $(d0root)/stp/geocal/prclay.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clay.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/prclay.f ;\
	mv prclay.o $(SCRATCH)/geocal/a11/prclay.o ;\
	)
$(d0root)/stp/geocal.a(prclin.o):\
  $(d0root)/stp/geocal/prclin.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/params/clin.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/prclin.f ;\
	mv prclin.o $(SCRATCH)/geocal/a11/prclin.o ;\
	)
$(d0root)/stp/geocal.a(prclyr.o):\
  $(d0root)/stp/geocal/prclyr.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/params/clyr.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/prclyr.f ;\
	mv prclyr.o $(SCRATCH)/geocal/a11/prclyr.o ;\
	)
$(d0root)/stp/geocal.a(prcmdl.o):\
  $(d0root)/stp/geocal/prcmdl.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/prcmdl.f ;\
	mv prcmdl.o $(SCRATCH)/geocal/a11/prcmdl.o ;\
	)
$(d0root)/stp/geocal.a(prcthe.o):\
  $(d0root)/stp/geocal/prcthe.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/prcthe.f ;\
	mv prcthe.o $(SCRATCH)/geocal/a11/prcthe.o ;\
	)
$(d0root)/stp/geocal.a(ptrap.o):\
  $(d0root)/stp/geocal/ptrap.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/ptrap.f ;\
	mv ptrap.o $(SCRATCH)/geocal/a11/ptrap.o ;\
	)
$(d0root)/stp/geocal.a(rmin_max.o):\
  $(d0root)/stp/geocal/rmin_max.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/region.def\
  $(d0library)/params/clga.params\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/clay.def\
  $(d0library)/links/izclga.link\
  $(d0library)/inc/rminmax.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/rmin_max.f ;\
	mv rmin_max.o $(SCRATCH)/geocal/a11/rmin_max.o ;\
	)
$(d0root)/stp/geocal.a(sbclex.o):\
  $(d0root)/stp/geocal/sbclex.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/clay.def\
  $(d0library)/params/cal_offline.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/sbclex.f ;\
	mv sbclex.o $(SCRATCH)/geocal/a11/sbclex.o ;\
	)
$(d0root)/stp/geocal.a(scal.o):\
  $(d0root)/stp/geocal/scal.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/scal.f ;\
	mv scal.o $(SCRATCH)/geocal/a11/scal.o ;\
	)
$(d0root)/stp/geocal.a(scnrcp.o):\
  $(d0root)/stp/geocal/scnrcp.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/scparr.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/params/clga.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/scnrcp.f ;\
	mv scnrcp.o $(SCRATCH)/geocal/a11/scnrcp.o ;\
	)
$(d0root)/stp/geocal.a(shpsrc.o):\
  $(d0root)/stp/geocal/shpsrc.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/scparr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/shpsrc.f ;\
	mv shpsrc.o $(SCRATCH)/geocal/a11/shpsrc.o ;\
	)
$(d0root)/stp/geocal.a(skin.o):\
  $(d0root)/stp/geocal/skin.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/calgeo.inc\
  $(d0library)/inc/clga.def\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/skin.f ;\
	mv skin.o $(SCRATCH)/geocal/a11/skin.o ;\
	)
$(d0root)/stp/geocal.a(spnmod.o):\
  $(d0root)/stp/geocal/spnmod.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/calgeo.inc\
  $(d0library)/inc/clga.def\
  $(d0library)/inc/region.def\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/spnmod.f ;\
	mv spnmod.o $(SCRATCH)/geocal/a11/spnmod.o ;\
	)
$(d0root)/stp/geocal.a(survey.o):\
  $(d0root)/stp/geocal/survey.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izscal.link\
  $(d0library)/links/izclin.link\
  $(d0library)/links/izcgeh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/survey.f ;\
	mv survey.o $(SCRATCH)/geocal/a12/survey.o ;\
	)
$(d0root)/stp/geocal.a(survey_rcp.o):\
  $(d0root)/stp/geocal/survey_rcp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/survey_rcp.f ;\
	mv survey_rcp.o $(SCRATCH)/geocal/a12/survey_rcp.o ;\
	)
$(d0root)/stp/geocal.a(tower_geom.o):\
  $(d0root)/stp/geocal/tower_geom.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/tower_geom.f ;\
	mv tower_geom.o $(SCRATCH)/geocal/a12/tower_geom.o ;\
	)
$(d0root)/stp/geocal.a(towgeo.o):\
  $(d0root)/stp/geocal/towgeo.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/towgeo.f ;\
	mv towgeo.o $(SCRATCH)/geocal/a12/towgeo.o ;\
	)
$(d0root)/stp/geocal.a(towshp.o):\
  $(d0root)/stp/geocal/towshp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/towshp.f ;\
	mv towshp.o $(SCRATCH)/geocal/a12/towshp.o ;\
	)
$(d0root)/stp/geocal.a(write_iuset.o):\
  $(d0root)/stp/geocal/write_iuset.f\
  $(d0library)/inc/iuset.inc\
  $(d0library)/inc/write_unit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/write_iuset.f ;\
	mv write_iuset.o $(SCRATCH)/geocal/a12/write_iuset.o ;\
	)
$(d0root)/stp/geocal.a(writeials622.o):\
  $(d0root)/stp/geocal/write_materials.f\
  $(d0library)/inc/material_list.inc\
  $(d0library)/inc/material.inc\
  $(d0library)/inc/write_unit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/write_materials.f ;\
	mv write_materials.o $(SCRATCH)/geocal/a12/writeials622.o ;\
	)
$(d0root)/stp/geocal.a(writeices516.o):\
  $(d0root)/stp/geocal/write_matrices.f\
  $(d0library)/inc/matrix_list.inc\
  $(d0library)/inc/matrix.inc\
  $(d0library)/inc/write_unit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/write_matrices.f ;\
	mv write_matrices.o $(SCRATCH)/geocal/a12/writeices516.o ;\
	)
$(d0root)/stp/geocal.a(write_volume.o):\
  $(d0root)/stp/geocal/write_volume.f\
  $(d0library)/inc/srcp_volume.inc\
  $(d0library)/inc/write_unit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/write_volume.f ;\
	mv write_volume.o $(SCRATCH)/geocal/a12/write_volume.o ;\
	)
$(d0root)/stp/geocal.a(write_zdiv.o):\
  $(d0root)/stp/geocal/write_zdiv.f\
  $(d0library)/inc/write_unit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/write_zdiv.f ;\
	mv write_zdiv.o $(SCRATCH)/geocal/a12/write_zdiv.o ;\
	)
$(d0root)/stp/geocal.a(wrtzdv.o):\
  $(d0root)/stp/geocal/wrtzdv.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/wrtzdv.f ;\
	mv wrtzdv.o $(SCRATCH)/geocal/a12/wrtzdv.o ;\
	)
$(d0root)/stp/geocal.a(wrtzdv1.o):\
  $(d0root)/stp/geocal/wrtzdv1.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/wrtzdv1.f ;\
	mv wrtzdv1.o $(SCRATCH)/geocal/a12/wrtzdv1.o ;\
	)
geogen.a : $(d0root)/stp/geogen.a
	@ echo geogen.a is up to date
$(d0root)/stp/geogen.a:: $(SCRATCH)/geogen/nothing.nl
$(d0root)/stp/geogen.a::\
  $(d0root)/stp/geogen.a(blsgbp.o)\
  $(d0root)/stp/geogen.a(blsgen.o)\
  $(d0root)/stp/geogen.a(blsgmc.o)\
  $(d0root)/stp/geogen.a(gnwstp.o)\
  $(d0root)/stp/geogen.a(inzstp.o)
	ar $(ARFLAGS) $(d0root)/stp/geogen.a `ls $(SCRATCH)/geogen/a1/*.o 2> /dev/null`
	rmdirp $(SCRATCH)/geogen
$(SCRATCH)/geogen/nothing.nl:
	rmdirp $(SCRATCH)/geogen
	mkdirp $(SCRATCH)/geogen/a1
$(d0root)/stp/geogen.a(blsgbp.o):\
  $(d0root)/stp/geogen/blsgbp.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izsgen.link\
  $(d0library)/links/izsgbp.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geogen/blsgbp.f ;\
	mv blsgbp.o $(SCRATCH)/geogen/a1/blsgbp.o ;\
	)
$(d0root)/stp/geogen.a(blsgen.o):\
  $(d0root)/stp/geogen/blsgen.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izsgen.link\
  $(d0library)/links/izstpc.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geogen/blsgen.f ;\
	mv blsgen.o $(SCRATCH)/geogen/a1/blsgen.o ;\
	)
$(d0root)/stp/geogen.a(blsgmc.o):\
  $(d0root)/stp/geogen/blsgmc.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izsgen.link\
  $(d0library)/links/izsgmc.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geogen/blsgmc.f ;\
	mv blsgmc.o $(SCRATCH)/geogen/a1/blsgmc.o ;\
	)
$(d0root)/stp/geogen.a(gnwstp.o):\
  $(d0root)/stp/geogen/gnwstp.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/links/izsgen.link\
  $(d0library)/links/izstpc.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geogen/gnwstp.f ;\
	mv gnwstp.o $(SCRATCH)/geogen/a1/gnwstp.o ;\
	)
$(d0root)/stp/geogen.a(inzstp.o):\
  $(d0root)/stp/geocal/inzstp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/inzstp.f ;\
	mv inzstp.o $(SCRATCH)/geogen/a1/inzstp.o ;\
	)
geotbm.a : $(d0root)/stp/geotbm.a
	@ echo geotbm.a is up to date
$(d0root)/stp/geotbm.a:: $(SCRATCH)/geotbm/nothing.nl
$(d0root)/stp/geotbm.a::
	rmdirp $(SCRATCH)/geotbm
$(SCRATCH)/geotbm/nothing.nl:
	rmdirp $(SCRATCH)/geotbm
samus.a : $(d0root)/stp/samus.a
	@ echo samus.a is up to date
$(d0root)/stp/samus.a:: $(SCRATCH)/samus/nothing.nl
$(d0root)/stp/samus.a::\
  $(d0root)/stp/samus.a(inzstp.o)\
  $(d0root)/stp/samus.a(sawstp.o)
	ar $(ARFLAGS) $(d0root)/stp/samus.a `ls $(SCRATCH)/samus/a1/*.o 2> /dev/null`
	rmdirp $(SCRATCH)/samus
$(SCRATCH)/samus/nothing.nl:
	rmdirp $(SCRATCH)/samus
	mkdirp $(SCRATCH)/samus/a1
$(d0root)/stp/samus.a(inzstp.o):\
  $(d0root)/stp/geocal/inzstp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/inzstp.f ;\
	mv inzstp.o $(SCRATCH)/samus/a1/inzstp.o ;\
	)
$(d0root)/stp/samus.a(sawstp.o):\
  $(d0root)/stp/samus/sawstp.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/samus/sawstp.f ;\
	mv sawstp.o $(SCRATCH)/samus/a1/sawstp.o ;\
	)
tb90_data.a : $(d0root)/stp/tb90_data.a
	@ echo tb90_data.a is up to date
$(d0root)/stp/tb90_data.a:: $(SCRATCH)/tb90_data/nothing.nl
$(d0root)/stp/tb90_data.a::
	rmdirp $(SCRATCH)/tb90_data
$(SCRATCH)/tb90_data/nothing.nl:
	rmdirp $(SCRATCH)/tb90_data
tb90l2.a : $(d0root)/stp/tb90l2.a
	@ echo tb90l2.a is up to date
$(d0root)/stp/tb90l2.a:: $(SCRATCH)/tb90l2/nothing.nl
$(d0root)/stp/tb90l2.a::
	rmdirp $(SCRATCH)/tb90l2
$(SCRATCH)/tb90l2/nothing.nl:
	rmdirp $(SCRATCH)/tb90l2
trd.a : $(d0root)/stp/trd.a
	@ echo trd.a is up to date
$(d0root)/stp/trd.a:: $(SCRATCH)/trd/nothing.nl
$(d0root)/stp/trd.a::\
  $(d0root)/stp/trd.a(bltgai.o)\
  $(d0root)/stp/trd.a(bltpdh.o)\
  $(d0root)/stp/trd.a(fitca.o)\
  $(d0root)/stp/trd.a(fitcha.o)\
  $(d0root)/stp/trd.a(fitma.o)\
  $(d0root)/stp/trd.a(fitmae.o)\
  $(d0root)/stp/trd.a(fitmxe.o)\
  $(d0root)/stp/trd.a(fitpc.o)\
  $(d0root)/stp/trd.a(fitpe3.o)\
  $(d0root)/stp/trd.a(fitpec.o)\
  $(d0root)/stp/trd.a(fitpet.o)\
  $(d0root)/stp/trd.a(fitphy.o)\
  $(d0root)/stp/trd.a(fitple.o)\
  $(d0root)/stp/trd.a(fitpr.o)\
  $(d0root)/stp/trd.a(fitpro.o)\
  $(d0root)/stp/trd.a(fitpt3.o)\
  $(d0root)/stp/trd.a(fitptr.o)\
  $(d0root)/stp/trd.a(inzstp.o)\
  $(d0root)/stp/trd.a(pack__stp378.o)\
  $(d0root)/stp/trd.a(tcorfl.o)\
  $(d0root)/stp/trd.a(tnd1fl.o)\
  $(d0root)/stp/trd.a(tnd2fl.o)\
  $(d0root)/stp/trd.a(trwstp.o)\
  $(d0root)/stp/trd.a(tstpbk.o)\
  $(d0root)/stp/trd.a(tsttyp.o)
	ar $(ARFLAGS) $(d0root)/stp/trd.a `ls $(SCRATCH)/trd/a1/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/stp/trd.a `ls $(SCRATCH)/trd/a2/*.o 2> /dev/null`
	rmdirp $(SCRATCH)/trd
$(SCRATCH)/trd/nothing.nl:
	rmdirp $(SCRATCH)/trd
	mkdirp $(SCRATCH)/trd/a1
	mkdirp $(SCRATCH)/trd/a2
$(d0root)/stp/trd.a(bltgai.o):\
  $(d0root)/stp/trd/bltgai.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izstrd.link\
  $(d0library)/links/iztgai.link\
  $(d0library)/links/iztelh.link\
  $(d0library)/links/iztwgh.link\
  $(d0library)/links/iztgch.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/trd/bltgai.f ;\
	mv bltgai.o $(SCRATCH)/trd/a1/bltgai.o ;\
	)
$(d0root)/stp/trd.a(bltpdh.o):\
  $(d0root)/stp/trd/bltpdh.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izstrd.link\
  $(d0library)/links/iztpdh.link\
  $(d0library)/links/iztpch.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/trd/bltpdh.f ;\
	mv bltpdh.o $(SCRATCH)/trd/a1/bltpdh.o ;\
	)
$(d0root)/stp/trd.a(fitca.o):\
  $(d0root)/stp/trd/fitca.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/trd/fitca.f ;\
	mv fitca.o $(SCRATCH)/trd/a1/fitca.o ;\
	)
$(d0root)/stp/trd.a(fitcha.o):\
  $(d0root)/stp/trd/fitcha.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/trd/fitcha.f ;\
	mv fitcha.o $(SCRATCH)/trd/a1/fitcha.o ;\
	)
$(d0root)/stp/trd.a(fitma.o):\
  $(d0root)/stp/trd/fitma.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/worksp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/trd/fitma.f ;\
	mv fitma.o $(SCRATCH)/trd/a1/fitma.o ;\
	)
$(d0root)/stp/trd.a(fitmae.o):\
  $(d0root)/stp/trd/fitmae.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/trd/fitmae.f ;\
	mv fitmae.o $(SCRATCH)/trd/a1/fitmae.o ;\
	)
$(d0root)/stp/trd.a(fitmxe.o):\
  $(d0root)/stp/trd/fitmxe.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/worksp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/trd/fitmxe.f ;\
	mv fitmxe.o $(SCRATCH)/trd/a1/fitmxe.o ;\
	)
$(d0root)/stp/trd.a(fitpc.o):\
  $(d0root)/stp/trd/fitpc.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/worksp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/trd/fitpc.f ;\
	mv fitpc.o $(SCRATCH)/trd/a1/fitpc.o ;\
	)
$(d0root)/stp/trd.a(fitpe3.o):\
  $(d0root)/stp/trd/fitpe3.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/worksp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/trd/fitpe3.f ;\
	mv fitpe3.o $(SCRATCH)/trd/a1/fitpe3.o ;\
	)
$(d0root)/stp/trd.a(fitpec.o):\
  $(d0root)/stp/trd/fitpec.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/trd/fitpec.f ;\
	mv fitpec.o $(SCRATCH)/trd/a1/fitpec.o ;\
	)
$(d0root)/stp/trd.a(fitpet.o):\
  $(d0root)/stp/trd/fitpet.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/worksp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/trd/fitpet.f ;\
	mv fitpet.o $(SCRATCH)/trd/a1/fitpet.o ;\
	)
$(d0root)/stp/trd.a(fitphy.o):\
  $(d0root)/stp/trd/fitphy.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/trd/fitphy.f ;\
	mv fitphy.o $(SCRATCH)/trd/a1/fitphy.o ;\
	)
$(d0root)/stp/trd.a(fitple.o):\
  $(d0root)/stp/trd/fitple.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/worksp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/trd/fitple.f ;\
	mv fitple.o $(SCRATCH)/trd/a1/fitple.o ;\
	)
$(d0root)/stp/trd.a(fitpr.o):\
  $(d0root)/stp/trd/fitpr.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/trd/fitpr.f ;\
	mv fitpr.o $(SCRATCH)/trd/a1/fitpr.o ;\
	)
$(d0root)/stp/trd.a(fitpro.o):\
  $(d0root)/stp/trd/fitpro.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/trd/fitpro.f ;\
	mv fitpro.o $(SCRATCH)/trd/a1/fitpro.o ;\
	)
$(d0root)/stp/trd.a(fitpt3.o):\
  $(d0root)/stp/trd/fitpt3.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/worksp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/trd/fitpt3.f ;\
	mv fitpt3.o $(SCRATCH)/trd/a1/fitpt3.o ;\
	)
$(d0root)/stp/trd.a(fitptr.o):\
  $(d0root)/stp/trd/fitptr.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/worksp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/trd/fitptr.f ;\
	mv fitptr.o $(SCRATCH)/trd/a1/fitptr.o ;\
	)
$(d0root)/stp/trd.a(inzstp.o):\
  $(d0root)/stp/geocal/inzstp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/geocal/inzstp.f ;\
	mv inzstp.o $(SCRATCH)/trd/a1/inzstp.o ;\
	)
$(d0root)/stp/trd.a(pack__stp378.o):\
  $(d0root)/stp/trd/pack_real_stp.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/trd/pack_real_stp.f ;\
	mv pack_real_stp.o $(SCRATCH)/trd/a1/pack__stp378.o ;\
	)
$(d0root)/stp/trd.a(tcorfl.o):\
  $(d0root)/stp/trd/tcorfl.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/trd/tcorfl.f ;\
	mv tcorfl.o $(SCRATCH)/trd/a1/tcorfl.o ;\
	)
$(d0root)/stp/trd.a(tnd1fl.o):\
  $(d0root)/stp/trd/tnd1fl.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/trd/tnd1fl.f ;\
	mv tnd1fl.o $(SCRATCH)/trd/a2/tnd1fl.o ;\
	)
$(d0root)/stp/trd.a(tnd2fl.o):\
  $(d0root)/stp/trd/tnd2fl.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/trd/tnd2fl.f ;\
	mv tnd2fl.o $(SCRATCH)/trd/a2/tnd2fl.o ;\
	)
$(d0root)/stp/trd.a(trwstp.o):\
  $(d0root)/stp/trd/trwstp.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/trd/trwstp.f ;\
	mv trwstp.o $(SCRATCH)/trd/a2/trwstp.o ;\
	)
$(d0root)/stp/trd.a(tstpbk.o):\
  $(d0root)/stp/trd/tstpbk.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/ltrd.inc\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izstrd.link\
  $(d0library)/links/iztpdh.link\
  $(d0library)/links/iztgai.link\
  $(d0library)/links/iztgen.link\
  $(d0library)/links/iztgeo.link\
  $(d0library)/links/iztcan.link\
  $(d0library)/links/iztelh.link\
  $(d0library)/links/iztwgh.link\
  $(d0library)/links/iztrop.link\
  $(d0library)/links/iztphy.link\
  $(d0library)/links/iztcha.link\
  $(d0library)/links/iztca1.link\
  $(d0library)/links/iztca2.link\
  $(d0library)/links/iztmae.link\
  $(d0library)/links/iztmxe.link\
  $(d0library)/links/iztmaa.link\
  $(d0library)/links/iztmab.link\
  $(d0library)/links/iztmac.link\
  $(d0library)/links/iztpro.link\
  $(d0library)/links/iztpr1.link\
  $(d0library)/links/iztpr2.link\
  $(d0library)/links/iztpet.link\
  $(d0library)/links/iztpe3.link\
  $(d0library)/links/iztptr.link\
  $(d0library)/links/iztpt3.link\
  $(d0library)/links/iztple.link\
  $(d0library)/links/iztpec.link\
  $(d0library)/links/iztpca.link\
  $(d0library)/links/iztpcb.link\
  $(d0library)/links/iztpcc.link\
  $(d0library)/links/izt1a0.link\
  $(d0library)/links/izt1b0.link\
  $(d0library)/links/iztmat.link\
  $(d0library)/links/iztach.link\
  $(d0library)/links/iztdmc.link\
  $(d0library)/links/iztdda.link\
  $(d0library)/links/iztlik.link\
  $(d0library)/links/iztpio.link\
  $(d0library)/links/iztpi1.link\
  $(d0library)/links/iztpi2.link\
  $(d0library)/links/iztpde.link\
  $(d0library)/links/iztele.link\
  $(d0library)/links/iztel1.link\
  $(d0library)/links/iztel2.link\
  $(d0library)/links/iztede.link\
  $(d0library)/links/iztlta.link\
  $(d0library)/links/iztden.link\
  $(d0library)/links/iztdor.link\
  $(d0library)/links/iztdpi.link\
  $(d0library)/links/iztdfa.link\
  $(d0library)/links/iztdel.link\
  $(d0library)/links/iztdco.link\
  $(d0library)/links/iztcy1.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/trd/tstpbk.f ;\
	mv tstpbk.o $(SCRATCH)/trd/a2/tstpbk.o ;\
	)
$(d0root)/stp/trd.a(tsttyp.o):\
  $(d0root)/stp/trd/tsttyp.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/iztelh.link\
  $(d0library)/links/iztwgh.link\
  $(d0library)/links/iztrop.link\
  $(d0library)/links/iztlik.link\
  $(d0library)/links/iztrle.link\
  $(d0library)/links/iztrlc.link\
  $(d0library)/links/iztach.link\
  $(d0library)/links/iztphy.link\
  $(d0library)/links/iztcha.link\
  $(d0library)/links/iztca1.link\
  $(d0library)/links/iztca2.link\
  $(d0library)/links/iztmae.link\
  $(d0library)/links/iztmxe.link\
  $(d0library)/links/iztmaa.link\
  $(d0library)/links/iztmab.link\
  $(d0library)/links/iztmac.link\
  $(d0library)/links/iztpro.link\
  $(d0library)/links/iztpr1.link\
  $(d0library)/links/iztpr2.link\
  $(d0library)/links/iztpet.link\
  $(d0library)/links/iztpe3.link\
  $(d0library)/links/iztptr.link\
  $(d0library)/links/iztpt3.link\
  $(d0library)/links/iztple.link\
  $(d0library)/links/iztpec.link\
  $(d0library)/links/iztpca.link\
  $(d0library)/links/iztpcb.link\
  $(d0library)/links/iztpcc.link\
  $(d0library)/links/iztdmc.link\
  $(d0library)/links/iztdda.link\
  $(d0library)/links/iztpio.link\
  $(d0library)/links/iztele.link\
  $(d0library)/links/iztpi1.link\
  $(d0library)/links/iztpi2.link\
  $(d0library)/links/iztpde.link\
  $(d0library)/links/iztel1.link\
  $(d0library)/links/iztel2.link\
  $(d0library)/links/iztede.link\
  $(d0library)/links/iztlta.link\
  $(d0library)/links/iztden.link\
  $(d0library)/links/iztdor.link\
  $(d0library)/links/iztdpi.link\
  $(d0library)/links/iztdfa.link\
  $(d0library)/links/iztdel.link\
  $(d0library)/links/iztdco.link\
  $(d0library)/inc/ltrd.inc\
  $(d0library)/links/izt1a0.link\
  $(d0library)/links/izt1b0.link\
  $(d0library)/links/iztcy1.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/trd/tsttyp.f ;\
	mv tsttyp.o $(SCRATCH)/trd/a2/tsttyp.o ;\
	)
vtx.a : $(d0root)/stp/vtx.a
	@ echo vtx.a is up to date
$(d0root)/stp/vtx.a:: $(SCRATCH)/vtx/nothing.nl
$(d0root)/stp/vtx.a::\
  $(d0root)/stp/vtx.a(bkvdtm.o)\
  $(d0root)/stp/vtx.a(blvalh.o)\
  $(d0root)/stp/vtx.a(blvgeh.o)\
  $(d0root)/stp/vtx.a(blvgnh.o)\
  $(d0root)/stp/vtx.a(blvmat.o)\
  $(d0root)/stp/vtx.a(blvpdh.o)\
  $(d0root)/stp/vtx.a(blvrft.o)\
  $(d0root)/stp/vtx.a(blvtmh.o)\
  $(d0root)/stp/vtx.a(blvwal.o)\
  $(d0root)/stp/vtx.a(make_new_stp.o)\
  $(d0root)/stp/vtx.a(make_vdtm.o)\
  $(d0root)/stp/vtx.a(vals_rotate.o)\
  $(d0root)/stp/vtx.a(vals_late517.o)\
  $(d0root)/stp/vtx.a(vals_update.o)\
  $(d0root)/stp/vtx.a(vdtm_ini.o)\
  $(d0root)/stp/vtx.a(vdtm_read.o)\
  $(d0root)/stp/vtx.a(vdtmfl.o)\
  $(d0root)/stp/vtx.a(vgnl_update.o)\
  $(d0root)/stp/vtx.a(vmult.o)\
  $(d0root)/stp/vtx.a(vrotate.o)\
  $(d0root)/stp/vtx.a(vtmw_update.o)\
  $(d0root)/stp/vtx.a(vtstor.o)\
  $(d0root)/stp/vtx.a(vtwstp.o)
	ar $(ARFLAGS) $(d0root)/stp/vtx.a `ls $(SCRATCH)/vtx/a1/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/stp/vtx.a `ls $(SCRATCH)/vtx/a2/*.o 2> /dev/null`
	rmdirp $(SCRATCH)/vtx
$(SCRATCH)/vtx/nothing.nl:
	rmdirp $(SCRATCH)/vtx
	mkdirp $(SCRATCH)/vtx/a1
	mkdirp $(SCRATCH)/vtx/a2
$(d0root)/stp/vtx.a(bkvdtm.o):\
  $(d0root)/stp/vtx/bkvdtm.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izvdtm.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/vtx/bkvdtm.f ;\
	mv bkvdtm.o $(SCRATCH)/vtx/a1/bkvdtm.o ;\
	)
$(d0root)/stp/vtx.a(blvalh.o):\
  $(d0root)/stp/vtx/blvalh.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izvalh.link\
  $(d0library)/links/izvrft.link\
  $(d0library)/links/izvzst.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/vtx/blvalh.f ;\
	mv blvalh.o $(SCRATCH)/vtx/a1/blvalh.o ;\
	)
$(d0root)/stp/vtx.a(blvgeh.o):\
  $(d0root)/stp/vtx/blvgeh.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/vtx/blvgeh.f ;\
	mv blvgeh.o $(SCRATCH)/vtx/a1/blvgeh.o ;\
	)
$(d0root)/stp/vtx.a(blvgnh.o):\
  $(d0root)/stp/vtx/blvgnh.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izvrft.link\
  $(d0library)/links/izvzst.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/vtx/blvgnh.f ;\
	mv blvgnh.o $(SCRATCH)/vtx/a1/blvgnh.o ;\
	)
$(d0root)/stp/vtx.a(blvmat.o):\
  $(d0root)/stp/vtx/blvmat.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/vtx/blvmat.f ;\
	mv blvmat.o $(SCRATCH)/vtx/a1/blvmat.o ;\
	)
$(d0root)/stp/vtx.a(blvpdh.o):\
  $(d0root)/stp/vtx/blvpdh.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izvpdh.link\
  $(d0library)/links/izvrft.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/vtx/blvpdh.f ;\
	mv blvpdh.o $(SCRATCH)/vtx/a1/blvpdh.o ;\
	)
$(d0root)/stp/vtx.a(blvrft.o):\
  $(d0root)/stp/vtx/blvrft.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izvrft.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/vtx/blvrft.f ;\
	mv blvrft.o $(SCRATCH)/vtx/a1/blvrft.o ;\
	)
$(d0root)/stp/vtx.a(blvtmh.o):\
  $(d0root)/stp/vtx/blvtmh.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izvtmh.link\
  $(d0library)/links/izvgeh.link\
  $(d0library)/links/izvrft.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/vtx/blvtmh.f ;\
	mv blvtmh.o $(SCRATCH)/vtx/a1/blvtmh.o ;\
	)
$(d0root)/stp/vtx.a(blvwal.o):\
  $(d0root)/stp/vtx/blvwal.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/vtx/blvwal.f ;\
	mv blvwal.o $(SCRATCH)/vtx/a1/blvwal.o ;\
	)
$(d0root)/stp/vtx.a(make_new_stp.o):\
  $(d0root)/stp/vtx/make_new_stp.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/vtx/make_new_stp.f ;\
	mv make_new_stp.o $(SCRATCH)/vtx/a1/make_new_stp.o ;\
	)
$(d0root)/stp/vtx.a(make_vdtm.o):\
  $(d0root)/stp/vtx/make_vdtm.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/vtx/make_vdtm.f ;\
	mv make_vdtm.o $(SCRATCH)/vtx/a1/make_vdtm.o ;\
	)
$(d0root)/stp/vtx.a(vals_rotate.o):\
  $(d0root)/stp/vtx/vals_rotate.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/vtx/vals_rotate.f ;\
	mv vals_rotate.o $(SCRATCH)/vtx/a1/vals_rotate.o ;\
	)
$(d0root)/stp/vtx.a(vals_late517.o):\
  $(d0root)/stp/vtx/vals_translate.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/vtx/vals_translate.f ;\
	mv vals_translate.o $(SCRATCH)/vtx/a1/vals_late517.o ;\
	)
$(d0root)/stp/vtx.a(vals_update.o):\
  $(d0root)/stp/vtx/vals_update.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/vtx/vals_update.f ;\
	mv vals_update.o $(SCRATCH)/vtx/a1/vals_update.o ;\
	)
$(d0root)/stp/vtx.a(vdtm_ini.o):\
  $(d0root)/stp/vtx/vdtm_ini.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izvdtm.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/vtx/vdtm_ini.f ;\
	mv vdtm_ini.o $(SCRATCH)/vtx/a1/vdtm_ini.o ;\
	)
$(d0root)/stp/vtx.a(vdtm_read.o):\
  $(d0root)/stp/vtx/vdtm_read.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/vtx/vdtm_read.f ;\
	mv vdtm_read.o $(SCRATCH)/vtx/a1/vdtm_read.o ;\
	)
$(d0root)/stp/vtx.a(vdtmfl.o):\
  $(d0root)/stp/vtx/vdtmfl.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izvdtm.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/vtx/vdtmfl.f ;\
	mv vdtmfl.o $(SCRATCH)/vtx/a1/vdtmfl.o ;\
	)
$(d0root)/stp/vtx.a(vgnl_update.o):\
  $(d0root)/stp/vtx/vgnl_update.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/vtx/vgnl_update.f ;\
	mv vgnl_update.o $(SCRATCH)/vtx/a1/vgnl_update.o ;\
	)
$(d0root)/stp/vtx.a(vmult.o):\
  $(d0root)/stp/vtx/vmult.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/vtx/vmult.f ;\
	mv vmult.o $(SCRATCH)/vtx/a1/vmult.o ;\
	)
$(d0root)/stp/vtx.a(vrotate.o):\
  $(d0root)/stp/vtx/vrotate.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/vtx/vrotate.f ;\
	mv vrotate.o $(SCRATCH)/vtx/a1/vrotate.o ;\
	)
$(d0root)/stp/vtx.a(vtmw_update.o):\
  $(d0root)/stp/vtx/vtmw_update.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/vtx/vtmw_update.f ;\
	mv vtmw_update.o $(SCRATCH)/vtx/a2/vtmw_update.o ;\
	)
$(d0root)/stp/vtx.a(vtstor.o):\
  $(d0root)/stp/vtx/vtstor.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/vtx/vtstor.f ;\
	mv vtstor.o $(SCRATCH)/vtx/a2/vtstor.o ;\
	)
$(d0root)/stp/vtx.a(vtwstp.o):\
  $(d0root)/stp/vtx/vtwstp.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) stp/vtx/vtwstp.f ;\
	mv vtwstp.o $(SCRATCH)/vtx/a2/vtwstp.o ;\
	)
pre:\
  $(d0root)/stp/geocal/bkclin.f\
  $(d0root)/stp/geocal/bkcmdl.f\
  $(d0root)/stp/geocal/bkcsrv.f\
  $(d0root)/stp/geocal/bkcsyl.f\
  $(d0root)/stp/geocal/bkcthe.f\
  $(d0root)/stp/vtx/bkvdtm.f\
  $(d0root)/stp/cdc/bldalh.f\
  $(d0root)/stp/cdc/bldalh_0.f\
  $(d0root)/stp/cdc/bldcbd.f\
  $(d0root)/stp/cdc/bldcbd_d0.f\
  $(d0root)/stp/cdc/bldgeh.f\
  $(d0root)/stp/cdc/bldgnh.f\
  $(d0root)/stp/cdc/bldmat.f\
  $(d0root)/stp/cdc/bldnlx.f\
  $(d0root)/stp/cdc/bldpdh.f\
  $(d0root)/stp/cdc/bldrft.f\
  $(d0root)/stp/cdc/bldtmh.f\
  $(d0root)/stp/cdc/bldtmp.f\
  $(d0root)/stp/cdc/bldtva.f\
  $(d0root)/stp/cdc/bldwal.f\
  $(d0root)/stp/cdc/bldxxp.f\
  $(d0root)/stp/fdc/blfalh.f\
  $(d0root)/stp/fdc/blfalh_d0.f\
  $(d0root)/stp/fdc/blfalh_v2.f\
  $(d0root)/stp/fdc/blfdph.f\
  $(d0root)/stp/fdc/blfdrt.f\
  $(d0root)/stp/fdc/blfdrt_d0.f\
  $(d0root)/stp/fdc/blfdta.f\
  $(d0root)/stp/fdc/blfdta_d0.f\
  $(d0root)/stp/fdc/blfdtb.f\
  $(d0root)/stp/fdc/blfdtb_d0.f\
  $(d0root)/stp/fdc/blfgeh.f\
  $(d0root)/stp/fdc/blfgeh_d0.f\
  $(d0root)/stp/fdc/blfgeh_v2.f\
  $(d0root)/stp/fdc/blfgnh.f\
  $(d0root)/stp/fdc/blflis.f\
  $(d0root)/stp/fdc/blfmat.f\
  $(d0root)/stp/fdc/blfpdh.f\
  $(d0root)/stp/fdc/blftmh.f\
  $(d0root)/stp/fdc/blftmh_d0.f\
  $(d0root)/stp/fdc/blftmh_v2.f\
  $(d0root)/stp/fdc/blfwal.f\
  $(d0root)/stp/fdc/blfwph.f\
  $(d0root)/stp/fdc/blfwta.f\
  $(d0root)/stp/fdc/blfwtb.f\
  $(d0root)/stp/geogen/blsgbp.f\
  $(d0root)/stp/geogen/blsgen.f\
  $(d0root)/stp/geogen/blsgmc.f\
  $(d0root)/stp/trd/bltgai.f\
  $(d0root)/stp/trd/bltpdh.f\
  $(d0root)/stp/vtx/blvalh.f\
  $(d0root)/stp/vtx/blvgeh.f\
  $(d0root)/stp/vtx/blvgnh.f\
  $(d0root)/stp/vtx/blvmat.f\
  $(d0root)/stp/vtx/blvpdh.f\
  $(d0root)/stp/vtx/blvrft.f\
  $(d0root)/stp/vtx/blvtmh.f\
  $(d0root)/stp/vtx/blvwal.f\
  $(d0root)/stp/geocal/bmpipe.f\
  $(d0root)/stp/geocal/cabsrb.f\
  $(d0root)/stp/geocal/cal_alignments.f\
  $(d0root)/stp/geocal/cal_postgeo.f\
  $(d0root)/stp/geocal/cal_pregeo.f\
  $(d0root)/stp/geocal/cal_survey.f\
  $(d0root)/stp/geocal/cal_survey_ntuple.f\
  $(d0root)/stp/geocal/cawstp.f\
  $(d0root)/stp/geocal/cc_geom.f\
  $(d0root)/stp/geocal/cc_iuset.f\
  $(d0root)/stp/geocal/cc_massless_gap.f\
  $(d0root)/stp/geocal/cc_mg_div.f\
  $(d0root)/stp/geocal/cc_survey_cylinder.f\
  $(d0root)/stp/geocal/cc_survey_fit.f\
  $(d0root)/stp/geocal/cc_survey_in_zebra.f\
  $(d0root)/stp/geocal/cc_survey_module.f\
  $(d0root)/stp/geocal/cc_theo_geo_banks.f\
  $(d0root)/stp/geocal/cc_write_iuset.f\
  $(d0root)/stp/geocal/cc_write_materials.f\
  $(d0root)/stp/geocal/cc_write_matrices.f\
  $(d0root)/stp/geocal/cc_write_volume.f\
  $(d0root)/stp/geocal/ccch_backplate.f\
  $(d0root)/stp/geocal/ccch_cells.f\
  $(d0root)/stp/geocal/ccch_crack.f\
  $(d0root)/stp/geocal/ccch_endplate.f\
  $(d0root)/stp/geocal/ccch_floor.f\
  $(d0root)/stp/geocal/ccch_frontplate.f\
  $(d0root)/stp/geocal/ccch_geom.f\
  $(d0root)/stp/geocal/ccch_module.f\
  $(d0root)/stp/geocal/ccch_mother.f\
  $(d0root)/stp/geocal/ccch_mr_beampipe.f\
  $(d0root)/stp/geocal/ccch_mr_cutout.f\
  $(d0root)/stp/geocal/ccch_rotation_matrices.f\
  $(d0root)/stp/geocal/ccem_backplate.f\
  $(d0root)/stp/geocal/ccem_basic_module.f\
  $(d0root)/stp/geocal/ccem_cells.f\
  $(d0root)/stp/geocal/ccem_crack.f\
  $(d0root)/stp/geocal/ccem_endplate.f\
  $(d0root)/stp/geocal/ccem_endplate_iuset.f\
  $(d0root)/stp/geocal/ccem_floor.f\
  $(d0root)/stp/geocal/ccem_frontplate.f\
  $(d0root)/stp/geocal/ccem_geom.f\
  $(d0root)/stp/geocal/ccem_homo_geom.f\
  $(d0root)/stp/geocal/ccem_homo_iuset.f\
  $(d0root)/stp/geocal/ccem_module.f\
  $(d0root)/stp/geocal/ccem_mother.f\
  $(d0root)/stp/geocal/ccem_plate_geom.f\
  $(d0root)/stp/geocal/ccem_plate_iuset.f\
  $(d0root)/stp/geocal/ccem_rotation_matrices.f\
  $(d0root)/stp/geocal/ccem_skin_geom.f\
  $(d0root)/stp/geocal/ccem_skin_iuset.f\
  $(d0root)/stp/geocal/ccfh_backplate.f\
  $(d0root)/stp/geocal/ccfh_cells.f\
  $(d0root)/stp/geocal/ccfh_crack.f\
  $(d0root)/stp/geocal/ccfh_endplate.f\
  $(d0root)/stp/geocal/ccfh_floor.f\
  $(d0root)/stp/geocal/ccfh_frontplate.f\
  $(d0root)/stp/geocal/ccfh_geom.f\
  $(d0root)/stp/geocal/ccfh_module.f\
  $(d0root)/stp/geocal/ccfh_mother.f\
  $(d0root)/stp/geocal/ccfh_rotation_matrices.f\
  $(d0root)/stp/geocal/ccpad.f\
  $(d0root)/stp/cdc/cdwstp.f\
  $(d0root)/stp/geocal/cedp.f\
  $(d0root)/stp/geocal/celicd.f\
  $(d0root)/stp/geocal/celpos.f\
  $(d0root)/stp/geocal/celpsi.f\
  $(d0root)/stp/geocal/celsha.f\
  $(d0root)/stp/geocal/cenp.f\
  $(d0root)/stp/geocal/cenpcc.f\
  $(d0root)/stp/geocal/cenpeh.f\
  $(d0root)/stp/geocal/ceta.f\
  $(d0root)/stp/geocal/cgeh.f\
  $(d0root)/stp/geocal/chaba.f\
  $(d0root)/stp/geocal/clay.f\
  $(d0root)/stp/geocal/claycc.f\
  $(d0root)/stp/geocal/clayee.f\
  $(d0root)/stp/geocal/clayih.f\
  $(d0root)/stp/geocal/claymg_cc.f\
  $(d0root)/stp/geocal/claymg_ec.f\
  $(d0root)/stp/geocal/claymh.f\
  $(d0root)/stp/geocal/clayoh.f\
  $(d0root)/stp/geocal/clga.f\
  $(d0root)/stp/geocal/clgacc.f\
  $(d0root)/stp/geocal/clgaee.f\
  $(d0root)/stp/geocal/clgaeh.f\
  $(d0root)/stp/geocal/clgi.f\
  $(d0root)/stp/geocal/clgicc.f\
  $(d0root)/stp/geocal/clgiee.f\
  $(d0root)/stp/geocal/clgieh.f\
  $(d0root)/stp/geocal/clgsrc.f\
  $(d0root)/stp/geocal/clrrcp.f\
  $(d0root)/stp/geocal/clyr.f\
  $(d0root)/stp/geocal/cmat.f\
  $(d0root)/stp/geocal/cmsgcc.f\
  $(d0root)/stp/geocal/cmsgec.f\
  $(d0root)/stp/geocal/cold_shrink.f\
  $(d0root)/stp/geocal/cpla.f\
  $(d0root)/stp/geocal/cprzy.f\
  $(d0root)/stp/geocal/creg.f\
  $(d0root)/stp/geocal/crscel.f\
  $(d0root)/stp/geocal/crscel_eh.f\
  $(d0root)/stp/geocal/crscel_oh.f\
  $(d0root)/stp/geocal/crycen.f\
  $(d0root)/stp/geocal/crycnt.f\
  $(d0root)/stp/geocal/crydrw.f\
  $(d0root)/stp/geocal/cryenc.f\
  $(d0root)/stp/geocal/cryend.f\
  $(d0root)/stp/geocal/cryo.f\
  $(d0root)/stp/geocal/cryocc.f\
  $(d0root)/stp/geocal/cryoec.f\
  $(d0root)/stp/geocal/cryout.f\
  $(d0root)/stp/geocal/cryplt.f\
  $(d0root)/stp/geocal/cryprt.f\
  $(d0root)/stp/geocal/cryset.f\
  $(d0root)/stp/geocal/cscn.f\
  $(d0root)/stp/geocal/csfmake.f\
  $(d0root)/stp/cdc/dcstor.f\
  $(d0root)/stp/geocal/do_em_geom.f\
  $(d0root)/stp/geocal/do_ih_geom.f\
  $(d0root)/stp/geocal/do_mh_geom.f\
  $(d0root)/stp/geocal/do_misc.f\
  $(d0root)/stp/geocal/do_oh_geom.f\
  $(d0root)/stp/cdc/drddly.f\
  $(d0root)/stp/cdc/drdgns.f\
  $(d0root)/stp/geocal/ec_iusets.f\
  $(d0root)/stp/geocal/ec_mixtures.f\
  $(d0root)/stp/geocal/ec_rot_matrices.f\
  $(d0root)/stp/geocal/ec_survey_fit.f\
  $(d0root)/stp/geocal/ecem_homo_geom.f\
  $(d0root)/stp/geocal/ecem_homo_iuset.f\
  $(d0root)/stp/geocal/ecem_homo_volumes.f\
  $(d0root)/stp/geocal/ecem_misc_geom.f\
  $(d0root)/stp/geocal/ecem_plate_floor.f\
  $(d0root)/stp/geocal/ecem_plate_geom.f\
  $(d0root)/stp/geocal/ecem_plate_iuset.f\
  $(d0root)/stp/geocal/ecem_plate_volumes.f\
  $(d0root)/stp/geocal/ecem_tower_geom.f\
  $(d0root)/stp/geocal/ecih_argon_gap_geom.f\
  $(d0root)/stp/geocal/ecih_back_plate_geom.f\
  $(d0root)/stp/geocal/ecih_basic_module.f\
  $(d0root)/stp/geocal/ecih_coarse_abs_geom.f\
  $(d0root)/stp/geocal/ecih_fine_abs_geom.f\
  $(d0root)/stp/geocal/ecih_front_plate_geom.f\
  $(d0root)/stp/geocal/ecih_homo_geom.f\
  $(d0root)/stp/geocal/ecih_homo_iuset.f\
  $(d0root)/stp/geocal/ecih_homo_volumes.f\
  $(d0root)/stp/geocal/ecih_misc_geom.f\
  $(d0root)/stp/geocal/ecih_mlb_geom.f\
  $(d0root)/stp/geocal/ecih_plate_geom.f\
  $(d0root)/stp/geocal/ecih_plate_iuset.f\
  $(d0root)/stp/geocal/ecih_plate_volumes.f\
  $(d0root)/stp/geocal/ecih_support_pipe_geom.f\
  $(d0root)/stp/geocal/ecih_support_plate_geom.f\
  $(d0root)/stp/geocal/ecmh_basic_module.f\
  $(d0root)/stp/geocal/ecmh_homo_geom.f\
  $(d0root)/stp/geocal/ecmh_homo_iuset.f\
  $(d0root)/stp/geocal/ecmh_homo_volumes.f\
  $(d0root)/stp/geocal/ecmh_misc_geom.f\
  $(d0root)/stp/geocal/ecmh_plate_geom.f\
  $(d0root)/stp/geocal/ecmh_plate_iuset.f\
  $(d0root)/stp/geocal/ecmh_plate_volumes.f\
  $(d0root)/stp/geocal/ecmh_unit.f\
  $(d0root)/stp/geocal/ecn_survey_analyze.f\
  $(d0root)/stp/geocal/ecn_survey_in_zebra.f\
  $(d0root)/stp/geocal/ecn_survey_module.f\
  $(d0root)/stp/geocal/ecn_survey_rcp.f\
  $(d0root)/stp/geocal/ecn_theo_geo_banks.f\
  $(d0root)/stp/geocal/ecpad.f\
  $(d0root)/stp/geocal/ecpad_mh.f\
  $(d0root)/stp/geocal/ecs_survey_analyze.f\
  $(d0root)/stp/geocal/ecs_survey_in_zebra.f\
  $(d0root)/stp/geocal/ecs_survey_module.f\
  $(d0root)/stp/geocal/ecs_survey_rcp.f\
  $(d0root)/stp/geocal/ecs_theo_geo_banks.f\
  $(d0root)/stp/geocal/em_geom.f\
  $(d0root)/stp/geocal/fcwrit.f\
  $(d0root)/stp/fdc/fdwstp_d0.f\
  $(d0root)/stp/fdc/fdwstp_mc.f\
  $(d0root)/stp/trd/fitca.f\
  $(d0root)/stp/trd/fitcha.f\
  $(d0root)/stp/trd/fitma.f\
  $(d0root)/stp/trd/fitmae.f\
  $(d0root)/stp/trd/fitmxe.f\
  $(d0root)/stp/trd/fitpc.f\
  $(d0root)/stp/trd/fitpe3.f\
  $(d0root)/stp/trd/fitpec.f\
  $(d0root)/stp/trd/fitpet.f\
  $(d0root)/stp/trd/fitphy.f\
  $(d0root)/stp/trd/fitple.f\
  $(d0root)/stp/trd/fitpr.f\
  $(d0root)/stp/trd/fitpro.f\
  $(d0root)/stp/trd/fitpt3.f\
  $(d0root)/stp/trd/fitptr.f\
  $(d0root)/stp/geocal/flrsrc.f\
  $(d0root)/stp/fdc/fstpar.f\
  $(d0root)/stp/fdc/fstpar_v2.f\
  $(d0root)/stp/geocal/gang_cells.f\
  $(d0root)/stp/geogen/gnwstp.f\
  $(d0root)/stp/geocal/ih_geom.f\
  $(d0root)/stp/geocal/ih_pre_pre.f\
  $(d0root)/stp/geocal/int_date.f\
  $(d0root)/stp/geocal/inzstp.f\
  $(d0root)/stp/geocal/jdepth.f\
  $(d0root)/stp/geocal/lgtype.f\
  $(d0root)/stp/geocal/lnkenp.f\
  $(d0root)/stp/geocal/lnklay.f\
  $(d0root)/stp/geocal/make_icd.f\
  $(d0root)/stp/geocal/make_lv0.f\
  $(d0root)/stp/geocal/make_lv0_cover.f\
  $(d0root)/stp/geocal/make_lv0_mixture.f\
  $(d0root)/stp/geocal/make_lv0_mother.f\
  $(d0root)/stp/geocal/make_lv0_pmt.f\
  $(d0root)/stp/geocal/make_lv0_set.f\
  $(d0root)/stp/geocal/make_lv0_side_skin.f\
  $(d0root)/stp/geocal/make_lv0_tile.f\
  $(d0root)/stp/geocal/make_lv0_write.f\
  $(d0root)/stp/geocal/make_lv0_write2.f\
  $(d0root)/stp/vtx/make_new_stp.f\
  $(d0root)/stp/vtx/make_vdtm.f\
  $(d0root)/stp/geocal/mh_geom.f\
  $(d0root)/stp/geocal/mh_mvol.f\
  $(d0root)/stp/geocal/mixmat.f\
  $(d0root)/stp/geocal/module_corners.f\
  $(d0root)/stp/geocal/module_markers.f\
  $(d0root)/stp/geocal/oh_geom.f\
  $(d0root)/stp/geocal/oh_mvol.f\
  $(d0root)/stp/trd/pack_real_stp.f\
  $(d0root)/stp/geocal/pcone.f\
  $(d0root)/stp/geocal/prclay.f\
  $(d0root)/stp/geocal/prclin.f\
  $(d0root)/stp/geocal/prclyr.f\
  $(d0root)/stp/geocal/prcmdl.f\
  $(d0root)/stp/geocal/prcthe.f\
  $(d0root)/stp/geocal/ptrap.f\
  $(d0root)/stp/geocal/rmin_max.f\
  $(d0root)/stp/samus/sawstp.f\
  $(d0root)/stp/geocal/sbclex.f\
  $(d0root)/stp/geocal/scal.f\
  $(d0root)/stp/geocal/scnrcp.f\
  $(d0root)/stp/geocal/shpsrc.f\
  $(d0root)/stp/geocal/skin.f\
  $(d0root)/stp/geocal/spnmod.f\
  $(d0root)/stp/geocal/survey.f\
  $(d0root)/stp/geocal/survey_rcp.f\
  $(d0root)/stp/trd/tcorfl.f\
  $(d0root)/stp/trd/tnd1fl.f\
  $(d0root)/stp/trd/tnd2fl.f\
  $(d0root)/stp/geocal/tower_geom.f\
  $(d0root)/stp/geocal/towgeo.f\
  $(d0root)/stp/geocal/towshp.f\
  $(d0root)/stp/trd/trwstp.f\
  $(d0root)/stp/trd/tstpbk.f\
  $(d0root)/stp/trd/tsttyp.f\
  $(d0root)/stp/vtx/vals_rotate.f\
  $(d0root)/stp/vtx/vals_translate.f\
  $(d0root)/stp/vtx/vals_update.f\
  $(d0root)/stp/vtx/vdtm_ini.f\
  $(d0root)/stp/vtx/vdtm_read.f\
  $(d0root)/stp/vtx/vdtmfl.f\
  $(d0root)/stp/vtx/vgnl_update.f\
  $(d0root)/stp/vtx/vmult.f\
  $(d0root)/stp/vtx/vrotate.f\
  $(d0root)/stp/vtx/vtmw_update.f\
  $(d0root)/stp/vtx/vtstor.f\
  $(d0root)/stp/vtx/vtwstp.f\
  $(d0root)/stp/geocal/write_iuset.f\
  $(d0root)/stp/geocal/write_materials.f\
  $(d0root)/stp/geocal/write_matrices.f\
  $(d0root)/stp/geocal/write_volume.f\
  $(d0root)/stp/geocal/write_zdiv.f\
  $(d0root)/stp/geocal/wrtzdv.f\
  $(d0root)/stp/geocal/wrtzdv1.f
$(d0root)/stp/geocal/bkclin.f:\
  $(d0root)/stp/geocal/bkclin.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/params/clin.params\
  $(d0library)/links/izclin.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/bkclin.for | vmstounix > $(d0root)/stp/geocal/bkclin.f
$(d0root)/stp/geocal/bkcmdl.f:\
  $(d0root)/stp/geocal/bkcmdl.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc\
  $(d0library)/links/izcmdl.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/bkcmdl.for | vmstounix > $(d0root)/stp/geocal/bkcmdl.f
$(d0root)/stp/geocal/bkcsrv.f:\
  $(d0root)/stp/geocal/bkcsrv.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/bkcsrv.for | vmstounix > $(d0root)/stp/geocal/bkcsrv.f
$(d0root)/stp/geocal/bkcsyl.f:\
  $(d0root)/stp/geocal/bkcsyl.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc\
  $(d0library)/links/izcsyl.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/bkcsyl.for | vmstounix > $(d0root)/stp/geocal/bkcsyl.f
$(d0root)/stp/geocal/bkcthe.f:\
  $(d0root)/stp/geocal/bkcthe.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc\
  $(d0library)/links/izcthe.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/bkcthe.for | vmstounix > $(d0root)/stp/geocal/bkcthe.f
$(d0root)/stp/vtx/bkvdtm.f:\
  $(d0root)/stp/vtx/bkvdtm.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izvdtm.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/vtx/bkvdtm.for | vmstounix > $(d0root)/stp/vtx/bkvdtm.f
$(d0root)/stp/cdc/bldalh.f:\
  $(d0root)/stp/cdc/bldalh.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdalh.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/cdc/bldalh.for | vmstounix > $(d0root)/stp/cdc/bldalh.f
$(d0root)/stp/cdc/bldalh_0.f:\
  $(d0root)/stp/cdc/bldalh_0.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdalh.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/cdc/bldalh_0.for | vmstounix > $(d0root)/stp/cdc/bldalh_0.f
$(d0root)/stp/cdc/bldcbd.f:\
  $(d0root)/stp/cdc/bldcbd.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/cdc/bldcbd.for | vmstounix > $(d0root)/stp/cdc/bldcbd.f
$(d0root)/stp/cdc/bldcbd_d0.f:\
  $(d0root)/stp/cdc/bldcbd_d0.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdcbd.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/cdc/bldcbd_d0.for | vmstounix > $(d0root)/stp/cdc/bldcbd_d0.f
$(d0root)/stp/cdc/bldgeh.f:\
  $(d0root)/stp/cdc/bldgeh.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdgeh.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/cdc/bldgeh.for | vmstounix > $(d0root)/stp/cdc/bldgeh.f
$(d0root)/stp/cdc/bldgnh.f:\
  $(d0root)/stp/cdc/bldgnh.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/cdc/bldgnh.for | vmstounix > $(d0root)/stp/cdc/bldgnh.f
$(d0root)/stp/cdc/bldmat.f:\
  $(d0root)/stp/cdc/bldmat.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdmat.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/cdc/bldmat.for | vmstounix > $(d0root)/stp/cdc/bldmat.f
$(d0root)/stp/cdc/bldnlx.f:\
  $(d0root)/stp/cdc/bldnlx.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdnli.link\
  $(d0library)/links/izdnlo.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/cdc/bldnlx.for | vmstounix > $(d0root)/stp/cdc/bldnlx.f
$(d0root)/stp/cdc/bldpdh.f:\
  $(d0root)/stp/cdc/bldpdh.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/cdc/bldpdh.for | vmstounix > $(d0root)/stp/cdc/bldpdh.f
$(d0root)/stp/cdc/bldrft.f:\
  $(d0root)/stp/cdc/bldrft.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdrft.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/cdc/bldrft.for | vmstounix > $(d0root)/stp/cdc/bldrft.f
$(d0root)/stp/cdc/bldtmh.f:\
  $(d0root)/stp/cdc/bldtmh.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/cdc/bldtmh.for | vmstounix > $(d0root)/stp/cdc/bldtmh.f
$(d0root)/stp/cdc/bldtmp.f:\
  $(d0root)/stp/cdc/bldtmp.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/cdc/bldtmp.for | vmstounix > $(d0root)/stp/cdc/bldtmp.f
$(d0root)/stp/cdc/bldtva.f:\
  $(d0root)/stp/cdc/bldtva.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdtvh.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/cdc/bldtva.for | vmstounix > $(d0root)/stp/cdc/bldtva.f
$(d0root)/stp/cdc/bldwal.f:\
  $(d0root)/stp/cdc/bldwal.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdwal.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/cdc/bldwal.for | vmstounix > $(d0root)/stp/cdc/bldwal.f
$(d0root)/stp/cdc/bldxxp.f:\
  $(d0root)/stp/cdc/bldxxp.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdswp.link\
  $(d0library)/links/izddlp.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/cdc/bldxxp.for | vmstounix > $(d0root)/stp/cdc/bldxxp.f
$(d0root)/stp/fdc/blfalh.f:\
  $(d0root)/stp/fdc/blfalh.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izfalh.link\
  $(d0library)/links/izfgeh.link\
  $(d0library)/links/izfwal.link\
  $(d0library)/links/izfwta.link\
  $(d0library)/links/izfwtb.link\
  $(d0library)/links/izfwph.link\
  $(d0library)/links/izfdrt.link\
  $(d0library)/links/izfdta.link\
  $(d0library)/links/izfdtb.link\
  $(d0library)/links/izfdph.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/blfalh.for | vmstounix > $(d0root)/stp/fdc/blfalh.f
$(d0root)/stp/fdc/blfalh_d0.f:\
  $(d0root)/stp/fdc/blfalh_d0.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izfalh.link\
  $(d0library)/links/izfgeh.link\
  $(d0library)/links/izfwal.link\
  $(d0library)/links/izfwta.link\
  $(d0library)/links/izfwtb.link\
  $(d0library)/links/izfwph.link\
  $(d0library)/links/izfdrt.link\
  $(d0library)/links/izfdta.link\
  $(d0library)/links/izfdtb.link\
  $(d0library)/links/izfdph.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/blfalh_d0.for | vmstounix > $(d0root)/stp/fdc/blfalh_d0.f
$(d0root)/stp/fdc/blfalh_v2.f:\
  $(d0root)/stp/fdc/blfalh_v2.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izfalh.link\
  $(d0library)/links/izfgeh.link\
  $(d0library)/links/izfwal.link\
  $(d0library)/links/izfwta.link\
  $(d0library)/links/izfwtb.link\
  $(d0library)/links/izfwph.link\
  $(d0library)/links/izfdrt.link\
  $(d0library)/links/izfdta.link\
  $(d0library)/links/izfdtb.link\
  $(d0library)/links/izfdph.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/blfalh_v2.for | vmstounix > $(d0root)/stp/fdc/blfalh_v2.f
$(d0root)/stp/fdc/blfdph.f:\
  $(d0root)/stp/fdc/blfdph.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfdrt.link\
  $(d0library)/links/izfdph.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/blfdph.for | vmstounix > $(d0root)/stp/fdc/blfdph.f
$(d0root)/stp/fdc/blfdrt.f:\
  $(d0root)/stp/fdc/blfdrt.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfdrt.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/blfdrt.for | vmstounix > $(d0root)/stp/fdc/blfdrt.f
$(d0root)/stp/fdc/blfdrt_d0.f:\
  $(d0root)/stp/fdc/blfdrt_d0.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfdrt.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/blfdrt_d0.for | vmstounix > $(d0root)/stp/fdc/blfdrt_d0.f
$(d0root)/stp/fdc/blfdta.f:\
  $(d0root)/stp/fdc/blfdta.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfdrt.link\
  $(d0library)/links/izfdta.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/blfdta.for | vmstounix > $(d0root)/stp/fdc/blfdta.f
$(d0root)/stp/fdc/blfdta_d0.f:\
  $(d0root)/stp/fdc/blfdta_d0.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfdrt.link\
  $(d0library)/links/izfdta.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/blfdta_d0.for | vmstounix > $(d0root)/stp/fdc/blfdta_d0.f
$(d0root)/stp/fdc/blfdtb.f:\
  $(d0root)/stp/fdc/blfdtb.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfdrt.link\
  $(d0library)/links/izfdtb.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/blfdtb.for | vmstounix > $(d0root)/stp/fdc/blfdtb.f
$(d0root)/stp/fdc/blfdtb_d0.f:\
  $(d0root)/stp/fdc/blfdtb_d0.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfdrt.link\
  $(d0library)/links/izfdtb.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/blfdtb_d0.for | vmstounix > $(d0root)/stp/fdc/blfdtb_d0.f
$(d0root)/stp/fdc/blfgeh.f:\
  $(d0root)/stp/fdc/blfgeh.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izfgeh.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/blfgeh.for | vmstounix > $(d0root)/stp/fdc/blfgeh.f
$(d0root)/stp/fdc/blfgeh_d0.f:\
  $(d0root)/stp/fdc/blfgeh_d0.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izfgeh.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/blfgeh_d0.for | vmstounix > $(d0root)/stp/fdc/blfgeh_d0.f
$(d0root)/stp/fdc/blfgeh_v2.f:\
  $(d0root)/stp/fdc/blfgeh_v2.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izfgeh.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/blfgeh_v2.for | vmstounix > $(d0root)/stp/fdc/blfgeh_v2.f
$(d0root)/stp/fdc/blfgnh.f:\
  $(d0root)/stp/fdc/blfgnh.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfgnh.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/blfgnh.for | vmstounix > $(d0root)/stp/fdc/blfgnh.f
$(d0root)/stp/fdc/blflis.f:\
  $(d0root)/stp/fdc/blflis.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izflis.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/blflis.for | vmstounix > $(d0root)/stp/fdc/blflis.f
$(d0root)/stp/fdc/blfmat.f:\
  $(d0root)/stp/fdc/blfmat.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfmat.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/blfmat.for | vmstounix > $(d0root)/stp/fdc/blfmat.f
$(d0root)/stp/fdc/blfpdh.f:\
  $(d0root)/stp/fdc/blfpdh.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfpdh.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/blfpdh.for | vmstounix > $(d0root)/stp/fdc/blfpdh.f
$(d0root)/stp/fdc/blftmh.f:\
  $(d0root)/stp/fdc/blftmh.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izftmh.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/blftmh.for | vmstounix > $(d0root)/stp/fdc/blftmh.f
$(d0root)/stp/fdc/blftmh_d0.f:\
  $(d0root)/stp/fdc/blftmh_d0.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izftmh.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/blftmh_d0.for | vmstounix > $(d0root)/stp/fdc/blftmh_d0.f
$(d0root)/stp/fdc/blftmh_v2.f:\
  $(d0root)/stp/fdc/blftmh_v2.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izftmh.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/blftmh_v2.for | vmstounix > $(d0root)/stp/fdc/blftmh_v2.f
$(d0root)/stp/fdc/blfwal.f:\
  $(d0root)/stp/fdc/blfwal.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfwal.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/blfwal.for | vmstounix > $(d0root)/stp/fdc/blfwal.f
$(d0root)/stp/fdc/blfwph.f:\
  $(d0root)/stp/fdc/blfwph.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfwal.link\
  $(d0library)/links/izfwph.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/blfwph.for | vmstounix > $(d0root)/stp/fdc/blfwph.f
$(d0root)/stp/fdc/blfwta.f:\
  $(d0root)/stp/fdc/blfwta.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfwal.link\
  $(d0library)/links/izfwta.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/blfwta.for | vmstounix > $(d0root)/stp/fdc/blfwta.f
$(d0root)/stp/fdc/blfwtb.f:\
  $(d0root)/stp/fdc/blfwtb.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdcprm.inc\
  $(d0library)/links/izfwal.link\
  $(d0library)/links/izfwtb.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/blfwtb.for | vmstounix > $(d0root)/stp/fdc/blfwtb.f
$(d0root)/stp/geogen/blsgbp.f:\
  $(d0root)/stp/geogen/blsgbp.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izsgen.link\
  $(d0library)/links/izsgbp.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geogen/blsgbp.for | vmstounix > $(d0root)/stp/geogen/blsgbp.f
$(d0root)/stp/geogen/blsgen.f:\
  $(d0root)/stp/geogen/blsgen.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izsgen.link\
  $(d0library)/links/izstpc.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geogen/blsgen.for | vmstounix > $(d0root)/stp/geogen/blsgen.f
$(d0root)/stp/geogen/blsgmc.f:\
  $(d0root)/stp/geogen/blsgmc.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izsgen.link\
  $(d0library)/links/izsgmc.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geogen/blsgmc.for | vmstounix > $(d0root)/stp/geogen/blsgmc.f
$(d0root)/stp/trd/bltgai.f:\
  $(d0root)/stp/trd/bltgai.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izstrd.link\
  $(d0library)/links/iztgai.link\
  $(d0library)/links/iztelh.link\
  $(d0library)/links/iztwgh.link\
  $(d0library)/links/iztgch.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/trd/bltgai.for | vmstounix > $(d0root)/stp/trd/bltgai.f
$(d0root)/stp/trd/bltpdh.f:\
  $(d0root)/stp/trd/bltpdh.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izstrd.link\
  $(d0library)/links/iztpdh.link\
  $(d0library)/links/iztpch.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/trd/bltpdh.for | vmstounix > $(d0root)/stp/trd/bltpdh.f
$(d0root)/stp/vtx/blvalh.f:\
  $(d0root)/stp/vtx/blvalh.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izvalh.link\
  $(d0library)/links/izvrft.link\
  $(d0library)/links/izvzst.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/vtx/blvalh.for | vmstounix > $(d0root)/stp/vtx/blvalh.f
$(d0root)/stp/vtx/blvgeh.f:\
  $(d0root)/stp/vtx/blvgeh.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/vtx/blvgeh.for | vmstounix > $(d0root)/stp/vtx/blvgeh.f
$(d0root)/stp/vtx/blvgnh.f:\
  $(d0root)/stp/vtx/blvgnh.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izvrft.link\
  $(d0library)/links/izvzst.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/vtx/blvgnh.for | vmstounix > $(d0root)/stp/vtx/blvgnh.f
$(d0root)/stp/vtx/blvmat.f:\
  $(d0root)/stp/vtx/blvmat.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/vtx/blvmat.for | vmstounix > $(d0root)/stp/vtx/blvmat.f
$(d0root)/stp/vtx/blvpdh.f:\
  $(d0root)/stp/vtx/blvpdh.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izvpdh.link\
  $(d0library)/links/izvrft.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/vtx/blvpdh.for | vmstounix > $(d0root)/stp/vtx/blvpdh.f
$(d0root)/stp/vtx/blvrft.f:\
  $(d0root)/stp/vtx/blvrft.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izvrft.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/vtx/blvrft.for | vmstounix > $(d0root)/stp/vtx/blvrft.f
$(d0root)/stp/vtx/blvtmh.f:\
  $(d0root)/stp/vtx/blvtmh.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izvtmh.link\
  $(d0library)/links/izvgeh.link\
  $(d0library)/links/izvrft.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/vtx/blvtmh.for | vmstounix > $(d0root)/stp/vtx/blvtmh.f
$(d0root)/stp/vtx/blvwal.f:\
  $(d0root)/stp/vtx/blvwal.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/vtx/blvwal.for | vmstounix > $(d0root)/stp/vtx/blvwal.f
$(d0root)/stp/geocal/bmpipe.f:\
  $(d0root)/stp/geocal/bmpipe.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/bmpipe.for | vmstounix > $(d0root)/stp/geocal/bmpipe.f
$(d0root)/stp/geocal/cabsrb.f:\
  $(d0root)/stp/geocal/cabsrb.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cabsrb.for | vmstounix > $(d0root)/stp/geocal/cabsrb.f
$(d0root)/stp/geocal/cal_alignments.f:\
  $(d0root)/stp/geocal/cal_alignments.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clin.params\
  $(d0library)/inc/region.def\
  $(d0library)/links/izclin.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cal_alignments.for | vmstounix > $(d0root)/stp/geocal/cal_alignments.f
$(d0root)/stp/geocal/cal_postgeo.f:\
  $(d0root)/stp/geocal/cal_postgeo.for\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izscal.link\
  $(d0library)/links/izcgeh.link\
  $(d0library)/links/izcucl.link\
  $(d0library)/links/izcecl.link\
  $(d0library)/links/izcrst.link\
  $(d0library)/links/izcadt.link\
  $(d0library)/links/izccph.link\
  $(d0library)/links/izccpt.link\
  $(d0library)/links/izccpc.link\
  $(d0library)/links/izccua.link\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cal_postgeo.for | vmstounix > $(d0root)/stp/geocal/cal_postgeo.f
$(d0root)/stp/geocal/cal_pregeo.f:\
  $(d0root)/stp/geocal/cal_pregeo.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cal_pregeo.for | vmstounix > $(d0root)/stp/geocal/cal_pregeo.f
$(d0root)/stp/geocal/cal_survey.f:\
  $(d0root)/stp/geocal/cal_survey.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izscal.link\
  $(d0library)/links/izclin.link\
  $(d0library)/links/izcgeh.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cal_survey.for | vmstounix > $(d0root)/stp/geocal/cal_survey.f
$(d0root)/stp/geocal/cal_survey_ntuple.f:\
  $(d0root)/stp/geocal/cal_survey_ntuple.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/params/clin.params\
  $(d0library)/params/clga.params\
  $(d0library)/links/izcsyl.link\
  $(d0library)/links/izcmdl.link\
  $(d0library)/links/izcthe.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclga.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cal_survey_ntuple.for | vmstounix > $(d0root)/stp/geocal/cal_survey_ntuple.f
$(d0root)/stp/geocal/cawstp.f:\
  $(d0root)/stp/geocal/cawstp.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cawstp.for | vmstounix > $(d0root)/stp/geocal/cawstp.f
$(d0root)/stp/geocal/cc_geom.f:\
  $(d0root)/stp/geocal/cc_geom.for\
  $(d0library)/inc/material_list.inc\
  $(d0library)/inc/matrix_list.inc\
  $(d0library)/inc/write_unit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cc_geom.for | vmstounix > $(d0root)/stp/geocal/cc_geom.f
$(d0root)/stp/geocal/cc_iuset.f:\
  $(d0root)/stp/geocal/cc_iuset.for\
  $(d0library)/inc/iuset.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cc_iuset.for | vmstounix > $(d0root)/stp/geocal/cc_iuset.f
$(d0root)/stp/geocal/cc_massless_gap.f:\
  $(d0root)/stp/geocal/cc_massless_gap.for\
  $(d0library)/inc/scc_massless_gap.inc\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cc_massless_gap.for | vmstounix > $(d0root)/stp/geocal/cc_massless_gap.f
$(d0root)/stp/geocal/cc_mg_div.f:\
  $(d0root)/stp/geocal/cc_mg_div.for\
  $(d0library)/inc/write_unit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cc_mg_div.for | vmstounix > $(d0root)/stp/geocal/cc_mg_div.f
$(d0root)/stp/geocal/cc_survey_cylinder.f:\
  $(d0root)/stp/geocal/cc_survey_cylinder.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clin.params\
  $(d0library)/params/clga.params\
  $(d0library)/links/izcsyl.link\
  $(d0library)/links/izcmdl.link\
  $(d0library)/links/izcthe.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclga.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cc_survey_cylinder.for | vmstounix > $(d0root)/stp/geocal/cc_survey_cylinder.f
$(d0root)/stp/geocal/cc_survey_fit.f:\
  $(d0root)/stp/geocal/cc_survey_fit.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clin.params\
  $(d0library)/params/clga.params\
  $(d0library)/links/izcsyl.link\
  $(d0library)/links/izcmdl.link\
  $(d0library)/links/izcthe.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclga.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cc_survey_fit.for | vmstounix > $(d0root)/stp/geocal/cc_survey_fit.f
$(d0root)/stp/geocal/cc_survey_in_zebra.f:\
  $(d0root)/stp/geocal/cc_survey_in_zebra.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cc_survey_in_zebra.for | vmstounix > $(d0root)/stp/geocal/cc_survey_in_zebra.f
$(d0root)/stp/geocal/cc_survey_module.f:\
  $(d0root)/stp/geocal/cc_survey_module.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clin.params\
  $(d0library)/params/clga.params\
  $(d0library)/links/izcsyl.link\
  $(d0library)/links/izcmdl.link\
  $(d0library)/links/izcthe.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclga.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cc_survey_module.for | vmstounix > $(d0root)/stp/geocal/cc_survey_module.f
$(d0root)/stp/geocal/cc_theo_geo_banks.f:\
  $(d0root)/stp/geocal/cc_theo_geo_banks.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/links/izcsyl.link\
  $(d0library)/links/izcmdl.link\
  $(d0library)/links/izcthe.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclga.link\
  $(d0library)/links/izclgi.link\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cc_theo_geo_banks.for | vmstounix > $(d0root)/stp/geocal/cc_theo_geo_banks.f
$(d0root)/stp/geocal/cc_write_iuset.f:\
  $(d0root)/stp/geocal/cc_write_iuset.for\
  $(d0library)/inc/cc_iuset.inc\
  $(d0library)/inc/cc_unit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cc_write_iuset.for | vmstounix > $(d0root)/stp/geocal/cc_write_iuset.f
$(d0root)/stp/geocal/cc_write_materials.f:\
  $(d0root)/stp/geocal/cc_write_materials.for\
  $(d0library)/inc/cc_material_list.inc\
  $(d0library)/inc/cc_material.inc\
  $(d0library)/inc/cc_unit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cc_write_materials.for | vmstounix > $(d0root)/stp/geocal/cc_write_materials.f
$(d0root)/stp/geocal/cc_write_matrices.f:\
  $(d0root)/stp/geocal/cc_write_matrices.for\
  $(d0library)/inc/cc_matrix_list.inc\
  $(d0library)/inc/cc_matrix.inc\
  $(d0library)/inc/cc_unit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cc_write_matrices.for | vmstounix > $(d0root)/stp/geocal/cc_write_matrices.f
$(d0root)/stp/geocal/cc_write_volume.f:\
  $(d0root)/stp/geocal/cc_write_volume.for\
  $(d0library)/inc/cc_srcp_volume.inc\
  $(d0library)/inc/cc_unit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cc_write_volume.for | vmstounix > $(d0root)/stp/geocal/cc_write_volume.f
$(d0root)/stp/geocal/ccch_backplate.f:\
  $(d0root)/stp/geocal/ccch_backplate.for\
  $(d0library)/inc/sccch_backplate.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccch_backplate.for | vmstounix > $(d0root)/stp/geocal/ccch_backplate.f
$(d0root)/stp/geocal/ccch_cells.f:\
  $(d0root)/stp/geocal/ccch_cells.for\
  $(d0library)/inc/sccch_floor.inc\
  $(d0library)/inc/write_unit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccch_cells.for | vmstounix > $(d0root)/stp/geocal/ccch_cells.f
$(d0root)/stp/geocal/ccch_crack.f:\
  $(d0root)/stp/geocal/ccch_crack.for\
  $(d0library)/inc/sccch_crack.inc\
  $(d0library)/inc/sccch_floor.inc\
  $(d0library)/inc/sccch_endplate.inc\
  $(d0library)/inc/sccch_frontplate.inc\
  $(d0library)/inc/sccch_backplate.inc\
  $(d0library)/inc/sccch_module.inc\
  $(d0library)/inc/cc_material_codes.inc\
  $(d0library)/inc/material.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccch_crack.for | vmstounix > $(d0root)/stp/geocal/ccch_crack.f
$(d0root)/stp/geocal/ccch_endplate.f:\
  $(d0root)/stp/geocal/ccch_endplate.for\
  $(d0library)/inc/sccch_crack.inc\
  $(d0library)/inc/sccch_endplate.inc\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccch_endplate.for | vmstounix > $(d0root)/stp/geocal/ccch_endplate.f
$(d0root)/stp/geocal/ccch_floor.f:\
  $(d0root)/stp/geocal/ccch_floor.for\
  $(d0library)/inc/sccch_crack.inc\
  $(d0library)/inc/sccch_floor.inc\
  $(d0library)/inc/sccch_module.inc\
  $(d0library)/inc/sccch_mr_beampipe.inc\
  $(d0library)/inc/cc_material_codes.inc\
  $(d0library)/inc/material.inc\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccch_floor.for | vmstounix > $(d0root)/stp/geocal/ccch_floor.f
$(d0root)/stp/geocal/ccch_frontplate.f:\
  $(d0root)/stp/geocal/ccch_frontplate.for\
  $(d0library)/inc/sccch_frontplate.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccch_frontplate.for | vmstounix > $(d0root)/stp/geocal/ccch_frontplate.f
$(d0root)/stp/geocal/ccch_geom.f:\
  $(d0root)/stp/geocal/ccch_geom.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccch_geom.for | vmstounix > $(d0root)/stp/geocal/ccch_geom.f
$(d0root)/stp/geocal/ccch_module.f:\
  $(d0root)/stp/geocal/ccch_module.for\
  $(d0library)/inc/sccch_module.inc\
  $(d0library)/inc/sccch_mr_beampipe.inc\
  $(d0library)/inc/srcp_volume.inc\
  $(d0library)/inc/write_unit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccch_module.for | vmstounix > $(d0root)/stp/geocal/ccch_module.f
$(d0root)/stp/geocal/ccch_mother.f:\
  $(d0root)/stp/geocal/ccch_mother.for\
  $(d0library)/inc/sccch_mother.inc\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccch_mother.for | vmstounix > $(d0root)/stp/geocal/ccch_mother.f
$(d0root)/stp/geocal/ccch_mr_beampipe.f:\
  $(d0root)/stp/geocal/ccch_mr_beampipe.for\
  $(d0library)/inc/sccch_mr_beampipe.inc\
  $(d0library)/inc/srcp_volume.inc\
  $(d0library)/inc/matrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccch_mr_beampipe.for | vmstounix > $(d0root)/stp/geocal/ccch_mr_beampipe.f
$(d0root)/stp/geocal/ccch_mr_cutout.f:\
  $(d0root)/stp/geocal/ccch_mr_cutout.for\
  $(d0library)/inc/sccch_floor.inc\
  $(d0library)/inc/sccch_module.inc\
  $(d0library)/inc/sccch_mr_beampipe.inc\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccch_mr_cutout.for | vmstounix > $(d0root)/stp/geocal/ccch_mr_cutout.f
$(d0root)/stp/geocal/ccch_rotation_matrices.f:\
  $(d0root)/stp/geocal/ccch_rotation_matrices.for\
  $(d0library)/inc/matrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccch_rotation_matrices.for | vmstounix > $(d0root)/stp/geocal/ccch_rotation_matrices.f
$(d0root)/stp/geocal/ccem_backplate.f:\
  $(d0root)/stp/geocal/ccem_backplate.for\
  $(d0library)/inc/sccem_backplate.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccem_backplate.for | vmstounix > $(d0root)/stp/geocal/ccem_backplate.f
$(d0root)/stp/geocal/ccem_basic_module.f:\
  $(d0root)/stp/geocal/ccem_basic_module.for\
  $(d0library)/inc/sccem_module.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccem_basic_module.for | vmstounix > $(d0root)/stp/geocal/ccem_basic_module.f
$(d0root)/stp/geocal/ccem_cells.f:\
  $(d0root)/stp/geocal/ccem_cells.for\
  $(d0library)/inc/sccem_floor.inc\
  $(d0library)/inc/write_unit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccem_cells.for | vmstounix > $(d0root)/stp/geocal/ccem_cells.f
$(d0root)/stp/geocal/ccem_crack.f:\
  $(d0root)/stp/geocal/ccem_crack.for\
  $(d0library)/inc/sccem_crack.inc\
  $(d0library)/inc/sccem_floor.inc\
  $(d0library)/inc/sccem_endplate.inc\
  $(d0library)/inc/sccem_frontplate.inc\
  $(d0library)/inc/sccem_backplate.inc\
  $(d0library)/inc/sccem_module.inc\
  $(d0library)/inc/cc_material_codes.inc\
  $(d0library)/inc/material.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccem_crack.for | vmstounix > $(d0root)/stp/geocal/ccem_crack.f
$(d0root)/stp/geocal/ccem_endplate.f:\
  $(d0root)/stp/geocal/ccem_endplate.for\
  $(d0library)/inc/sccem_endplate.inc\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccem_endplate.for | vmstounix > $(d0root)/stp/geocal/ccem_endplate.f
$(d0root)/stp/geocal/ccem_endplate_iuset.f:\
  $(d0root)/stp/geocal/ccem_endplate_iuset.for\
  $(d0library)/inc/iuset.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccem_endplate_iuset.for | vmstounix > $(d0root)/stp/geocal/ccem_endplate_iuset.f
$(d0root)/stp/geocal/ccem_floor.f:\
  $(d0root)/stp/geocal/ccem_floor.for\
  $(d0library)/inc/sccem_crack.inc\
  $(d0library)/inc/sccem_floor.inc\
  $(d0library)/inc/sccem_module.inc\
  $(d0library)/inc/cc_material_codes.inc\
  $(d0library)/inc/material.inc\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccem_floor.for | vmstounix > $(d0root)/stp/geocal/ccem_floor.f
$(d0root)/stp/geocal/ccem_frontplate.f:\
  $(d0root)/stp/geocal/ccem_frontplate.for\
  $(d0library)/inc/sccem_frontplate.inc\
  $(d0library)/inc/sccem_module.inc\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccem_frontplate.for | vmstounix > $(d0root)/stp/geocal/ccem_frontplate.f
$(d0root)/stp/geocal/ccem_geom.f:\
  $(d0root)/stp/geocal/ccem_geom.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccem_geom.for | vmstounix > $(d0root)/stp/geocal/ccem_geom.f
$(d0root)/stp/geocal/ccem_homo_geom.f:\
  $(d0root)/stp/geocal/ccem_homo_geom.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccem_homo_geom.for | vmstounix > $(d0root)/stp/geocal/ccem_homo_geom.f
$(d0root)/stp/geocal/ccem_homo_iuset.f:\
  $(d0root)/stp/geocal/ccem_homo_iuset.for\
  $(d0library)/inc/iuset.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccem_homo_iuset.for | vmstounix > $(d0root)/stp/geocal/ccem_homo_iuset.f
$(d0root)/stp/geocal/ccem_module.f:\
  $(d0root)/stp/geocal/ccem_module.for\
  $(d0library)/inc/sccem_module.inc\
  $(d0library)/inc/srcp_volume.inc\
  $(d0library)/inc/write_unit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccem_module.for | vmstounix > $(d0root)/stp/geocal/ccem_module.f
$(d0root)/stp/geocal/ccem_mother.f:\
  $(d0root)/stp/geocal/ccem_mother.for\
  $(d0library)/inc/sccem_mother.inc\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccem_mother.for | vmstounix > $(d0root)/stp/geocal/ccem_mother.f
$(d0root)/stp/geocal/ccem_plate_geom.f:\
  $(d0root)/stp/geocal/ccem_plate_geom.for\
  $(d0library)/inc/sccem_module.inc\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccem_plate_geom.for | vmstounix > $(d0root)/stp/geocal/ccem_plate_geom.f
$(d0root)/stp/geocal/ccem_plate_iuset.f:\
  $(d0root)/stp/geocal/ccem_plate_iuset.for\
  $(d0library)/inc/iuset.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccem_plate_iuset.for | vmstounix > $(d0root)/stp/geocal/ccem_plate_iuset.f
$(d0root)/stp/geocal/ccem_rotation_matrices.f:\
  $(d0root)/stp/geocal/ccem_rotation_matrices.for\
  $(d0library)/inc/matrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccem_rotation_matrices.for | vmstounix > $(d0root)/stp/geocal/ccem_rotation_matrices.f
$(d0root)/stp/geocal/ccem_skin_geom.f:\
  $(d0root)/stp/geocal/ccem_skin_geom.for\
  $(d0library)/inc/sccem_module.inc\
  $(d0library)/inc/matrix.inc\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccem_skin_geom.for | vmstounix > $(d0root)/stp/geocal/ccem_skin_geom.f
$(d0root)/stp/geocal/ccem_skin_iuset.f:\
  $(d0root)/stp/geocal/ccem_skin_iuset.for\
  $(d0library)/inc/iuset.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccem_skin_iuset.for | vmstounix > $(d0root)/stp/geocal/ccem_skin_iuset.f
$(d0root)/stp/geocal/ccfh_backplate.f:\
  $(d0root)/stp/geocal/ccfh_backplate.for\
  $(d0library)/inc/sccfh_backplate.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccfh_backplate.for | vmstounix > $(d0root)/stp/geocal/ccfh_backplate.f
$(d0root)/stp/geocal/ccfh_cells.f:\
  $(d0root)/stp/geocal/ccfh_cells.for\
  $(d0library)/inc/sccfh_floor.inc\
  $(d0library)/inc/write_unit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccfh_cells.for | vmstounix > $(d0root)/stp/geocal/ccfh_cells.f
$(d0root)/stp/geocal/ccfh_crack.f:\
  $(d0root)/stp/geocal/ccfh_crack.for\
  $(d0library)/inc/sccfh_crack.inc\
  $(d0library)/inc/sccfh_floor.inc\
  $(d0library)/inc/sccfh_endplate.inc\
  $(d0library)/inc/sccfh_frontplate.inc\
  $(d0library)/inc/sccfh_backplate.inc\
  $(d0library)/inc/sccfh_module.inc\
  $(d0library)/inc/cc_material_codes.inc\
  $(d0library)/inc/material.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccfh_crack.for | vmstounix > $(d0root)/stp/geocal/ccfh_crack.f
$(d0root)/stp/geocal/ccfh_endplate.f:\
  $(d0root)/stp/geocal/ccfh_endplate.for\
  $(d0library)/inc/sccfh_crack.inc\
  $(d0library)/inc/sccfh_endplate.inc\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccfh_endplate.for | vmstounix > $(d0root)/stp/geocal/ccfh_endplate.f
$(d0root)/stp/geocal/ccfh_floor.f:\
  $(d0root)/stp/geocal/ccfh_floor.for\
  $(d0library)/inc/sccfh_crack.inc\
  $(d0library)/inc/sccfh_floor.inc\
  $(d0library)/inc/sccfh_module.inc\
  $(d0library)/inc/cc_material_codes.inc\
  $(d0library)/inc/material.inc\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccfh_floor.for | vmstounix > $(d0root)/stp/geocal/ccfh_floor.f
$(d0root)/stp/geocal/ccfh_frontplate.f:\
  $(d0root)/stp/geocal/ccfh_frontplate.for\
  $(d0library)/inc/sccfh_frontplate.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccfh_frontplate.for | vmstounix > $(d0root)/stp/geocal/ccfh_frontplate.f
$(d0root)/stp/geocal/ccfh_geom.f:\
  $(d0root)/stp/geocal/ccfh_geom.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccfh_geom.for | vmstounix > $(d0root)/stp/geocal/ccfh_geom.f
$(d0root)/stp/geocal/ccfh_module.f:\
  $(d0root)/stp/geocal/ccfh_module.for\
  $(d0library)/inc/sccfh_module.inc\
  $(d0library)/inc/srcp_volume.inc\
  $(d0library)/inc/write_unit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccfh_module.for | vmstounix > $(d0root)/stp/geocal/ccfh_module.f
$(d0root)/stp/geocal/ccfh_mother.f:\
  $(d0root)/stp/geocal/ccfh_mother.for\
  $(d0library)/inc/sccfh_mother.inc\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccfh_mother.for | vmstounix > $(d0root)/stp/geocal/ccfh_mother.f
$(d0root)/stp/geocal/ccfh_rotation_matrices.f:\
  $(d0root)/stp/geocal/ccfh_rotation_matrices.for\
  $(d0library)/inc/matrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccfh_rotation_matrices.for | vmstounix > $(d0root)/stp/geocal/ccfh_rotation_matrices.f
$(d0root)/stp/geocal/ccpad.f:\
  $(d0root)/stp/geocal/ccpad.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clay.def\
  $(d0library)/inc/scparr.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/sccch_floor.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ccpad.for | vmstounix > $(d0root)/stp/geocal/ccpad.f
$(d0root)/stp/cdc/cdwstp.f:\
  $(d0root)/stp/cdc/cdwstp.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/cdc/cdwstp.for | vmstounix > $(d0root)/stp/cdc/cdwstp.f
$(d0root)/stp/geocal/cedp.f:\
  $(d0root)/stp/geocal/cedp.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/calgeo.inc\
  $(d0library)/params/cedp.params\
  $(d0library)/links/izcedp.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cedp.for | vmstounix > $(d0root)/stp/geocal/cedp.f
$(d0root)/stp/geocal/celicd.f:\
  $(d0root)/stp/geocal/celicd.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/params/ceta.params\
  $(d0library)/params/clyr.params\
  $(d0library)/params/clga.params\
  $(d0library)/params/cal_offline.params\
  $(d0library)/links/izclay.link\
  $(d0library)/links/izclnk.link\
  $(d0library)/inc/clay.def
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/celicd.for | vmstounix > $(d0root)/stp/geocal/celicd.f
$(d0root)/stp/geocal/celpos.f:\
  $(d0root)/stp/geocal/celpos.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/region.def\
  $(d0library)/params/cedp.params\
  $(d0library)/params/ceta.params\
  $(d0library)/links/izceta.link\
  $(d0library)/links/izclyr.link\
  $(d0library)/params/clyr.params\
  $(d0library)/params/clga.params\
  $(d0library)/inc/clay.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def\
  $(d0library)/params/cal_offline.params\
  $(d0library)/inc/lclyr.inc\
  $(d0library)/inc/rminmax.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/celpos.for | vmstounix > $(d0root)/stp/geocal/celpos.f
$(d0root)/stp/geocal/celpsi.f:\
  $(d0root)/stp/geocal/celpsi.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clyr.params\
  $(d0library)/inc/clay.def\
  $(d0library)/inc/region.def\
  $(d0library)/params/cal_offline.params\
  $(d0library)/inc/lclyr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/celpsi.for | vmstounix > $(d0root)/stp/geocal/celpsi.f
$(d0root)/stp/geocal/celsha.f:\
  $(d0root)/stp/geocal/celsha.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/region.def\
  $(d0library)/params/cedp.params\
  $(d0library)/params/ceta.params\
  $(d0library)/params/clyr.params\
  $(d0library)/params/clga.params\
  $(d0library)/inc/clay.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def\
  $(d0library)/params/cal_offline.params\
  $(d0library)/inc/lclyr.inc\
  $(d0library)/inc/rminmax.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/celsha.for | vmstounix > $(d0root)/stp/geocal/celsha.f
$(d0root)/stp/geocal/cenp.f:\
  $(d0root)/stp/geocal/cenp.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cenp.for | vmstounix > $(d0root)/stp/geocal/cenp.f
$(d0root)/stp/geocal/cenpcc.f:\
  $(d0root)/stp/geocal/cenpcc.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/calgeo.inc\
  $(d0library)/inc/clga.def\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cenpcc.for | vmstounix > $(d0root)/stp/geocal/cenpcc.f
$(d0root)/stp/geocal/cenpeh.f:\
  $(d0root)/stp/geocal/cenpeh.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/calgeo.inc\
  $(d0library)/inc/clga.def\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cenpeh.for | vmstounix > $(d0root)/stp/geocal/cenpeh.f
$(d0root)/stp/geocal/ceta.f:\
  $(d0root)/stp/geocal/ceta.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/cedp.params\
  $(d0library)/links/izcedp.link\
  $(d0library)/params/ceta.params\
  $(d0library)/links/izceta.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ceta.for | vmstounix > $(d0root)/stp/geocal/ceta.f
$(d0root)/stp/geocal/cgeh.f:\
  $(d0root)/stp/geocal/cgeh.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/cghead.def\
  $(d0library)/links/izcgeh.link\
  $(d0library)/links/izcsha.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cgeh.for | vmstounix > $(d0root)/stp/geocal/cgeh.f
$(d0root)/stp/geocal/chaba.f:\
  $(d0root)/stp/geocal/chaba.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/chaba.for | vmstounix > $(d0root)/stp/geocal/chaba.f
$(d0root)/stp/geocal/clay.f:\
  $(d0root)/stp/geocal/clay.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/clay.for | vmstounix > $(d0root)/stp/geocal/clay.f
$(d0root)/stp/geocal/claycc.f:\
  $(d0root)/stp/geocal/claycc.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/clay.def\
  $(d0library)/params/cmat.params\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izclga.link\
  $(d0library)/links/izclay.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izcsha.link\
  $(d0library)/links/izclnk.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/claycc.for | vmstounix > $(d0root)/stp/geocal/claycc.f
$(d0root)/stp/geocal/clayee.f:\
  $(d0root)/stp/geocal/clayee.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/calgeo.inc\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/params/cmat.params\
  $(d0library)/inc/clay.def\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izcsha.link\
  $(d0library)/links/izclga.link\
  $(d0library)/links/izclay.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclnk.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/clayee.for | vmstounix > $(d0root)/stp/geocal/clayee.f
$(d0root)/stp/geocal/clayih.f:\
  $(d0root)/stp/geocal/clayih.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clga.params\
  $(d0library)/params/cmat.params\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/clay.def\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izclay.link\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izcsha.link\
  $(d0library)/links/izclga.link\
  $(d0library)/links/izclnk.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/clayih.for | vmstounix > $(d0root)/stp/geocal/clayih.f
$(d0root)/stp/geocal/claymg_cc.f:\
  $(d0root)/stp/geocal/claymg_cc.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/region.def\
  $(d0library)/inc/pi.def\
  $(d0library)/params/clga.params\
  $(d0library)/inc/clay.def\
  $(d0library)/params/cmat.params\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izcmsg.link\
  $(d0library)/links/izclay.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclnk.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/claymg_cc.for | vmstounix > $(d0root)/stp/geocal/claymg_cc.f
$(d0root)/stp/geocal/claymg_ec.f:\
  $(d0root)/stp/geocal/claymg_ec.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/region.def\
  $(d0library)/inc/pi.def\
  $(d0library)/params/clga.params\
  $(d0library)/inc/clay.def\
  $(d0library)/params/cmat.params\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izcmsg.link\
  $(d0library)/links/izclay.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclnk.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/claymg_ec.for | vmstounix > $(d0root)/stp/geocal/claymg_ec.f
$(d0root)/stp/geocal/claymh.f:\
  $(d0root)/stp/geocal/claymh.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/params/cmat.params\
  $(d0library)/inc/clay.def\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izclay.link\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izcsha.link\
  $(d0library)/links/izclga.link\
  $(d0library)/links/izclnk.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/claymh.for | vmstounix > $(d0root)/stp/geocal/claymh.f
$(d0root)/stp/geocal/clayoh.f:\
  $(d0root)/stp/geocal/clayoh.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/params/cmat.params\
  $(d0library)/inc/clay.def\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izclay.link\
  $(d0library)/links/izclga.link\
  $(d0library)/links/izcsha.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izclnk.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/clayoh.for | vmstounix > $(d0root)/stp/geocal/clayoh.f
$(d0root)/stp/geocal/clga.f:\
  $(d0root)/stp/geocal/clga.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/clga.for | vmstounix > $(d0root)/stp/geocal/clga.f
$(d0root)/stp/geocal/clgacc.f:\
  $(d0root)/stp/geocal/clgacc.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izclga.link\
  $(d0library)/links/izclgi.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izcsha.link\
  $(d0library)/links/izclnk.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/clgacc.for | vmstounix > $(d0root)/stp/geocal/clgacc.f
$(d0root)/stp/geocal/clgaee.f:\
  $(d0root)/stp/geocal/clgaee.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izclga.link\
  $(d0library)/links/izclgi.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izcsha.link\
  $(d0library)/links/izclnk.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/clgaee.for | vmstounix > $(d0root)/stp/geocal/clgaee.f
$(d0root)/stp/geocal/clgaeh.f:\
  $(d0root)/stp/geocal/clgaeh.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izclga.link\
  $(d0library)/links/izclgi.link\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izcsha.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclnk.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/clgaeh.for | vmstounix > $(d0root)/stp/geocal/clgaeh.f
$(d0root)/stp/geocal/clgi.f:\
  $(d0root)/stp/geocal/clgi.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/clgi.for | vmstounix > $(d0root)/stp/geocal/clgi.f
$(d0root)/stp/geocal/clgicc.f:\
  $(d0root)/stp/geocal/clgicc.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clga.params\
  $(d0library)/params/material_types.params\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izclgi.link\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izcsha.link\
  $(d0library)/links/izcreg.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/clgicc.for | vmstounix > $(d0root)/stp/geocal/clgicc.f
$(d0root)/stp/geocal/clgiee.f:\
  $(d0root)/stp/geocal/clgiee.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clga.params\
  $(d0library)/params/material_types.params\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izclgi.link\
  $(d0library)/links/izcsha.link\
  $(d0library)/links/izcreg.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/clgiee.for | vmstounix > $(d0root)/stp/geocal/clgiee.f
$(d0root)/stp/geocal/clgieh.f:\
  $(d0root)/stp/geocal/clgieh.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclga.link\
  $(d0library)/links/izclgi.link\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izcsha.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/clgieh.for | vmstounix > $(d0root)/stp/geocal/clgieh.f
$(d0root)/stp/geocal/clgsrc.f:\
  $(d0root)/stp/geocal/clgsrc.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/scparr.inc\
  $(d0library)/params/clga.params
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/clgsrc.for | vmstounix > $(d0root)/stp/geocal/clgsrc.f
$(d0root)/stp/geocal/clrrcp.f:\
  $(d0root)/stp/geocal/clrrcp.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/scparr.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/params/clga.params\
  $(d0library)/params/clyr.params
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/clrrcp.for | vmstounix > $(d0root)/stp/geocal/clrrcp.f
$(d0root)/stp/geocal/clyr.f:\
  $(d0root)/stp/geocal/clyr.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/region.def\
  $(d0library)/params/cedp.params\
  $(d0library)/params/ceta.params\
  $(d0library)/links/izceta.link\
  $(d0library)/links/izclay.link\
  $(d0library)/links/izclyr.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclga.link\
  $(d0library)/params/clyr.params\
  $(d0library)/params/clga.params\
  $(d0library)/inc/clay.def\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/lclyr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/clyr.for | vmstounix > $(d0root)/stp/geocal/clyr.f
$(d0root)/stp/geocal/cmat.f:\
  $(d0root)/stp/geocal/cmat.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/cmat.params\
  $(d0library)/links/izcmat.link\
  $(d0library)/inc/scparr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cmat.for | vmstounix > $(d0root)/stp/geocal/cmat.f
$(d0root)/stp/geocal/cmsgcc.f:\
  $(d0root)/stp/geocal/cmsgcc.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/region.def\
  $(d0library)/inc/pi.def\
  $(d0library)/params/clga.params\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izcmsg.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izcsha.link\
  $(d0library)/links/izclnk.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cmsgcc.for | vmstounix > $(d0root)/stp/geocal/cmsgcc.f
$(d0root)/stp/geocal/cmsgec.f:\
  $(d0root)/stp/geocal/cmsgec.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/region.def\
  $(d0library)/inc/pi.def\
  $(d0library)/params/clga.params\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izcmsg.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izcsha.link\
  $(d0library)/links/izclnk.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cmsgec.for | vmstounix > $(d0root)/stp/geocal/cmsgec.f
$(d0root)/stp/geocal/cold_shrink.f:\
  $(d0root)/stp/geocal/cold_shrink.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cold_shrink.for | vmstounix > $(d0root)/stp/geocal/cold_shrink.f
$(d0root)/stp/geocal/cpla.f:\
  $(d0root)/stp/geocal/cpla.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cpla.for | vmstounix > $(d0root)/stp/geocal/cpla.f
$(d0root)/stp/geocal/cprzy.f:\
  $(d0root)/stp/geocal/cprzy.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cprzy.for | vmstounix > $(d0root)/stp/geocal/cprzy.f
$(d0root)/stp/geocal/creg.f:\
  $(d0root)/stp/geocal/creg.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/cghead.def\
  $(d0library)/inc/region.def\
  $(d0library)/links/izcreg.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/creg.for | vmstounix > $(d0root)/stp/geocal/creg.f
$(d0root)/stp/geocal/crscel.f:\
  $(d0root)/stp/geocal/crscel.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/clay.def\
  $(d0library)/params/clyr.params\
  $(d0library)/params/cal_offline.params\
  $(d0library)/inc/lclyr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/crscel.for | vmstounix > $(d0root)/stp/geocal/crscel.f
$(d0root)/stp/geocal/crscel_eh.f:\
  $(d0root)/stp/geocal/crscel_eh.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/clay.def\
  $(d0library)/params/clyr.params\
  $(d0library)/params/cal_offline.params\
  $(d0library)/inc/lclyr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/crscel_eh.for | vmstounix > $(d0root)/stp/geocal/crscel_eh.f
$(d0root)/stp/geocal/crscel_oh.f:\
  $(d0root)/stp/geocal/crscel_oh.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/clay.def\
  $(d0library)/params/clyr.params\
  $(d0library)/params/cal_offline.params\
  $(d0library)/inc/lclyr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/crscel_oh.for | vmstounix > $(d0root)/stp/geocal/crscel_oh.f
$(d0root)/stp/geocal/crycen.f:\
  $(d0root)/stp/geocal/crycen.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/crycen.for | vmstounix > $(d0root)/stp/geocal/crycen.f
$(d0root)/stp/geocal/crycnt.f:\
  $(d0root)/stp/geocal/crycnt.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/crycnt.for | vmstounix > $(d0root)/stp/geocal/crycnt.f
$(d0root)/stp/geocal/crydrw.f:\
  $(d0root)/stp/geocal/crydrw.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/crydrw.for | vmstounix > $(d0root)/stp/geocal/crydrw.f
$(d0root)/stp/geocal/cryenc.f:\
  $(d0root)/stp/geocal/cryenc.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cryenc.for | vmstounix > $(d0root)/stp/geocal/cryenc.f
$(d0root)/stp/geocal/cryend.f:\
  $(d0root)/stp/geocal/cryend.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cryend.for | vmstounix > $(d0root)/stp/geocal/cryend.f
$(d0root)/stp/geocal/cryo.f:\
  $(d0root)/stp/geocal/cryo.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cryo.for | vmstounix > $(d0root)/stp/geocal/cryo.f
$(d0root)/stp/geocal/cryocc.f:\
  $(d0root)/stp/geocal/cryocc.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/calgeo.inc\
  $(d0library)/inc/clga.def\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cryocc.for | vmstounix > $(d0root)/stp/geocal/cryocc.f
$(d0root)/stp/geocal/cryoec.f:\
  $(d0root)/stp/geocal/cryoec.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/calgeo.inc\
  $(d0library)/inc/clga.def\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cryoec.for | vmstounix > $(d0root)/stp/geocal/cryoec.f
$(d0root)/stp/geocal/cryout.f:\
  $(d0root)/stp/geocal/cryout.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cryout.for | vmstounix > $(d0root)/stp/geocal/cryout.f
$(d0root)/stp/geocal/cryplt.f:\
  $(d0root)/stp/geocal/cryplt.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cryplt.for | vmstounix > $(d0root)/stp/geocal/cryplt.f
$(d0root)/stp/geocal/cryprt.f:\
  $(d0root)/stp/geocal/cryprt.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cryprt.for | vmstounix > $(d0root)/stp/geocal/cryprt.f
$(d0root)/stp/geocal/cryset.f:\
  $(d0root)/stp/geocal/cryset.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cryset.for | vmstounix > $(d0root)/stp/geocal/cryset.f
$(d0root)/stp/geocal/cscn.f:\
  $(d0root)/stp/geocal/cscn.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/scparr.inc\
  $(d0library)/params/clga.params\
  $(d0library)/inc/clay.def\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izcmat.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izcsha.link\
  $(d0library)/links/izcscn.link\
  $(d0library)/links/izclay.link\
  $(d0library)/links/izclnk.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/cscn.for | vmstounix > $(d0root)/stp/geocal/cscn.f
$(d0root)/stp/geocal/csfmake.f:\
  $(d0root)/stp/geocal/csfmake.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/params/cal_offline.params
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/csfmake.for | vmstounix > $(d0root)/stp/geocal/csfmake.f
$(d0root)/stp/cdc/dcstor.f:\
  $(d0root)/stp/cdc/dcstor.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/cdc/dcstor.for | vmstounix > $(d0root)/stp/cdc/dcstor.f
$(d0root)/stp/geocal/do_em_geom.f:\
  $(d0root)/stp/geocal/do_em_geom.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/do_em_geom.for | vmstounix > $(d0root)/stp/geocal/do_em_geom.f
$(d0root)/stp/geocal/do_ih_geom.f:\
  $(d0root)/stp/geocal/do_ih_geom.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/do_ih_geom.for | vmstounix > $(d0root)/stp/geocal/do_ih_geom.f
$(d0root)/stp/geocal/do_mh_geom.f:\
  $(d0root)/stp/geocal/do_mh_geom.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/do_mh_geom.for | vmstounix > $(d0root)/stp/geocal/do_mh_geom.f
$(d0root)/stp/geocal/do_misc.f:\
  $(d0root)/stp/geocal/do_misc.for\
  $(d0library)/params/srcp.def\
  $(d0library)/inc/lksrcp.inc\
  $(d0library)/inc/bfsrcp.inc\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/do_misc.for | vmstounix > $(d0root)/stp/geocal/do_misc.f
$(d0root)/stp/geocal/do_oh_geom.f:\
  $(d0root)/stp/geocal/do_oh_geom.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/do_oh_geom.for | vmstounix > $(d0root)/stp/geocal/do_oh_geom.f
$(d0root)/stp/cdc/drddly.f:\
  $(d0root)/stp/cdc/drddly.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/cdc/drddly.for | vmstounix > $(d0root)/stp/cdc/drddly.f
$(d0root)/stp/cdc/drdgns.f:\
  $(d0root)/stp/cdc/drdgns.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/cdc/drdgns.for | vmstounix > $(d0root)/stp/cdc/drdgns.f
$(d0root)/stp/geocal/ec_iusets.f:\
  $(d0root)/stp/geocal/ec_iusets.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ec_iusets.for | vmstounix > $(d0root)/stp/geocal/ec_iusets.f
$(d0root)/stp/geocal/ec_mixtures.f:\
  $(d0root)/stp/geocal/ec_mixtures.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ec_mixtures.for | vmstounix > $(d0root)/stp/geocal/ec_mixtures.f
$(d0root)/stp/geocal/ec_rot_matrices.f:\
  $(d0root)/stp/geocal/ec_rot_matrices.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ec_rot_matrices.for | vmstounix > $(d0root)/stp/geocal/ec_rot_matrices.f
$(d0root)/stp/geocal/ec_survey_fit.f:\
  $(d0root)/stp/geocal/ec_survey_fit.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clin.params\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def\
  $(d0library)/links/izcsyl.link\
  $(d0library)/links/izcmdl.link\
  $(d0library)/links/izcthe.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclga.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ec_survey_fit.for | vmstounix > $(d0root)/stp/geocal/ec_survey_fit.f
$(d0root)/stp/geocal/ecem_homo_geom.f:\
  $(d0root)/stp/geocal/ecem_homo_geom.for\
  $(d0library)/inc/srcp_volume.inc\
  $(d0library)/inc/material.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecem_homo_geom.for | vmstounix > $(d0root)/stp/geocal/ecem_homo_geom.f
$(d0root)/stp/geocal/ecem_homo_iuset.f:\
  $(d0root)/stp/geocal/ecem_homo_iuset.for\
  $(d0library)/inc/iuset.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecem_homo_iuset.for | vmstounix > $(d0root)/stp/geocal/ecem_homo_iuset.f
$(d0root)/stp/geocal/ecem_homo_volumes.f:\
  $(d0root)/stp/geocal/ecem_homo_volumes.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecem_homo_volumes.for | vmstounix > $(d0root)/stp/geocal/ecem_homo_volumes.f
$(d0root)/stp/geocal/ecem_misc_geom.f:\
  $(d0root)/stp/geocal/ecem_misc_geom.for\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecem_misc_geom.for | vmstounix > $(d0root)/stp/geocal/ecem_misc_geom.f
$(d0root)/stp/geocal/ecem_plate_floor.f:\
  $(d0root)/stp/geocal/ecem_plate_floor.for\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecem_plate_floor.for | vmstounix > $(d0root)/stp/geocal/ecem_plate_floor.f
$(d0root)/stp/geocal/ecem_plate_geom.f:\
  $(d0root)/stp/geocal/ecem_plate_geom.for\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecem_plate_geom.for | vmstounix > $(d0root)/stp/geocal/ecem_plate_geom.f
$(d0root)/stp/geocal/ecem_plate_iuset.f:\
  $(d0root)/stp/geocal/ecem_plate_iuset.for\
  $(d0library)/inc/iuset.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecem_plate_iuset.for | vmstounix > $(d0root)/stp/geocal/ecem_plate_iuset.f
$(d0root)/stp/geocal/ecem_plate_volumes.f:\
  $(d0root)/stp/geocal/ecem_plate_volumes.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecem_plate_volumes.for | vmstounix > $(d0root)/stp/geocal/ecem_plate_volumes.f
$(d0root)/stp/geocal/ecem_tower_geom.f:\
  $(d0root)/stp/geocal/ecem_tower_geom.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecem_tower_geom.for | vmstounix > $(d0root)/stp/geocal/ecem_tower_geom.f
$(d0root)/stp/geocal/ecih_argon_gap_geom.f:\
  $(d0root)/stp/geocal/ecih_argon_gap_geom.for\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecih_argon_gap_geom.for | vmstounix > $(d0root)/stp/geocal/ecih_argon_gap_geom.f
$(d0root)/stp/geocal/ecih_back_plate_geom.f:\
  $(d0root)/stp/geocal/ecih_back_plate_geom.for\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecih_back_plate_geom.for | vmstounix > $(d0root)/stp/geocal/ecih_back_plate_geom.f
$(d0root)/stp/geocal/ecih_basic_module.f:\
  $(d0root)/stp/geocal/ecih_basic_module.for\
  $(d0library)/inc/ecih_module.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecih_basic_module.for | vmstounix > $(d0root)/stp/geocal/ecih_basic_module.f
$(d0root)/stp/geocal/ecih_coarse_abs_geom.f:\
  $(d0root)/stp/geocal/ecih_coarse_abs_geom.for\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecih_coarse_abs_geom.for | vmstounix > $(d0root)/stp/geocal/ecih_coarse_abs_geom.f
$(d0root)/stp/geocal/ecih_fine_abs_geom.f:\
  $(d0root)/stp/geocal/ecih_fine_abs_geom.for\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecih_fine_abs_geom.for | vmstounix > $(d0root)/stp/geocal/ecih_fine_abs_geom.f
$(d0root)/stp/geocal/ecih_front_plate_geom.f:\
  $(d0root)/stp/geocal/ecih_front_plate_geom.for\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecih_front_plate_geom.for | vmstounix > $(d0root)/stp/geocal/ecih_front_plate_geom.f
$(d0root)/stp/geocal/ecih_homo_geom.f:\
  $(d0root)/stp/geocal/ecih_homo_geom.for\
  $(d0library)/inc/ecih_module.inc\
  $(d0library)/inc/material.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecih_homo_geom.for | vmstounix > $(d0root)/stp/geocal/ecih_homo_geom.f
$(d0root)/stp/geocal/ecih_homo_iuset.f:\
  $(d0root)/stp/geocal/ecih_homo_iuset.for\
  $(d0library)/inc/iuset.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecih_homo_iuset.for | vmstounix > $(d0root)/stp/geocal/ecih_homo_iuset.f
$(d0root)/stp/geocal/ecih_homo_volumes.f:\
  $(d0root)/stp/geocal/ecih_homo_volumes.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecih_homo_volumes.for | vmstounix > $(d0root)/stp/geocal/ecih_homo_volumes.f
$(d0root)/stp/geocal/ecih_misc_geom.f:\
  $(d0root)/stp/geocal/ecih_misc_geom.for\
  $(d0library)/inc/ecih_module.inc\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecih_misc_geom.for | vmstounix > $(d0root)/stp/geocal/ecih_misc_geom.f
$(d0root)/stp/geocal/ecih_mlb_geom.f:\
  $(d0root)/stp/geocal/ecih_mlb_geom.for\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecih_mlb_geom.for | vmstounix > $(d0root)/stp/geocal/ecih_mlb_geom.f
$(d0root)/stp/geocal/ecih_plate_geom.f:\
  $(d0root)/stp/geocal/ecih_plate_geom.for\
  $(d0library)/inc/ecih_module.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecih_plate_geom.for | vmstounix > $(d0root)/stp/geocal/ecih_plate_geom.f
$(d0root)/stp/geocal/ecih_plate_iuset.f:\
  $(d0root)/stp/geocal/ecih_plate_iuset.for\
  $(d0library)/inc/iuset.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecih_plate_iuset.for | vmstounix > $(d0root)/stp/geocal/ecih_plate_iuset.f
$(d0root)/stp/geocal/ecih_plate_volumes.f:\
  $(d0root)/stp/geocal/ecih_plate_volumes.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecih_plate_volumes.for | vmstounix > $(d0root)/stp/geocal/ecih_plate_volumes.f
$(d0root)/stp/geocal/ecih_support_pipe_geom.f:\
  $(d0root)/stp/geocal/ecih_support_pipe_geom.for\
  $(d0library)/inc/ecih_module.inc\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecih_support_pipe_geom.for | vmstounix > $(d0root)/stp/geocal/ecih_support_pipe_geom.f
$(d0root)/stp/geocal/ecih_support_plate_geom.f:\
  $(d0root)/stp/geocal/ecih_support_plate_geom.for\
  $(d0library)/inc/srcp_volume.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecih_support_plate_geom.for | vmstounix > $(d0root)/stp/geocal/ecih_support_plate_geom.f
$(d0root)/stp/geocal/ecmh_basic_module.f:\
  $(d0root)/stp/geocal/ecmh_basic_module.for\
  $(d0library)/inc/srcp_volume.inc\
  $(d0library)/inc/write_unit.inc\
  $(d0library)/inc/matrix.inc\
  $(d0library)/inc/iuset.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecmh_basic_module.for | vmstounix > $(d0root)/stp/geocal/ecmh_basic_module.f
$(d0root)/stp/geocal/ecmh_homo_geom.f:\
  $(d0root)/stp/geocal/ecmh_homo_geom.for\
  $(d0library)/inc/srcp_volume.inc\
  $(d0library)/inc/write_unit.inc\
  $(d0library)/inc/material.inc\
  $(d0library)/inc/matrix.inc\
  $(d0library)/inc/iuset.inc\
  $(d0library)/inc/ecmh_module.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecmh_homo_geom.for | vmstounix > $(d0root)/stp/geocal/ecmh_homo_geom.f
$(d0root)/stp/geocal/ecmh_homo_iuset.f:\
  $(d0root)/stp/geocal/ecmh_homo_iuset.for\
  $(d0library)/inc/srcp_volume.inc\
  $(d0library)/inc/iuset.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecmh_homo_iuset.for | vmstounix > $(d0root)/stp/geocal/ecmh_homo_iuset.f
$(d0root)/stp/geocal/ecmh_homo_volumes.f:\
  $(d0root)/stp/geocal/ecmh_homo_volumes.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecmh_homo_volumes.for | vmstounix > $(d0root)/stp/geocal/ecmh_homo_volumes.f
$(d0root)/stp/geocal/ecmh_misc_geom.f:\
  $(d0root)/stp/geocal/ecmh_misc_geom.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecmh_misc_geom.for | vmstounix > $(d0root)/stp/geocal/ecmh_misc_geom.f
$(d0root)/stp/geocal/ecmh_plate_geom.f:\
  $(d0root)/stp/geocal/ecmh_plate_geom.for\
  $(d0library)/inc/srcp_volume.inc\
  $(d0library)/inc/write_unit.inc\
  $(d0library)/inc/matrix.inc\
  $(d0library)/inc/iuset.inc\
  $(d0library)/inc/ecmh_module.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecmh_plate_geom.for | vmstounix > $(d0root)/stp/geocal/ecmh_plate_geom.f
$(d0root)/stp/geocal/ecmh_plate_iuset.f:\
  $(d0root)/stp/geocal/ecmh_plate_iuset.for\
  $(d0library)/inc/srcp_volume.inc\
  $(d0library)/inc/iuset.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecmh_plate_iuset.for | vmstounix > $(d0root)/stp/geocal/ecmh_plate_iuset.f
$(d0root)/stp/geocal/ecmh_plate_volumes.f:\
  $(d0root)/stp/geocal/ecmh_plate_volumes.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecmh_plate_volumes.for | vmstounix > $(d0root)/stp/geocal/ecmh_plate_volumes.f
$(d0root)/stp/geocal/ecmh_unit.f:\
  $(d0root)/stp/geocal/ecmh_unit.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecmh_unit.for | vmstounix > $(d0root)/stp/geocal/ecmh_unit.f
$(d0root)/stp/geocal/ecn_survey_analyze.f:\
  $(d0root)/stp/geocal/ecn_survey_analyze.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecn_survey_analyze.for | vmstounix > $(d0root)/stp/geocal/ecn_survey_analyze.f
$(d0root)/stp/geocal/ecn_survey_in_zebra.f:\
  $(d0root)/stp/geocal/ecn_survey_in_zebra.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecn_survey_in_zebra.for | vmstounix > $(d0root)/stp/geocal/ecn_survey_in_zebra.f
$(d0root)/stp/geocal/ecn_survey_module.f:\
  $(d0root)/stp/geocal/ecn_survey_module.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clin.params\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def\
  $(d0library)/links/izcsyl.link\
  $(d0library)/links/izcmdl.link\
  $(d0library)/links/izcthe.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclga.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecn_survey_module.for | vmstounix > $(d0root)/stp/geocal/ecn_survey_module.f
$(d0root)/stp/geocal/ecn_survey_rcp.f:\
  $(d0root)/stp/geocal/ecn_survey_rcp.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecn_survey_rcp.for | vmstounix > $(d0root)/stp/geocal/ecn_survey_rcp.f
$(d0root)/stp/geocal/ecn_theo_geo_banks.f:\
  $(d0root)/stp/geocal/ecn_theo_geo_banks.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/links/izcsyl.link\
  $(d0library)/links/izcmdl.link\
  $(d0library)/links/izcthe.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclga.link\
  $(d0library)/links/izclgi.link\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecn_theo_geo_banks.for | vmstounix > $(d0root)/stp/geocal/ecn_theo_geo_banks.f
$(d0root)/stp/geocal/ecpad.f:\
  $(d0root)/stp/geocal/ecpad.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clay.def\
  $(d0library)/inc/scparr.inc\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecpad.for | vmstounix > $(d0root)/stp/geocal/ecpad.f
$(d0root)/stp/geocal/ecpad_mh.f:\
  $(d0root)/stp/geocal/ecpad_mh.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clay.def\
  $(d0library)/inc/scparr.inc\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecpad_mh.for | vmstounix > $(d0root)/stp/geocal/ecpad_mh.f
$(d0root)/stp/geocal/ecs_survey_analyze.f:\
  $(d0root)/stp/geocal/ecs_survey_analyze.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecs_survey_analyze.for | vmstounix > $(d0root)/stp/geocal/ecs_survey_analyze.f
$(d0root)/stp/geocal/ecs_survey_in_zebra.f:\
  $(d0root)/stp/geocal/ecs_survey_in_zebra.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecs_survey_in_zebra.for | vmstounix > $(d0root)/stp/geocal/ecs_survey_in_zebra.f
$(d0root)/stp/geocal/ecs_survey_module.f:\
  $(d0root)/stp/geocal/ecs_survey_module.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clin.params\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def\
  $(d0library)/links/izcsyl.link\
  $(d0library)/links/izcmdl.link\
  $(d0library)/links/izcthe.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclga.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecs_survey_module.for | vmstounix > $(d0root)/stp/geocal/ecs_survey_module.f
$(d0root)/stp/geocal/ecs_survey_rcp.f:\
  $(d0root)/stp/geocal/ecs_survey_rcp.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecs_survey_rcp.for | vmstounix > $(d0root)/stp/geocal/ecs_survey_rcp.f
$(d0root)/stp/geocal/ecs_theo_geo_banks.f:\
  $(d0root)/stp/geocal/ecs_theo_geo_banks.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/links/izcsyl.link\
  $(d0library)/links/izcmdl.link\
  $(d0library)/links/izcthe.link\
  $(d0library)/links/izcreg.link\
  $(d0library)/links/izclga.link\
  $(d0library)/links/izclgi.link\
  $(d0library)/params/clga.params\
  $(d0library)/inc/region.def
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ecs_theo_geo_banks.for | vmstounix > $(d0root)/stp/geocal/ecs_theo_geo_banks.f
$(d0root)/stp/geocal/em_geom.f:\
  $(d0root)/stp/geocal/em_geom.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/em_geom.for | vmstounix > $(d0root)/stp/geocal/em_geom.f
$(d0root)/stp/geocal/fcwrit.f:\
  $(d0root)/stp/geocal/fcwrit.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/links/izscal.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/fcwrit.for | vmstounix > $(d0root)/stp/geocal/fcwrit.f
$(d0root)/stp/fdc/fdwstp_d0.f:\
  $(d0root)/stp/fdc/fdwstp_d0.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/fdwstp_d0.for | vmstounix > $(d0root)/stp/fdc/fdwstp_d0.f
$(d0root)/stp/fdc/fdwstp_mc.f:\
  $(d0root)/stp/fdc/fdwstp_mc.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/fdwstp_mc.for | vmstounix > $(d0root)/stp/fdc/fdwstp_mc.f
$(d0root)/stp/trd/fitca.f:\
  $(d0root)/stp/trd/fitca.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/trd/fitca.for | vmstounix > $(d0root)/stp/trd/fitca.f
$(d0root)/stp/trd/fitcha.f:\
  $(d0root)/stp/trd/fitcha.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/trd/fitcha.for | vmstounix > $(d0root)/stp/trd/fitcha.f
$(d0root)/stp/trd/fitma.f:\
  $(d0root)/stp/trd/fitma.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/worksp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/trd/fitma.for | vmstounix > $(d0root)/stp/trd/fitma.f
$(d0root)/stp/trd/fitmae.f:\
  $(d0root)/stp/trd/fitmae.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/trd/fitmae.for | vmstounix > $(d0root)/stp/trd/fitmae.f
$(d0root)/stp/trd/fitmxe.f:\
  $(d0root)/stp/trd/fitmxe.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/worksp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/trd/fitmxe.for | vmstounix > $(d0root)/stp/trd/fitmxe.f
$(d0root)/stp/trd/fitpc.f:\
  $(d0root)/stp/trd/fitpc.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/worksp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/trd/fitpc.for | vmstounix > $(d0root)/stp/trd/fitpc.f
$(d0root)/stp/trd/fitpe3.f:\
  $(d0root)/stp/trd/fitpe3.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/worksp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/trd/fitpe3.for | vmstounix > $(d0root)/stp/trd/fitpe3.f
$(d0root)/stp/trd/fitpec.f:\
  $(d0root)/stp/trd/fitpec.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/trd/fitpec.for | vmstounix > $(d0root)/stp/trd/fitpec.f
$(d0root)/stp/trd/fitpet.f:\
  $(d0root)/stp/trd/fitpet.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/worksp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/trd/fitpet.for | vmstounix > $(d0root)/stp/trd/fitpet.f
$(d0root)/stp/trd/fitphy.f:\
  $(d0root)/stp/trd/fitphy.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/trd/fitphy.for | vmstounix > $(d0root)/stp/trd/fitphy.f
$(d0root)/stp/trd/fitple.f:\
  $(d0root)/stp/trd/fitple.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/worksp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/trd/fitple.for | vmstounix > $(d0root)/stp/trd/fitple.f
$(d0root)/stp/trd/fitpr.f:\
  $(d0root)/stp/trd/fitpr.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/trd/fitpr.for | vmstounix > $(d0root)/stp/trd/fitpr.f
$(d0root)/stp/trd/fitpro.f:\
  $(d0root)/stp/trd/fitpro.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/trd/fitpro.for | vmstounix > $(d0root)/stp/trd/fitpro.f
$(d0root)/stp/trd/fitpt3.f:\
  $(d0root)/stp/trd/fitpt3.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/worksp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/trd/fitpt3.for | vmstounix > $(d0root)/stp/trd/fitpt3.f
$(d0root)/stp/trd/fitptr.f:\
  $(d0root)/stp/trd/fitptr.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/worksp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/trd/fitptr.for | vmstounix > $(d0root)/stp/trd/fitptr.f
$(d0root)/stp/geocal/flrsrc.f:\
  $(d0root)/stp/geocal/flrsrc.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/scparr.inc\
  $(d0library)/params/clga.params\
  $(d0library)/inc/clay.def
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/flrsrc.for | vmstounix > $(d0root)/stp/geocal/flrsrc.f
$(d0root)/stp/fdc/fstpar.f:\
  $(d0root)/stp/fdc/fstpar.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdprm2.inc\
  $(d0library)/links/izfalh.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/fstpar.for | vmstounix > $(d0root)/stp/fdc/fstpar.f
$(d0root)/stp/fdc/fstpar_v2.f:\
  $(d0root)/stp/fdc/fstpar_v2.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdprm2.inc\
  $(d0library)/links/izfalh.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/fdc/fstpar_v2.for | vmstounix > $(d0root)/stp/fdc/fstpar_v2.f
$(d0root)/stp/geocal/gang_cells.f:\
  $(d0root)/stp/geocal/gang_cells.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/lclyr.inc\
  $(d0library)/params/clyr.params\
  $(d0library)/links/izclyr.link\
  $(d0library)/links/izsclr.link\
  $(d0library)/params/cal_offline.params
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/gang_cells.for | vmstounix > $(d0root)/stp/geocal/gang_cells.f
$(d0root)/stp/geogen/gnwstp.f:\
  $(d0root)/stp/geogen/gnwstp.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/links/izsgen.link\
  $(d0library)/links/izstpc.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geogen/gnwstp.for | vmstounix > $(d0root)/stp/geogen/gnwstp.f
$(d0root)/stp/geocal/ih_geom.f:\
  $(d0root)/stp/geocal/ih_geom.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ih_geom.for | vmstounix > $(d0root)/stp/geocal/ih_geom.f
$(d0root)/stp/geocal/ih_pre_pre.f:\
  $(d0root)/stp/geocal/ih_pre_pre.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ih_pre_pre.for | vmstounix > $(d0root)/stp/geocal/ih_pre_pre.f
$(d0root)/stp/geocal/int_date.f:\
  $(d0root)/stp/geocal/int_date.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/int_date.for | vmstounix > $(d0root)/stp/geocal/int_date.f
$(d0root)/stp/geocal/inzstp.f:\
  $(d0root)/stp/geocal/inzstp.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/inzstp.for | vmstounix > $(d0root)/stp/geocal/inzstp.f
$(d0root)/stp/geocal/jdepth.f:\
  $(d0root)/stp/geocal/jdepth.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/cghead.def\
  $(d0library)/inc/region.def\
  $(d0library)/params/ceta.params\
  $(d0library)/params/cal_offline.params
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/jdepth.for | vmstounix > $(d0root)/stp/geocal/jdepth.f
$(d0root)/stp/geocal/lgtype.f:\
  $(d0root)/stp/geocal/lgtype.for\
  $(d0library)/inc/cghead.def
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/lgtype.for | vmstounix > $(d0root)/stp/geocal/lgtype.f
$(d0root)/stp/geocal/lnkenp.f:\
  $(d0root)/stp/geocal/lnkenp.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/calgeo.inc\
  $(d0library)/inc/clga.def\
  $(d0library)/inc/region.def
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/lnkenp.for | vmstounix > $(d0root)/stp/geocal/lnkenp.f
$(d0root)/stp/geocal/lnklay.f:\
  $(d0root)/stp/geocal/lnklay.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clga.params\
  $(d0library)/inc/clay.def\
  $(d0library)/links/izclay.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/lnklay.for | vmstounix > $(d0root)/stp/geocal/lnklay.f
$(d0root)/stp/geocal/make_icd.f:\
  $(d0root)/stp/geocal/make_icd.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/make_icd.for | vmstounix > $(d0root)/stp/geocal/make_icd.f
$(d0root)/stp/geocal/make_lv0.f:\
  $(d0root)/stp/geocal/make_lv0.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/make_lv0.for | vmstounix > $(d0root)/stp/geocal/make_lv0.f
$(d0root)/stp/geocal/make_lv0_cover.f:\
  $(d0root)/stp/geocal/make_lv0_cover.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/make_lv0_cover.for | vmstounix > $(d0root)/stp/geocal/make_lv0_cover.f
$(d0root)/stp/geocal/make_lv0_mixture.f:\
  $(d0root)/stp/geocal/make_lv0_mixture.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/make_lv0_mixture.for | vmstounix > $(d0root)/stp/geocal/make_lv0_mixture.f
$(d0root)/stp/geocal/make_lv0_mother.f:\
  $(d0root)/stp/geocal/make_lv0_mother.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/make_lv0_mother.for | vmstounix > $(d0root)/stp/geocal/make_lv0_mother.f
$(d0root)/stp/geocal/make_lv0_pmt.f:\
  $(d0root)/stp/geocal/make_lv0_pmt.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/make_lv0_pmt.for | vmstounix > $(d0root)/stp/geocal/make_lv0_pmt.f
$(d0root)/stp/geocal/make_lv0_set.f:\
  $(d0root)/stp/geocal/make_lv0_set.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/make_lv0_set.for | vmstounix > $(d0root)/stp/geocal/make_lv0_set.f
$(d0root)/stp/geocal/make_lv0_side_skin.f:\
  $(d0root)/stp/geocal/make_lv0_side_skin.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/make_lv0_side_skin.for | vmstounix > $(d0root)/stp/geocal/make_lv0_side_skin.f
$(d0root)/stp/geocal/make_lv0_tile.f:\
  $(d0root)/stp/geocal/make_lv0_tile.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/make_lv0_tile.for | vmstounix > $(d0root)/stp/geocal/make_lv0_tile.f
$(d0root)/stp/geocal/make_lv0_write.f:\
  $(d0root)/stp/geocal/make_lv0_write.for\
  $(d0library)/inc/matrix.inc\
  $(d0library)/inc/write_unit.inc\
  $(d0library)/inc/matrix_list.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/make_lv0_write.for | vmstounix > $(d0root)/stp/geocal/make_lv0_write.f
$(d0root)/stp/geocal/make_lv0_write2.f:\
  $(d0root)/stp/geocal/make_lv0_write2.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/make_lv0_write2.for | vmstounix > $(d0root)/stp/geocal/make_lv0_write2.f
$(d0root)/stp/vtx/make_new_stp.f:\
  $(d0root)/stp/vtx/make_new_stp.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/vtx/make_new_stp.for | vmstounix > $(d0root)/stp/vtx/make_new_stp.f
$(d0root)/stp/vtx/make_vdtm.f:\
  $(d0root)/stp/vtx/make_vdtm.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/vtx/make_vdtm.for | vmstounix > $(d0root)/stp/vtx/make_vdtm.f
$(d0root)/stp/geocal/mh_geom.f:\
  $(d0root)/stp/geocal/mh_geom.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/mh_geom.for | vmstounix > $(d0root)/stp/geocal/mh_geom.f
$(d0root)/stp/geocal/mh_mvol.f:\
  $(d0root)/stp/geocal/mh_mvol.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/mh_mvol.for | vmstounix > $(d0root)/stp/geocal/mh_mvol.f
$(d0root)/stp/geocal/mixmat.f:\
  $(d0root)/stp/geocal/mixmat.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/cmat.def\
  $(d0library)/inc/calgeo.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/mixmat.for | vmstounix > $(d0root)/stp/geocal/mixmat.f
$(d0root)/stp/geocal/module_corners.f:\
  $(d0root)/stp/geocal/module_corners.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/params/clga.params\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/region.def
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/module_corners.for | vmstounix > $(d0root)/stp/geocal/module_corners.f
$(d0root)/stp/geocal/module_markers.f:\
  $(d0root)/stp/geocal/module_markers.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/region.def\
  $(d0library)/inc/pi.def\
  $(d0library)/params/clga.params
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/module_markers.for | vmstounix > $(d0root)/stp/geocal/module_markers.f
$(d0root)/stp/geocal/oh_geom.f:\
  $(d0root)/stp/geocal/oh_geom.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/oh_geom.for | vmstounix > $(d0root)/stp/geocal/oh_geom.f
$(d0root)/stp/geocal/oh_mvol.f:\
  $(d0root)/stp/geocal/oh_mvol.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/oh_mvol.for | vmstounix > $(d0root)/stp/geocal/oh_mvol.f
$(d0root)/stp/trd/pack_real_stp.f:\
  $(d0root)/stp/trd/pack_real_stp.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/trd/pack_real_stp.for | vmstounix > $(d0root)/stp/trd/pack_real_stp.f
$(d0root)/stp/geocal/pcone.f:\
  $(d0root)/stp/geocal/pcone.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/pcone.for | vmstounix > $(d0root)/stp/geocal/pcone.f
$(d0root)/stp/geocal/prclay.f:\
  $(d0root)/stp/geocal/prclay.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clay.def
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/prclay.for | vmstounix > $(d0root)/stp/geocal/prclay.f
$(d0root)/stp/geocal/prclin.f:\
  $(d0root)/stp/geocal/prclin.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/params/clin.params
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/prclin.for | vmstounix > $(d0root)/stp/geocal/prclin.f
$(d0root)/stp/geocal/prclyr.f:\
  $(d0root)/stp/geocal/prclyr.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/params/clyr.params
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/prclyr.for | vmstounix > $(d0root)/stp/geocal/prclyr.f
$(d0root)/stp/geocal/prcmdl.f:\
  $(d0root)/stp/geocal/prcmdl.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/prcmdl.for | vmstounix > $(d0root)/stp/geocal/prcmdl.f
$(d0root)/stp/geocal/prcthe.f:\
  $(d0root)/stp/geocal/prcthe.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/calor_survey.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/prcthe.for | vmstounix > $(d0root)/stp/geocal/prcthe.f
$(d0root)/stp/geocal/ptrap.f:\
  $(d0root)/stp/geocal/ptrap.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/ptrap.for | vmstounix > $(d0root)/stp/geocal/ptrap.f
$(d0root)/stp/geocal/rmin_max.f:\
  $(d0root)/stp/geocal/rmin_max.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/region.def\
  $(d0library)/params/clga.params\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/clay.def\
  $(d0library)/links/izclga.link\
  $(d0library)/inc/rminmax.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/rmin_max.for | vmstounix > $(d0root)/stp/geocal/rmin_max.f
$(d0root)/stp/samus/sawstp.f:\
  $(d0root)/stp/samus/sawstp.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/samus/sawstp.for | vmstounix > $(d0root)/stp/samus/sawstp.f
$(d0root)/stp/geocal/sbclex.f:\
  $(d0root)/stp/geocal/sbclex.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/clay.def\
  $(d0library)/params/cal_offline.params
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/sbclex.for | vmstounix > $(d0root)/stp/geocal/sbclex.f
$(d0root)/stp/geocal/scal.f:\
  $(d0root)/stp/geocal/scal.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/scal.for | vmstounix > $(d0root)/stp/geocal/scal.f
$(d0root)/stp/geocal/scnrcp.f:\
  $(d0root)/stp/geocal/scnrcp.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/scparr.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/params/clga.params
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/scnrcp.for | vmstounix > $(d0root)/stp/geocal/scnrcp.f
$(d0root)/stp/geocal/shpsrc.f:\
  $(d0root)/stp/geocal/shpsrc.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/scparr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/shpsrc.for | vmstounix > $(d0root)/stp/geocal/shpsrc.f
$(d0root)/stp/geocal/skin.f:\
  $(d0root)/stp/geocal/skin.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/calgeo.inc\
  $(d0library)/inc/clga.def\
  $(d0library)/inc/region.def\
  $(d0library)/inc/csha.def\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/skin.for | vmstounix > $(d0root)/stp/geocal/skin.f
$(d0root)/stp/geocal/spnmod.f:\
  $(d0root)/stp/geocal/spnmod.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/inc/calgeo.inc\
  $(d0library)/inc/clga.def\
  $(d0library)/inc/region.def\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/spnmod.for | vmstounix > $(d0root)/stp/geocal/spnmod.f
$(d0root)/stp/geocal/survey.f:\
  $(d0root)/stp/geocal/survey.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izscal.link\
  $(d0library)/links/izclin.link\
  $(d0library)/links/izcgeh.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/survey.for | vmstounix > $(d0root)/stp/geocal/survey.f
$(d0root)/stp/geocal/survey_rcp.f:\
  $(d0root)/stp/geocal/survey_rcp.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/survey_rcp.for | vmstounix > $(d0root)/stp/geocal/survey_rcp.f
$(d0root)/stp/trd/tcorfl.f:\
  $(d0root)/stp/trd/tcorfl.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/trd/tcorfl.for | vmstounix > $(d0root)/stp/trd/tcorfl.f
$(d0root)/stp/trd/tnd1fl.f:\
  $(d0root)/stp/trd/tnd1fl.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/trd/tnd1fl.for | vmstounix > $(d0root)/stp/trd/tnd1fl.f
$(d0root)/stp/trd/tnd2fl.f:\
  $(d0root)/stp/trd/tnd2fl.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/trd/tnd2fl.for | vmstounix > $(d0root)/stp/trd/tnd2fl.f
$(d0root)/stp/geocal/tower_geom.f:\
  $(d0root)/stp/geocal/tower_geom.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/tower_geom.for | vmstounix > $(d0root)/stp/geocal/tower_geom.f
$(d0root)/stp/geocal/towgeo.f:\
  $(d0root)/stp/geocal/towgeo.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/clinks.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/towgeo.for | vmstounix > $(d0root)/stp/geocal/towgeo.f
$(d0root)/stp/geocal/towshp.f:\
  $(d0root)/stp/geocal/towshp.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/towshp.for | vmstounix > $(d0root)/stp/geocal/towshp.f
$(d0root)/stp/trd/trwstp.f:\
  $(d0root)/stp/trd/trwstp.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/trd/trwstp.for | vmstounix > $(d0root)/stp/trd/trwstp.f
$(d0root)/stp/trd/tstpbk.f:\
  $(d0root)/stp/trd/tstpbk.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/ltrd.inc\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izstrd.link\
  $(d0library)/links/iztpdh.link\
  $(d0library)/links/iztgai.link\
  $(d0library)/links/iztgen.link\
  $(d0library)/links/iztgeo.link\
  $(d0library)/links/iztcan.link\
  $(d0library)/links/iztelh.link\
  $(d0library)/links/iztwgh.link\
  $(d0library)/links/iztrop.link\
  $(d0library)/links/iztphy.link\
  $(d0library)/links/iztcha.link\
  $(d0library)/links/iztca1.link\
  $(d0library)/links/iztca2.link\
  $(d0library)/links/iztmae.link\
  $(d0library)/links/iztmxe.link\
  $(d0library)/links/iztmaa.link\
  $(d0library)/links/iztmab.link\
  $(d0library)/links/iztmac.link\
  $(d0library)/links/iztpro.link\
  $(d0library)/links/iztpr1.link\
  $(d0library)/links/iztpr2.link\
  $(d0library)/links/iztpet.link\
  $(d0library)/links/iztpe3.link\
  $(d0library)/links/iztptr.link\
  $(d0library)/links/iztpt3.link\
  $(d0library)/links/iztple.link\
  $(d0library)/links/iztpec.link\
  $(d0library)/links/iztpca.link\
  $(d0library)/links/iztpcb.link\
  $(d0library)/links/iztpcc.link\
  $(d0library)/links/izt1a0.link\
  $(d0library)/links/izt1b0.link\
  $(d0library)/links/iztmat.link\
  $(d0library)/links/iztach.link\
  $(d0library)/links/iztdmc.link\
  $(d0library)/links/iztdda.link\
  $(d0library)/links/iztlik.link\
  $(d0library)/links/iztpio.link\
  $(d0library)/links/iztpi1.link\
  $(d0library)/links/iztpi2.link\
  $(d0library)/links/iztpde.link\
  $(d0library)/links/iztele.link\
  $(d0library)/links/iztel1.link\
  $(d0library)/links/iztel2.link\
  $(d0library)/links/iztede.link\
  $(d0library)/links/iztlta.link\
  $(d0library)/links/iztden.link\
  $(d0library)/links/iztdor.link\
  $(d0library)/links/iztdpi.link\
  $(d0library)/links/iztdfa.link\
  $(d0library)/links/iztdel.link\
  $(d0library)/links/iztdco.link\
  $(d0library)/links/iztcy1.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/trd/tstpbk.for | vmstounix > $(d0root)/stp/trd/tstpbk.f
$(d0root)/stp/trd/tsttyp.f:\
  $(d0root)/stp/trd/tsttyp.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/iztelh.link\
  $(d0library)/links/iztwgh.link\
  $(d0library)/links/iztrop.link\
  $(d0library)/links/iztlik.link\
  $(d0library)/links/iztrle.link\
  $(d0library)/links/iztrlc.link\
  $(d0library)/links/iztach.link\
  $(d0library)/links/iztphy.link\
  $(d0library)/links/iztcha.link\
  $(d0library)/links/iztca1.link\
  $(d0library)/links/iztca2.link\
  $(d0library)/links/iztmae.link\
  $(d0library)/links/iztmxe.link\
  $(d0library)/links/iztmaa.link\
  $(d0library)/links/iztmab.link\
  $(d0library)/links/iztmac.link\
  $(d0library)/links/iztpro.link\
  $(d0library)/links/iztpr1.link\
  $(d0library)/links/iztpr2.link\
  $(d0library)/links/iztpet.link\
  $(d0library)/links/iztpe3.link\
  $(d0library)/links/iztptr.link\
  $(d0library)/links/iztpt3.link\
  $(d0library)/links/iztple.link\
  $(d0library)/links/iztpec.link\
  $(d0library)/links/iztpca.link\
  $(d0library)/links/iztpcb.link\
  $(d0library)/links/iztpcc.link\
  $(d0library)/links/iztdmc.link\
  $(d0library)/links/iztdda.link\
  $(d0library)/links/iztpio.link\
  $(d0library)/links/iztele.link\
  $(d0library)/links/iztpi1.link\
  $(d0library)/links/iztpi2.link\
  $(d0library)/links/iztpde.link\
  $(d0library)/links/iztel1.link\
  $(d0library)/links/iztel2.link\
  $(d0library)/links/iztede.link\
  $(d0library)/links/iztlta.link\
  $(d0library)/links/iztden.link\
  $(d0library)/links/iztdor.link\
  $(d0library)/links/iztdpi.link\
  $(d0library)/links/iztdfa.link\
  $(d0library)/links/iztdel.link\
  $(d0library)/links/iztdco.link\
  $(d0library)/inc/ltrd.inc\
  $(d0library)/links/izt1a0.link\
  $(d0library)/links/izt1b0.link\
  $(d0library)/links/iztcy1.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/trd/tsttyp.for | vmstounix > $(d0root)/stp/trd/tsttyp.f
$(d0root)/stp/vtx/vals_rotate.f:\
  $(d0root)/stp/vtx/vals_rotate.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/vtx/vals_rotate.for | vmstounix > $(d0root)/stp/vtx/vals_rotate.f
$(d0root)/stp/vtx/vals_translate.f:\
  $(d0root)/stp/vtx/vals_translate.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/vtx/vals_translate.for | vmstounix > $(d0root)/stp/vtx/vals_translate.f
$(d0root)/stp/vtx/vals_update.f:\
  $(d0root)/stp/vtx/vals_update.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/vtx/vals_update.for | vmstounix > $(d0root)/stp/vtx/vals_update.f
$(d0root)/stp/vtx/vdtm_ini.f:\
  $(d0root)/stp/vtx/vdtm_ini.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izvdtm.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/vtx/vdtm_ini.for | vmstounix > $(d0root)/stp/vtx/vdtm_ini.f
$(d0root)/stp/vtx/vdtm_read.f:\
  $(d0root)/stp/vtx/vdtm_read.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/vtx/vdtm_read.for | vmstounix > $(d0root)/stp/vtx/vdtm_read.f
$(d0root)/stp/vtx/vdtmfl.f:\
  $(d0root)/stp/vtx/vdtmfl.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izvdtm.link
	tasteofd0 $(FLAVOR) < $(d0root)/stp/vtx/vdtmfl.for | vmstounix > $(d0root)/stp/vtx/vdtmfl.f
$(d0root)/stp/vtx/vgnl_update.f:\
  $(d0root)/stp/vtx/vgnl_update.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/vtx/vgnl_update.for | vmstounix > $(d0root)/stp/vtx/vgnl_update.f
$(d0root)/stp/vtx/vmult.f:\
  $(d0root)/stp/vtx/vmult.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/vtx/vmult.for | vmstounix > $(d0root)/stp/vtx/vmult.f
$(d0root)/stp/vtx/vrotate.f:\
  $(d0root)/stp/vtx/vrotate.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/vtx/vrotate.for | vmstounix > $(d0root)/stp/vtx/vrotate.f
$(d0root)/stp/vtx/vtmw_update.f:\
  $(d0root)/stp/vtx/vtmw_update.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/vtx/vtmw_update.for | vmstounix > $(d0root)/stp/vtx/vtmw_update.f
$(d0root)/stp/vtx/vtstor.f:\
  $(d0root)/stp/vtx/vtstor.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/vtx/vtstor.for | vmstounix > $(d0root)/stp/vtx/vtstor.f
$(d0root)/stp/vtx/vtwstp.f:\
  $(d0root)/stp/vtx/vtwstp.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/vtx/vtwstp.for | vmstounix > $(d0root)/stp/vtx/vtwstp.f
$(d0root)/stp/geocal/write_iuset.f:\
  $(d0root)/stp/geocal/write_iuset.for\
  $(d0library)/inc/iuset.inc\
  $(d0library)/inc/write_unit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/write_iuset.for | vmstounix > $(d0root)/stp/geocal/write_iuset.f
$(d0root)/stp/geocal/write_materials.f:\
  $(d0root)/stp/geocal/write_materials.for\
  $(d0library)/inc/material_list.inc\
  $(d0library)/inc/material.inc\
  $(d0library)/inc/write_unit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/write_materials.for | vmstounix > $(d0root)/stp/geocal/write_materials.f
$(d0root)/stp/geocal/write_matrices.f:\
  $(d0root)/stp/geocal/write_matrices.for\
  $(d0library)/inc/matrix_list.inc\
  $(d0library)/inc/matrix.inc\
  $(d0library)/inc/write_unit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/write_matrices.for | vmstounix > $(d0root)/stp/geocal/write_matrices.f
$(d0root)/stp/geocal/write_volume.f:\
  $(d0root)/stp/geocal/write_volume.for\
  $(d0library)/inc/srcp_volume.inc\
  $(d0library)/inc/write_unit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/write_volume.for | vmstounix > $(d0root)/stp/geocal/write_volume.f
$(d0root)/stp/geocal/write_zdiv.f:\
  $(d0root)/stp/geocal/write_zdiv.for\
  $(d0library)/inc/write_unit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/write_zdiv.for | vmstounix > $(d0root)/stp/geocal/write_zdiv.f
$(d0root)/stp/geocal/wrtzdv.f:\
  $(d0root)/stp/geocal/wrtzdv.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/wrtzdv.for | vmstounix > $(d0root)/stp/geocal/wrtzdv.f
$(d0root)/stp/geocal/wrtzdv1.f:\
  $(d0root)/stp/geocal/wrtzdv1.for
	tasteofd0 $(FLAVOR) < $(d0root)/stp/geocal/wrtzdv1.for | vmstounix > $(d0root)/stp/geocal/wrtzdv1.f
for:
	touch $(d0root)/stp/geocal/bkclin.for
	touch $(d0root)/stp/geocal/bkcmdl.for
	touch $(d0root)/stp/geocal/bkcsrv.for
	touch $(d0root)/stp/geocal/bkcsyl.for
	touch $(d0root)/stp/geocal/bkcthe.for
	touch $(d0root)/stp/vtx/bkvdtm.for
	touch $(d0root)/stp/cdc/bldalh.for
	touch $(d0root)/stp/cdc/bldalh_0.for
	touch $(d0root)/stp/cdc/bldcbd.for
	touch $(d0root)/stp/cdc/bldcbd_d0.for
	touch $(d0root)/stp/cdc/bldgeh.for
	touch $(d0root)/stp/cdc/bldgnh.for
	touch $(d0root)/stp/cdc/bldmat.for
	touch $(d0root)/stp/cdc/bldnlx.for
	touch $(d0root)/stp/cdc/bldpdh.for
	touch $(d0root)/stp/cdc/bldrft.for
	touch $(d0root)/stp/cdc/bldtmh.for
	touch $(d0root)/stp/cdc/bldtmp.for
	touch $(d0root)/stp/cdc/bldtva.for
	touch $(d0root)/stp/cdc/bldwal.for
	touch $(d0root)/stp/cdc/bldxxp.for
	touch $(d0root)/stp/fdc/blfalh.for
	touch $(d0root)/stp/fdc/blfalh_d0.for
	touch $(d0root)/stp/fdc/blfalh_v2.for
	touch $(d0root)/stp/fdc/blfdph.for
	touch $(d0root)/stp/fdc/blfdrt.for
	touch $(d0root)/stp/fdc/blfdrt_d0.for
	touch $(d0root)/stp/fdc/blfdta.for
	touch $(d0root)/stp/fdc/blfdta_d0.for
	touch $(d0root)/stp/fdc/blfdtb.for
	touch $(d0root)/stp/fdc/blfdtb_d0.for
	touch $(d0root)/stp/fdc/blfgeh.for
	touch $(d0root)/stp/fdc/blfgeh_d0.for
	touch $(d0root)/stp/fdc/blfgeh_v2.for
	touch $(d0root)/stp/fdc/blfgnh.for
	touch $(d0root)/stp/fdc/blflis.for
	touch $(d0root)/stp/fdc/blfmat.for
	touch $(d0root)/stp/fdc/blfpdh.for
	touch $(d0root)/stp/fdc/blftmh.for
	touch $(d0root)/stp/fdc/blftmh_d0.for
	touch $(d0root)/stp/fdc/blftmh_v2.for
	touch $(d0root)/stp/fdc/blfwal.for
	touch $(d0root)/stp/fdc/blfwph.for
	touch $(d0root)/stp/fdc/blfwta.for
	touch $(d0root)/stp/fdc/blfwtb.for
	touch $(d0root)/stp/geogen/blsgbp.for
	touch $(d0root)/stp/geogen/blsgen.for
	touch $(d0root)/stp/geogen/blsgmc.for
	touch $(d0root)/stp/trd/bltgai.for
	touch $(d0root)/stp/trd/bltpdh.for
	touch $(d0root)/stp/vtx/blvalh.for
	touch $(d0root)/stp/vtx/blvgeh.for
	touch $(d0root)/stp/vtx/blvgnh.for
	touch $(d0root)/stp/vtx/blvmat.for
	touch $(d0root)/stp/vtx/blvpdh.for
	touch $(d0root)/stp/vtx/blvrft.for
	touch $(d0root)/stp/vtx/blvtmh.for
	touch $(d0root)/stp/vtx/blvwal.for
	touch $(d0root)/stp/geocal/bmpipe.for
	touch $(d0root)/stp/geocal/cabsrb.for
	touch $(d0root)/stp/geocal/cal_alignments.for
	touch $(d0root)/stp/geocal/cal_postgeo.for
	touch $(d0root)/stp/geocal/cal_pregeo.for
	touch $(d0root)/stp/geocal/cal_survey.for
	touch $(d0root)/stp/geocal/cal_survey_ntuple.for
	touch $(d0root)/stp/geocal/cawstp.for
	touch $(d0root)/stp/geocal/cc_geom.for
	touch $(d0root)/stp/geocal/cc_iuset.for
	touch $(d0root)/stp/geocal/cc_massless_gap.for
	touch $(d0root)/stp/geocal/cc_mg_div.for
	touch $(d0root)/stp/geocal/cc_survey_cylinder.for
	touch $(d0root)/stp/geocal/cc_survey_fit.for
	touch $(d0root)/stp/geocal/cc_survey_in_zebra.for
	touch $(d0root)/stp/geocal/cc_survey_module.for
	touch $(d0root)/stp/geocal/cc_theo_geo_banks.for
	touch $(d0root)/stp/geocal/cc_write_iuset.for
	touch $(d0root)/stp/geocal/cc_write_materials.for
	touch $(d0root)/stp/geocal/cc_write_matrices.for
	touch $(d0root)/stp/geocal/cc_write_volume.for
	touch $(d0root)/stp/geocal/ccch_backplate.for
	touch $(d0root)/stp/geocal/ccch_cells.for
	touch $(d0root)/stp/geocal/ccch_crack.for
	touch $(d0root)/stp/geocal/ccch_endplate.for
	touch $(d0root)/stp/geocal/ccch_floor.for
	touch $(d0root)/stp/geocal/ccch_frontplate.for
	touch $(d0root)/stp/geocal/ccch_geom.for
	touch $(d0root)/stp/geocal/ccch_module.for
	touch $(d0root)/stp/geocal/ccch_mother.for
	touch $(d0root)/stp/geocal/ccch_mr_beampipe.for
	touch $(d0root)/stp/geocal/ccch_mr_cutout.for
	touch $(d0root)/stp/geocal/ccch_rotation_matrices.for
	touch $(d0root)/stp/geocal/ccem_backplate.for
	touch $(d0root)/stp/geocal/ccem_basic_module.for
	touch $(d0root)/stp/geocal/ccem_cells.for
	touch $(d0root)/stp/geocal/ccem_crack.for
	touch $(d0root)/stp/geocal/ccem_endplate.for
	touch $(d0root)/stp/geocal/ccem_endplate_iuset.for
	touch $(d0root)/stp/geocal/ccem_floor.for
	touch $(d0root)/stp/geocal/ccem_frontplate.for
	touch $(d0root)/stp/geocal/ccem_geom.for
	touch $(d0root)/stp/geocal/ccem_homo_geom.for
	touch $(d0root)/stp/geocal/ccem_homo_iuset.for
	touch $(d0root)/stp/geocal/ccem_module.for
	touch $(d0root)/stp/geocal/ccem_mother.for
	touch $(d0root)/stp/geocal/ccem_plate_geom.for
	touch $(d0root)/stp/geocal/ccem_plate_iuset.for
	touch $(d0root)/stp/geocal/ccem_rotation_matrices.for
	touch $(d0root)/stp/geocal/ccem_skin_geom.for
	touch $(d0root)/stp/geocal/ccem_skin_iuset.for
	touch $(d0root)/stp/geocal/ccfh_backplate.for
	touch $(d0root)/stp/geocal/ccfh_cells.for
	touch $(d0root)/stp/geocal/ccfh_crack.for
	touch $(d0root)/stp/geocal/ccfh_endplate.for
	touch $(d0root)/stp/geocal/ccfh_floor.for
	touch $(d0root)/stp/geocal/ccfh_frontplate.for
	touch $(d0root)/stp/geocal/ccfh_geom.for
	touch $(d0root)/stp/geocal/ccfh_module.for
	touch $(d0root)/stp/geocal/ccfh_mother.for
	touch $(d0root)/stp/geocal/ccfh_rotation_matrices.for
	touch $(d0root)/stp/geocal/ccpad.for
	touch $(d0root)/stp/cdc/cdwstp.for
	touch $(d0root)/stp/geocal/cedp.for
	touch $(d0root)/stp/geocal/celicd.for
	touch $(d0root)/stp/geocal/celpos.for
	touch $(d0root)/stp/geocal/celpsi.for
	touch $(d0root)/stp/geocal/celsha.for
	touch $(d0root)/stp/geocal/cenp.for
	touch $(d0root)/stp/geocal/cenpcc.for
	touch $(d0root)/stp/geocal/cenpeh.for
	touch $(d0root)/stp/geocal/ceta.for
	touch $(d0root)/stp/geocal/cgeh.for
	touch $(d0root)/stp/geocal/chaba.for
	touch $(d0root)/stp/geocal/clay.for
	touch $(d0root)/stp/geocal/claycc.for
	touch $(d0root)/stp/geocal/clayee.for
	touch $(d0root)/stp/geocal/clayih.for
	touch $(d0root)/stp/geocal/claymg_cc.for
	touch $(d0root)/stp/geocal/claymg_ec.for
	touch $(d0root)/stp/geocal/claymh.for
	touch $(d0root)/stp/geocal/clayoh.for
	touch $(d0root)/stp/geocal/clga.for
	touch $(d0root)/stp/geocal/clgacc.for
	touch $(d0root)/stp/geocal/clgaee.for
	touch $(d0root)/stp/geocal/clgaeh.for
	touch $(d0root)/stp/geocal/clgi.for
	touch $(d0root)/stp/geocal/clgicc.for
	touch $(d0root)/stp/geocal/clgiee.for
	touch $(d0root)/stp/geocal/clgieh.for
	touch $(d0root)/stp/geocal/clgsrc.for
	touch $(d0root)/stp/geocal/clrrcp.for
	touch $(d0root)/stp/geocal/clyr.for
	touch $(d0root)/stp/geocal/cmat.for
	touch $(d0root)/stp/geocal/cmsgcc.for
	touch $(d0root)/stp/geocal/cmsgec.for
	touch $(d0root)/stp/geocal/cold_shrink.for
	touch $(d0root)/stp/geocal/cpla.for
	touch $(d0root)/stp/geocal/cprzy.for
	touch $(d0root)/stp/geocal/creg.for
	touch $(d0root)/stp/geocal/crscel.for
	touch $(d0root)/stp/geocal/crscel_eh.for
	touch $(d0root)/stp/geocal/crscel_oh.for
	touch $(d0root)/stp/geocal/crycen.for
	touch $(d0root)/stp/geocal/crycnt.for
	touch $(d0root)/stp/geocal/crydrw.for
	touch $(d0root)/stp/geocal/cryenc.for
	touch $(d0root)/stp/geocal/cryend.for
	touch $(d0root)/stp/geocal/cryo.for
	touch $(d0root)/stp/geocal/cryocc.for
	touch $(d0root)/stp/geocal/cryoec.for
	touch $(d0root)/stp/geocal/cryout.for
	touch $(d0root)/stp/geocal/cryplt.for
	touch $(d0root)/stp/geocal/cryprt.for
	touch $(d0root)/stp/geocal/cryset.for
	touch $(d0root)/stp/geocal/cscn.for
	touch $(d0root)/stp/geocal/csfmake.for
	touch $(d0root)/stp/cdc/dcstor.for
	touch $(d0root)/stp/geocal/do_em_geom.for
	touch $(d0root)/stp/geocal/do_ih_geom.for
	touch $(d0root)/stp/geocal/do_mh_geom.for
	touch $(d0root)/stp/geocal/do_misc.for
	touch $(d0root)/stp/geocal/do_oh_geom.for
	touch $(d0root)/stp/cdc/drddly.for
	touch $(d0root)/stp/cdc/drdgns.for
	touch $(d0root)/stp/geocal/ec_iusets.for
	touch $(d0root)/stp/geocal/ec_mixtures.for
	touch $(d0root)/stp/geocal/ec_rot_matrices.for
	touch $(d0root)/stp/geocal/ec_survey_fit.for
	touch $(d0root)/stp/geocal/ecem_homo_geom.for
	touch $(d0root)/stp/geocal/ecem_homo_iuset.for
	touch $(d0root)/stp/geocal/ecem_homo_volumes.for
	touch $(d0root)/stp/geocal/ecem_misc_geom.for
	touch $(d0root)/stp/geocal/ecem_plate_floor.for
	touch $(d0root)/stp/geocal/ecem_plate_geom.for
	touch $(d0root)/stp/geocal/ecem_plate_iuset.for
	touch $(d0root)/stp/geocal/ecem_plate_volumes.for
	touch $(d0root)/stp/geocal/ecem_tower_geom.for
	touch $(d0root)/stp/geocal/ecih_argon_gap_geom.for
	touch $(d0root)/stp/geocal/ecih_back_plate_geom.for
	touch $(d0root)/stp/geocal/ecih_basic_module.for
	touch $(d0root)/stp/geocal/ecih_coarse_abs_geom.for
	touch $(d0root)/stp/geocal/ecih_fine_abs_geom.for
	touch $(d0root)/stp/geocal/ecih_front_plate_geom.for
	touch $(d0root)/stp/geocal/ecih_homo_geom.for
	touch $(d0root)/stp/geocal/ecih_homo_iuset.for
	touch $(d0root)/stp/geocal/ecih_homo_volumes.for
	touch $(d0root)/stp/geocal/ecih_misc_geom.for
	touch $(d0root)/stp/geocal/ecih_mlb_geom.for
	touch $(d0root)/stp/geocal/ecih_plate_geom.for
	touch $(d0root)/stp/geocal/ecih_plate_iuset.for
	touch $(d0root)/stp/geocal/ecih_plate_volumes.for
	touch $(d0root)/stp/geocal/ecih_support_pipe_geom.for
	touch $(d0root)/stp/geocal/ecih_support_plate_geom.for
	touch $(d0root)/stp/geocal/ecmh_basic_module.for
	touch $(d0root)/stp/geocal/ecmh_homo_geom.for
	touch $(d0root)/stp/geocal/ecmh_homo_iuset.for
	touch $(d0root)/stp/geocal/ecmh_homo_volumes.for
	touch $(d0root)/stp/geocal/ecmh_misc_geom.for
	touch $(d0root)/stp/geocal/ecmh_plate_geom.for
	touch $(d0root)/stp/geocal/ecmh_plate_iuset.for
	touch $(d0root)/stp/geocal/ecmh_plate_volumes.for
	touch $(d0root)/stp/geocal/ecmh_unit.for
	touch $(d0root)/stp/geocal/ecn_survey_analyze.for
	touch $(d0root)/stp/geocal/ecn_survey_in_zebra.for
	touch $(d0root)/stp/geocal/ecn_survey_module.for
	touch $(d0root)/stp/geocal/ecn_survey_rcp.for
	touch $(d0root)/stp/geocal/ecn_theo_geo_banks.for
	touch $(d0root)/stp/geocal/ecpad.for
	touch $(d0root)/stp/geocal/ecpad_mh.for
	touch $(d0root)/stp/geocal/ecs_survey_analyze.for
	touch $(d0root)/stp/geocal/ecs_survey_in_zebra.for
	touch $(d0root)/stp/geocal/ecs_survey_module.for
	touch $(d0root)/stp/geocal/ecs_survey_rcp.for
	touch $(d0root)/stp/geocal/ecs_theo_geo_banks.for
	touch $(d0root)/stp/geocal/em_geom.for
	touch $(d0root)/stp/geocal/fcwrit.for
	touch $(d0root)/stp/fdc/fdwstp_d0.for
	touch $(d0root)/stp/fdc/fdwstp_mc.for
	touch $(d0root)/stp/trd/fitca.for
	touch $(d0root)/stp/trd/fitcha.for
	touch $(d0root)/stp/trd/fitma.for
	touch $(d0root)/stp/trd/fitmae.for
	touch $(d0root)/stp/trd/fitmxe.for
	touch $(d0root)/stp/trd/fitpc.for
	touch $(d0root)/stp/trd/fitpe3.for
	touch $(d0root)/stp/trd/fitpec.for
	touch $(d0root)/stp/trd/fitpet.for
	touch $(d0root)/stp/trd/fitphy.for
	touch $(d0root)/stp/trd/fitple.for
	touch $(d0root)/stp/trd/fitpr.for
	touch $(d0root)/stp/trd/fitpro.for
	touch $(d0root)/stp/trd/fitpt3.for
	touch $(d0root)/stp/trd/fitptr.for
	touch $(d0root)/stp/geocal/flrsrc.for
	touch $(d0root)/stp/fdc/fstpar.for
	touch $(d0root)/stp/fdc/fstpar_v2.for
	touch $(d0root)/stp/geocal/gang_cells.for
	touch $(d0root)/stp/geogen/gnwstp.for
	touch $(d0root)/stp/geocal/ih_geom.for
	touch $(d0root)/stp/geocal/ih_pre_pre.for
	touch $(d0root)/stp/geocal/int_date.for
	touch $(d0root)/stp/geocal/inzstp.for
	touch $(d0root)/stp/geocal/jdepth.for
	touch $(d0root)/stp/geocal/lgtype.for
	touch $(d0root)/stp/geocal/lnkenp.for
	touch $(d0root)/stp/geocal/lnklay.for
	touch $(d0root)/stp/geocal/make_icd.for
	touch $(d0root)/stp/geocal/make_lv0.for
	touch $(d0root)/stp/geocal/make_lv0_cover.for
	touch $(d0root)/stp/geocal/make_lv0_mixture.for
	touch $(d0root)/stp/geocal/make_lv0_mother.for
	touch $(d0root)/stp/geocal/make_lv0_pmt.for
	touch $(d0root)/stp/geocal/make_lv0_set.for
	touch $(d0root)/stp/geocal/make_lv0_side_skin.for
	touch $(d0root)/stp/geocal/make_lv0_tile.for
	touch $(d0root)/stp/geocal/make_lv0_write.for
	touch $(d0root)/stp/geocal/make_lv0_write2.for
	touch $(d0root)/stp/vtx/make_new_stp.for
	touch $(d0root)/stp/vtx/make_vdtm.for
	touch $(d0root)/stp/geocal/mh_geom.for
	touch $(d0root)/stp/geocal/mh_mvol.for
	touch $(d0root)/stp/geocal/mixmat.for
	touch $(d0root)/stp/geocal/module_corners.for
	touch $(d0root)/stp/geocal/module_markers.for
	touch $(d0root)/stp/geocal/oh_geom.for
	touch $(d0root)/stp/geocal/oh_mvol.for
	touch $(d0root)/stp/trd/pack_real_stp.for
	touch $(d0root)/stp/geocal/pcone.for
	touch $(d0root)/stp/geocal/prclay.for
	touch $(d0root)/stp/geocal/prclin.for
	touch $(d0root)/stp/geocal/prclyr.for
	touch $(d0root)/stp/geocal/prcmdl.for
	touch $(d0root)/stp/geocal/prcthe.for
	touch $(d0root)/stp/geocal/ptrap.for
	touch $(d0root)/stp/geocal/rmin_max.for
	touch $(d0root)/stp/samus/sawstp.for
	touch $(d0root)/stp/geocal/sbclex.for
	touch $(d0root)/stp/geocal/scal.for
	touch $(d0root)/stp/geocal/scnrcp.for
	touch $(d0root)/stp/geocal/shpsrc.for
	touch $(d0root)/stp/geocal/skin.for
	touch $(d0root)/stp/geocal/spnmod.for
	touch $(d0root)/stp/geocal/survey.for
	touch $(d0root)/stp/geocal/survey_rcp.for
	touch $(d0root)/stp/trd/tcorfl.for
	touch $(d0root)/stp/trd/tnd1fl.for
	touch $(d0root)/stp/trd/tnd2fl.for
	touch $(d0root)/stp/geocal/tower_geom.for
	touch $(d0root)/stp/geocal/towgeo.for
	touch $(d0root)/stp/geocal/towshp.for
	touch $(d0root)/stp/trd/trwstp.for
	touch $(d0root)/stp/trd/tstpbk.for
	touch $(d0root)/stp/trd/tsttyp.for
	touch $(d0root)/stp/vtx/vals_rotate.for
	touch $(d0root)/stp/vtx/vals_translate.for
	touch $(d0root)/stp/vtx/vals_update.for
	touch $(d0root)/stp/vtx/vdtm_ini.for
	touch $(d0root)/stp/vtx/vdtm_read.for
	touch $(d0root)/stp/vtx/vdtmfl.for
	touch $(d0root)/stp/vtx/vgnl_update.for
	touch $(d0root)/stp/vtx/vmult.for
	touch $(d0root)/stp/vtx/vrotate.for
	touch $(d0root)/stp/vtx/vtmw_update.for
	touch $(d0root)/stp/vtx/vtstor.for
	touch $(d0root)/stp/vtx/vtwstp.for
	touch $(d0root)/stp/geocal/write_iuset.for
	touch $(d0root)/stp/geocal/write_materials.for
	touch $(d0root)/stp/geocal/write_matrices.for
	touch $(d0root)/stp/geocal/write_volume.for
	touch $(d0root)/stp/geocal/write_zdiv.for
	touch $(d0root)/stp/geocal/wrtzdv.for
	touch $(d0root)/stp/geocal/wrtzdv1.for
