SHELL = /bin/sh
CCFLAGS = -c -dollar -G 0 -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0test)/unix/source -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
LCCFLAGS = -c -dollar -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0test)/unix/source -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
F77FLAGS = -c -nocpp -static -col72 -backslash -G 0 -Olimit 2500 -Wo,-loopunroll,2
DEBUG = -O0 -g2
OPT = -O2
ARFLAGS = crsl
SCRATCH = $(d0library)/admin/tmp/d0librar/mmstomake/util
FLAVOR = SIUNIX
F77 = f77
.IGNORE:
.SUFFIXES:

opt :\
  bstat.a\
  d0flavor.a\
  fzdiff.a\
  sources.a\
  util.a
bstat.a : $(d0root)/util/bstat.a
	@ echo bstat.a is up to date
$(d0root)/util/bstat.a:: $(SCRATCH)/bstat/nothing.nl
$(d0root)/util/bstat.a::
	rmdirp $(SCRATCH)/bstat
$(SCRATCH)/bstat/nothing.nl:
	rmdirp $(SCRATCH)/bstat
d0flavor.a : $(d0root)/util/d0flavor.a
	@ echo d0flavor.a is up to date
$(d0root)/util/d0flavor.a:: $(SCRATCH)/d0flavor/nothing.nl
$(d0root)/util/d0flavor.a::\
  $(d0root)/util/d0flavor.a(d0flavor.o)\
  $(d0root)/util/d0flavor.a(d0open_text.o)\
  $(d0root)/util/d0flavor.a(decodline544.o)\
  $(d0root)/util/d0flavor.a(kern_util.o)
	ar $(ARFLAGS) $(d0root)/util/d0flavor.a `ls $(SCRATCH)/d0flavor/a1/*.o 2> /dev/null`
	rmdirp $(SCRATCH)/d0flavor
$(SCRATCH)/d0flavor/nothing.nl:
	rmdirp $(SCRATCH)/d0flavor
	mkdirp $(SCRATCH)/d0flavor/a1
$(d0root)/util/d0flavor.a(d0flavor.o):\
  $(d0root)/util/d0flavor/d0flavor.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) util/d0flavor/d0flavor.f ;\
	mv d0flavor.o $(SCRATCH)/d0flavor/a1/d0flavor.o ;\
	)
$(d0root)/util/d0flavor.a(d0open_text.o):\
  $(d0root)/util/d0flavor/d0open_text.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) util/d0flavor/d0open_text.f ;\
	mv d0open_text.o $(SCRATCH)/d0flavor/a1/d0open_text.o ;\
	)
$(d0root)/util/d0flavor.a(decodline544.o):\
  $(d0root)/util/d0flavor/decode_cmd_line.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) util/d0flavor/decode_cmd_line.f ;\
	mv decode_cmd_line.o $(SCRATCH)/d0flavor/a1/decodline544.o ;\
	)
$(d0root)/util/d0flavor.a(kern_util.o):\
  $(d0root)/util/d0flavor/kern_util.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) util/d0flavor/kern_util.f ;\
	mv kern_util.o $(SCRATCH)/d0flavor/a1/kern_util.o ;\
	)
fzdiff.a : $(d0root)/util/fzdiff.a
	@ echo fzdiff.a is up to date
$(d0root)/util/fzdiff.a:: $(SCRATCH)/fzdiff/nothing.nl
$(d0root)/util/fzdiff.a::\
  $(d0root)/util/fzdiff.a(fzdiff.o)\
  $(d0root)/util/fzdiff.a(fzdifname360.o)\
  $(d0root)/util/fzdiff.a(fzdiff_comp.o)\
  $(d0root)/util/fzdiff.a(fzdiff_flat.o)\
  $(d0root)/util/fzdiff.a(fzdifzcom394.o)\
  $(d0root)/util/fzdiff.a(fzdifcomp496.o)\
  $(d0root)/util/fzdiff.a(fzdifcomp400.o)\
  $(d0root)/util/fzdiff.a(fzdiflink399.o)\
  $(d0root)/util/fzdiff.a(fzdiff_struc.o)\
  $(d0root)/util/fzdiff.a(fzeventdis.o)\
  $(d0root)/util/fzdiff.a(fzscramble.o)\
  $(d0root)/util/fzdiff.a(fzsort.o)\
  $(d0root)/util/fzdiff.a(fzton.o)
	ar $(ARFLAGS) $(d0root)/util/fzdiff.a `ls $(SCRATCH)/fzdiff/a1/*.o 2> /dev/null`
	rmdirp $(SCRATCH)/fzdiff
$(SCRATCH)/fzdiff/nothing.nl:
	rmdirp $(SCRATCH)/fzdiff
	mkdirp $(SCRATCH)/fzdiff/a1
$(d0root)/util/fzdiff.a(fzdiff.o):\
  $(d0root)/util/fzdiff/fzdiff.f\
  $(d0root)/util/fzdiff/fzdiff_zebcom.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) util/fzdiff/fzdiff.f ;\
	mv fzdiff.o $(SCRATCH)/fzdiff/a1/fzdiff.o ;\
	)
$(d0root)/util/fzdiff.a(fzdifname360.o):\
  $(d0root)/util/fzdiff/fzdiff_bkname.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) util/fzdiff/fzdiff_bkname.f ;\
	mv fzdiff_bkname.o $(SCRATCH)/fzdiff/a1/fzdifname360.o ;\
	)
$(d0root)/util/fzdiff.a(fzdiff_comp.o):\
  $(d0root)/util/fzdiff/fzdiff_comp.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) util/fzdiff/fzdiff_comp.f ;\
	mv fzdiff_comp.o $(SCRATCH)/fzdiff/a1/fzdiff_comp.o ;\
	)
$(d0root)/util/fzdiff.a(fzdiff_flat.o):\
  $(d0root)/util/fzdiff/fzdiff_flat.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) util/fzdiff/fzdiff_flat.f ;\
	mv fzdiff_flat.o $(SCRATCH)/fzdiff/a1/fzdiff_flat.o ;\
	)
$(d0root)/util/fzdiff.a(fzdifzcom394.o):\
  $(d0root)/util/fzdiff/fzdiff_inzcom.f\
  $(d0root)/util/fzdiff/fzdiff_zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) util/fzdiff/fzdiff_inzcom.f ;\
	mv fzdiff_inzcom.o $(SCRATCH)/fzdiff/a1/fzdifzcom394.o ;\
	)
$(d0root)/util/fzdiff.a(fzdifcomp496.o):\
  $(d0root)/util/fzdiff/fzdiff_precomp.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) util/fzdiff/fzdiff_precomp.f ;\
	mv fzdiff_precomp.o $(SCRATCH)/fzdiff/a1/fzdifcomp496.o ;\
	)
$(d0root)/util/fzdiff.a(fzdifcomp400.o):\
  $(d0root)/util/fzdiff/fzdiff_stcomp.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) util/fzdiff/fzdiff_stcomp.f ;\
	mv fzdiff_stcomp.o $(SCRATCH)/fzdiff/a1/fzdifcomp400.o ;\
	)
$(d0root)/util/fzdiff.a(fzdiflink399.o):\
  $(d0root)/util/fzdiff/fzdiff_stlink.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) util/fzdiff/fzdiff_stlink.f ;\
	mv fzdiff_stlink.o $(SCRATCH)/fzdiff/a1/fzdiflink399.o ;\
	)
$(d0root)/util/fzdiff.a(fzdiff_struc.o):\
  $(d0root)/util/fzdiff/fzdiff_struc.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) util/fzdiff/fzdiff_struc.f ;\
	mv fzdiff_struc.o $(SCRATCH)/fzdiff/a1/fzdiff_struc.o ;\
	)
$(d0root)/util/fzdiff.a(fzeventdis.o):\
  $(d0root)/util/fzdiff/fzeventdis.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) util/fzdiff/fzeventdis.f ;\
	mv fzeventdis.o $(SCRATCH)/fzdiff/a1/fzeventdis.o ;\
	)
$(d0root)/util/fzdiff.a(fzscramble.o):\
  $(d0root)/util/fzdiff/fzscramble.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) util/fzdiff/fzscramble.f ;\
	mv fzscramble.o $(SCRATCH)/fzdiff/a1/fzscramble.o ;\
	)
$(d0root)/util/fzdiff.a(fzsort.o):\
  $(d0root)/util/fzdiff/fzsort.f\
  $(d0root)/util/fzdiff/fzdiff_zebcom.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) util/fzdiff/fzsort.f ;\
	mv fzsort.o $(SCRATCH)/fzdiff/a1/fzsort.o ;\
	)
$(d0root)/util/fzdiff.a(fzton.o):\
  $(d0root)/util/fzdiff/fzton.f\
  $(d0root)/util/fzdiff/fzdiff_zebcom.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/links/izjets.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) util/fzdiff/fzton.f ;\
	mv fzton.o $(SCRATCH)/fzdiff/a1/fzton.o ;\
	)
sources.a : $(d0root)/util/sources.a
	@ echo sources.a is up to date
$(d0root)/util/sources.a:: $(SCRATCH)/sources/nothing.nl
$(d0root)/util/sources.a::
	rmdirp $(SCRATCH)/sources
$(SCRATCH)/sources/nothing.nl:
	rmdirp $(SCRATCH)/sources
util.a : $(d0root)/util/util.a
	@ echo util.a is up to date
$(d0root)/util/util.a:: $(SCRATCH)/util/nothing.nl
$(d0root)/util/util.a::\
  $(d0root)/util/util.a(copy_fz.o)\
  $(d0root)/util/util.a(evnt_list.o)\
  $(d0root)/util/util.a(fz_inzcom.o)\
  $(d0root)/util/util.a(get_str.o)\
  $(d0root)/util/util.a(map.o)
	ar $(ARFLAGS) $(d0root)/util/util.a `ls $(SCRATCH)/util/a1/*.o 2> /dev/null`
	rmdirp $(SCRATCH)/util
$(SCRATCH)/util/nothing.nl:
	rmdirp $(SCRATCH)/util
	mkdirp $(SCRATCH)/util/a1
$(d0root)/util/util.a(copy_fz.o):\
  $(d0root)/util/files/copy_fz.f\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) util/files/copy_fz.f ;\
	mv copy_fz.o $(SCRATCH)/util/a1/copy_fz.o ;\
	)
$(d0root)/util/util.a(evnt_list.o):\
  $(d0root)/util/files/evnt_list.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) util/files/evnt_list.f ;\
	mv evnt_list.o $(SCRATCH)/util/a1/evnt_list.o ;\
	)
$(d0root)/util/util.a(fz_inzcom.o):\
  $(d0root)/util/files/fz_inzcom.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) util/files/fz_inzcom.f ;\
	mv fz_inzcom.o $(SCRATCH)/util/a1/fz_inzcom.o ;\
	)
$(d0root)/util/util.a(get_str.o):\
  $(d0root)/util/files/get_str.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) util/files/get_str.f ;\
	mv get_str.o $(SCRATCH)/util/a1/get_str.o ;\
	)
$(d0root)/util/util.a(map.o):\
  $(d0root)/util/files/map.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) util/files/map.f ;\
	mv map.o $(SCRATCH)/util/a1/map.o ;\
	)
pre:\
  $(d0root)/util/files/copy_fz.f\
  $(d0root)/util/d0flavor/d0flavor.f\
  $(d0root)/util/d0flavor/d0open_text.f\
  $(d0root)/util/d0flavor/decode_cmd_line.f\
  $(d0root)/util/files/evnt_list.f\
  $(d0root)/util/files/fz_inzcom.f\
  $(d0root)/util/fzdiff/fzdiff.f\
  $(d0root)/util/fzdiff/fzdiff_bkname.f\
  $(d0root)/util/fzdiff/fzdiff_comp.f\
  $(d0root)/util/fzdiff/fzdiff_flat.f\
  $(d0root)/util/fzdiff/fzdiff_inzcom.f\
  $(d0root)/util/fzdiff/fzdiff_precomp.f\
  $(d0root)/util/fzdiff/fzdiff_stcomp.f\
  $(d0root)/util/fzdiff/fzdiff_stlink.f\
  $(d0root)/util/fzdiff/fzdiff_struc.f\
  $(d0root)/util/fzdiff/fzeventdis.f\
  $(d0root)/util/fzdiff/fzscramble.f\
  $(d0root)/util/fzdiff/fzsort.f\
  $(d0root)/util/fzdiff/fzton.f\
  $(d0root)/util/files/get_str.f\
  $(d0root)/util/d0flavor/kern_util.f\
  $(d0root)/util/files/map.f
$(d0root)/util/files/copy_fz.f:\
  $(d0root)/util/files/copy_fz.for\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0root)/util/files/copy_fz.for | vmstounix > $(d0root)/util/files/copy_fz.f
$(d0root)/util/d0flavor/d0flavor.f:\
  $(d0root)/util/d0flavor/d0flavor.for
	tasteofd0 $(FLAVOR) < $(d0root)/util/d0flavor/d0flavor.for | vmstounix > $(d0root)/util/d0flavor/d0flavor.f
$(d0root)/util/d0flavor/d0open_text.f:\
  $(d0root)/util/d0flavor/d0open_text.for
	tasteofd0 $(FLAVOR) < $(d0root)/util/d0flavor/d0open_text.for | vmstounix > $(d0root)/util/d0flavor/d0open_text.f
$(d0root)/util/d0flavor/decode_cmd_line.f:\
  $(d0root)/util/d0flavor/decode_cmd_line.for
	tasteofd0 $(FLAVOR) < $(d0root)/util/d0flavor/decode_cmd_line.for | vmstounix > $(d0root)/util/d0flavor/decode_cmd_line.f
$(d0root)/util/files/evnt_list.f:\
  $(d0root)/util/files/evnt_list.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/util/files/evnt_list.for | vmstounix > $(d0root)/util/files/evnt_list.f
$(d0root)/util/files/fz_inzcom.f:\
  $(d0root)/util/files/fz_inzcom.for
	tasteofd0 $(FLAVOR) < $(d0root)/util/files/fz_inzcom.for | vmstounix > $(d0root)/util/files/fz_inzcom.f
$(d0root)/util/fzdiff/fzdiff.f:\
  $(d0root)/util/fzdiff/fzdiff.for\
  $(d0root)/util/fzdiff/fzdiff_zebcom.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0root)/util/fzdiff/fzdiff.for | vmstounix > $(d0root)/util/fzdiff/fzdiff.f
$(d0root)/util/fzdiff/fzdiff_bkname.f:\
  $(d0root)/util/fzdiff/fzdiff_bkname.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/util/fzdiff/fzdiff_bkname.for | vmstounix > $(d0root)/util/fzdiff/fzdiff_bkname.f
$(d0root)/util/fzdiff/fzdiff_comp.f:\
  $(d0root)/util/fzdiff/fzdiff_comp.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0root)/util/fzdiff/fzdiff_comp.for | vmstounix > $(d0root)/util/fzdiff/fzdiff_comp.f
$(d0root)/util/fzdiff/fzdiff_flat.f:\
  $(d0root)/util/fzdiff/fzdiff_flat.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/util/fzdiff/fzdiff_flat.for | vmstounix > $(d0root)/util/fzdiff/fzdiff_flat.f
$(d0root)/util/fzdiff/fzdiff_inzcom.f:\
  $(d0root)/util/fzdiff/fzdiff_inzcom.for\
  $(d0root)/util/fzdiff/fzdiff_zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/util/fzdiff/fzdiff_inzcom.for | vmstounix > $(d0root)/util/fzdiff/fzdiff_inzcom.f
$(d0root)/util/fzdiff/fzdiff_precomp.f:\
  $(d0root)/util/fzdiff/fzdiff_precomp.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/util/fzdiff/fzdiff_precomp.for | vmstounix > $(d0root)/util/fzdiff/fzdiff_precomp.f
$(d0root)/util/fzdiff/fzdiff_stcomp.f:\
  $(d0root)/util/fzdiff/fzdiff_stcomp.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0root)/util/fzdiff/fzdiff_stcomp.for | vmstounix > $(d0root)/util/fzdiff/fzdiff_stcomp.f
$(d0root)/util/fzdiff/fzdiff_stlink.f:\
  $(d0root)/util/fzdiff/fzdiff_stlink.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/util/fzdiff/fzdiff_stlink.for | vmstounix > $(d0root)/util/fzdiff/fzdiff_stlink.f
$(d0root)/util/fzdiff/fzdiff_struc.f:\
  $(d0root)/util/fzdiff/fzdiff_struc.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/util/fzdiff/fzdiff_struc.for | vmstounix > $(d0root)/util/fzdiff/fzdiff_struc.f
$(d0root)/util/fzdiff/fzeventdis.f:\
  $(d0root)/util/fzdiff/fzeventdis.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/util/fzdiff/fzeventdis.for | vmstounix > $(d0root)/util/fzdiff/fzeventdis.f
$(d0root)/util/fzdiff/fzscramble.f:\
  $(d0root)/util/fzdiff/fzscramble.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0root)/util/fzdiff/fzscramble.for | vmstounix > $(d0root)/util/fzdiff/fzscramble.f
$(d0root)/util/fzdiff/fzsort.f:\
  $(d0root)/util/fzdiff/fzsort.for\
  $(d0root)/util/fzdiff/fzdiff_zebcom.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0root)/util/fzdiff/fzsort.for | vmstounix > $(d0root)/util/fzdiff/fzsort.f
$(d0root)/util/fzdiff/fzton.f:\
  $(d0root)/util/fzdiff/fzton.for\
  $(d0root)/util/fzdiff/fzdiff_zebcom.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/links/izjets.link
	tasteofd0 $(FLAVOR) < $(d0root)/util/fzdiff/fzton.for | vmstounix > $(d0root)/util/fzdiff/fzton.f
$(d0root)/util/files/get_str.f:\
  $(d0root)/util/files/get_str.for
	tasteofd0 $(FLAVOR) < $(d0root)/util/files/get_str.for | vmstounix > $(d0root)/util/files/get_str.f
$(d0root)/util/d0flavor/kern_util.f:\
  $(d0root)/util/d0flavor/kern_util.for
	tasteofd0 $(FLAVOR) < $(d0root)/util/d0flavor/kern_util.for | vmstounix > $(d0root)/util/d0flavor/kern_util.f
$(d0root)/util/files/map.f:\
  $(d0root)/util/files/map.for
	tasteofd0 $(FLAVOR) < $(d0root)/util/files/map.for | vmstounix > $(d0root)/util/files/map.f
for:
	touch $(d0root)/util/files/copy_fz.for
	touch $(d0root)/util/d0flavor/d0flavor.for
	touch $(d0root)/util/d0flavor/d0open_text.for
	touch $(d0root)/util/d0flavor/decode_cmd_line.for
	touch $(d0root)/util/files/evnt_list.for
	touch $(d0root)/util/files/fz_inzcom.for
	touch $(d0root)/util/fzdiff/fzdiff.for
	touch $(d0root)/util/fzdiff/fzdiff_bkname.for
	touch $(d0root)/util/fzdiff/fzdiff_comp.for
	touch $(d0root)/util/fzdiff/fzdiff_flat.for
	touch $(d0root)/util/fzdiff/fzdiff_inzcom.for
	touch $(d0root)/util/fzdiff/fzdiff_precomp.for
	touch $(d0root)/util/fzdiff/fzdiff_stcomp.for
	touch $(d0root)/util/fzdiff/fzdiff_stlink.for
	touch $(d0root)/util/fzdiff/fzdiff_struc.for
	touch $(d0root)/util/fzdiff/fzeventdis.for
	touch $(d0root)/util/fzdiff/fzscramble.for
	touch $(d0root)/util/fzdiff/fzsort.for
	touch $(d0root)/util/fzdiff/fzton.for
	touch $(d0root)/util/files/get_str.for
	touch $(d0root)/util/d0flavor/kern_util.for
	touch $(d0root)/util/files/map.for
