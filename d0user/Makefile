SHELL = /bin/sh
CCFLAGS = -c -dollar -G 0 -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0test)/unix/source -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
LCCFLAGS = -c -dollar -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0test)/unix/source -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
F77FLAGS = -c -nocpp -static -col72 -backslash -G 0 -Olimit 2500 -Wo,-loopunroll,2
DEBUG = -O0 -g2
OPT = -O2
ARFLAGS = crsl
SCRATCH = $(d0library)/admin/tmp/d0librar/mmstomake/d0user
FLAVOR = SIUNIX
F77 = f77
.IGNORE:
.SUFFIXES:
debug :\
  deb_frame.a\
  deb_physics.a
deb_frame.a : $(d0root)/test/d0user/deb_frame.a
	@ echo deb_frame.a is up to date
$(d0root)/test/d0user/deb_frame.a:: $(SCRATCH)/frame/nothing.nl
$(d0root)/test/d0user/deb_frame.a::\
  $(d0root)/test/d0user/deb_frame.a(d0user.o)\
  $(d0root)/test/d0user/deb_frame.a(dial_events.o)\
  $(d0root)/test/d0user/deb_frame.a(dishis.o)\
  $(d0root)/test/d0user/deb_frame.a(disusr.o)\
  $(d0root)/test/d0user/deb_frame.a(endjob.o)\
  $(d0root)/test/d0user/deb_frame.a(endrun.o)\
  $(d0root)/test/d0user/deb_frame.a(events.o)\
  $(d0root)/test/d0user/deb_frame.a(examin.o)\
  $(d0root)/test/d0user/deb_frame.a(finish.o)\
  $(d0root)/test/d0user/deb_frame.a(get_dvent376.o)\
  $(d0root)/test/d0user/deb_frame.a(goon.o)\
  $(d0root)/test/d0user/deb_frame.a(hbdial.o)\
  $(d0root)/test/d0user/deb_frame.a(hispak.o)\
  $(d0root)/test/d0user/deb_frame.a(iniflg.o)\
  $(d0root)/test/d0user/deb_frame.a(inigen.o)\
  $(d0root)/test/d0user/deb_frame.a(inigra.o)\
  $(d0root)/test/d0user/deb_frame.a(inijob.o)\
  $(d0root)/test/d0user/deb_frame.a(inimen.o)\
  $(d0root)/test/d0user/deb_frame.a(inirun.o)\
  $(d0root)/test/d0user/deb_frame.a(interr.o)\
  $(d0root)/test/d0user/deb_frame.a(intrpt.o)\
  $(d0root)/test/d0user/deb_frame.a(mkreco.o)\
  $(d0root)/test/d0user/deb_frame.a(mtista.o)\
  $(d0root)/test/d0user/deb_frame.a(mtitle.o)\
  $(d0root)/test/d0user/deb_frame.a(newrun.o)\
  $(d0root)/test/d0user/deb_frame.a(nomore.o)\
  $(d0root)/test/d0user/deb_frame.a(ostrm.o)\
  $(d0root)/test/d0user/deb_frame.a(out_oents655.o)\
  $(d0root)/test/d0user/deb_frame.a(proces.o)\
  $(d0root)/test/d0user/deb_frame.a(ptusev.o)\
  $(d0root)/test/d0user/deb_frame.a(quit.o)\
  $(d0root)/test/d0user/deb_frame.a(runs_summary.o)\
  $(d0root)/test/d0user/deb_frame.a(stdout.o)\
  $(d0root)/test/d0user/deb_frame.a(strhst.o)\
  $(d0root)/test/d0user/deb_frame.a(sumary.o)\
  $(d0root)/test/d0user/deb_frame.a(uflset.o)\
  $(d0root)/test/d0user/deb_frame.a(ureqst.o)\
  $(d0root)/test/d0user/deb_frame.a(usnvrn.o)\
  $(d0root)/test/d0user/deb_frame.a(wrhead.o)\
  $(d0root)/test/d0user/deb_frame.a(wrostr.o)\
  $(d0root)/test/d0user/deb_frame.a(zbinf2.o)\
  $(d0root)/test/d0user/deb_frame.a(zbinpf.o)\
  $(d0root)/test/d0user/deb_frame.a(zboutf.o)\
  $(d0root)/test/d0user/deb_frame.a(zeroev.o)
	ar $(ARFLAGS) $(d0root)/test/d0user/deb_frame.a `ls $(SCRATCH)/frame/a1/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/test/d0user/deb_frame.a `ls $(SCRATCH)/frame/a2/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/test/d0user/deb_frame.a `ls $(SCRATCH)/frame/a3/*.o 2> /dev/null`
	rmdirp $(SCRATCH)/frame
$(SCRATCH)/frame/nothing.nl:
	rmdirp $(SCRATCH)/frame
	mkdirp $(SCRATCH)/frame/a1
	mkdirp $(SCRATCH)/frame/a2
	mkdirp $(SCRATCH)/frame/a3
$(d0root)/test/d0user/deb_frame.a(d0user.o):\
  $(d0library)/d0user/frame/d0user.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/d0user.f ;\
	mv d0user.o $(SCRATCH)/frame/a1/d0user.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(dial_events.o):\
  $(d0library)/d0user/frame/dial_events.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/dial_events.f ;\
	mv dial_events.o $(SCRATCH)/frame/a1/dial_events.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(dishis.o):\
  $(d0library)/d0user/frame/dishis.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/dishis.f ;\
	mv dishis.o $(SCRATCH)/frame/a1/dishis.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(disusr.o):\
  $(d0library)/d0user/frame/disusr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/disusr.f ;\
	mv disusr.o $(SCRATCH)/frame/a1/disusr.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(endjob.o):\
  $(d0library)/d0user/frame/endjob.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/endjob.f ;\
	mv endjob.o $(SCRATCH)/frame/a1/endjob.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(endrun.o):\
  $(d0library)/d0user/frame/endrun.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/endrun.f ;\
	mv endrun.o $(SCRATCH)/frame/a1/endrun.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(events.o):\
  $(d0library)/d0user/frame/events.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/events.f ;\
	mv events.o $(SCRATCH)/frame/a1/events.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(examin.o):\
  $(d0library)/d0user/frame/examin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/examin.f ;\
	mv examin.o $(SCRATCH)/frame/a1/examin.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(finish.o):\
  $(d0library)/d0user/frame/finish.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/finish.f ;\
	mv finish.o $(SCRATCH)/frame/a1/finish.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(get_dvent376.o):\
  $(d0library)/d0user/frame/get_daq_event.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/get_daq_event.f ;\
	mv get_daq_event.o $(SCRATCH)/frame/a1/get_dvent376.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(goon.o):\
  $(d0library)/d0user/frame/goon.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/goon.f ;\
	mv goon.o $(SCRATCH)/frame/a1/goon.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(hbdial.o):\
  $(d0library)/d0user/frame/hbdial.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/hbdial.f ;\
	mv hbdial.o $(SCRATCH)/frame/a1/hbdial.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(hispak.o):\
  $(d0library)/d0user/frame/hispak.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/hispak.f ;\
	mv hispak.o $(SCRATCH)/frame/a1/hispak.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(iniflg.o):\
  $(d0library)/d0user/frame/iniflg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/iniflg.f ;\
	mv iniflg.o $(SCRATCH)/frame/a1/iniflg.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(inigen.o):\
  $(d0library)/d0user/frame/inigen.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/inigen.f ;\
	mv inigen.o $(SCRATCH)/frame/a1/inigen.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(inigra.o):\
  $(d0library)/d0user/frame/inigra.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/inigra.f ;\
	mv inigra.o $(SCRATCH)/frame/a1/inigra.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(inijob.o):\
  $(d0library)/d0user/frame/inijob.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/inijob.f ;\
	mv inijob.o $(SCRATCH)/frame/a1/inijob.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(inimen.o):\
  $(d0library)/d0user/frame/inimen.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/inimen.f ;\
	mv inimen.o $(SCRATCH)/frame/a1/inimen.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(inirun.o):\
  $(d0library)/d0user/frame/inirun.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/inirun.f ;\
	mv inirun.o $(SCRATCH)/frame/a1/inirun.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(interr.o):\
  $(d0library)/d0user/frame/interr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/interr.f ;\
	mv interr.o $(SCRATCH)/frame/a1/interr.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(intrpt.o):\
  $(d0library)/d0user/frame/intrpt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/intrpt.f ;\
	mv intrpt.o $(SCRATCH)/frame/a2/intrpt.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(mkreco.o):\
  $(d0library)/d0user/frame/mkreco.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/mkreco.f ;\
	mv mkreco.o $(SCRATCH)/frame/a2/mkreco.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(mtista.o):\
  $(d0library)/d0user/frame/mtista.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/mtista.f ;\
	mv mtista.o $(SCRATCH)/frame/a2/mtista.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(mtitle.o):\
  $(d0library)/d0user/frame/mtitle.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/mtitle.f ;\
	mv mtitle.o $(SCRATCH)/frame/a2/mtitle.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(newrun.o):\
  $(d0library)/d0user/frame/newrun.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/newrun.f ;\
	mv newrun.o $(SCRATCH)/frame/a2/newrun.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(nomore.o):\
  $(d0library)/d0user/frame/nomore.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/nomore.f ;\
	mv nomore.o $(SCRATCH)/frame/a2/nomore.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(ostrm.o):\
  $(d0library)/d0user/frame/ostrm.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/ostrm.f ;\
	mv ostrm.o $(SCRATCH)/frame/a2/ostrm.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(out_oents655.o):\
  $(d0library)/d0user/frame/out_only_events.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/out_only_events.f ;\
	mv out_only_events.o $(SCRATCH)/frame/a2/out_oents655.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(proces.o):\
  $(d0library)/d0user/frame/proces.f\
  $(d0library)/inc/fatcom.inc\
  $(d0library)/params/fatpara.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/proces.f ;\
	mv proces.o $(SCRATCH)/frame/a2/proces.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(ptusev.o):\
  $(d0library)/d0user/frame/ptusev.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/ptusev.f ;\
	mv ptusev.o $(SCRATCH)/frame/a2/ptusev.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(quit.o):\
  $(d0library)/d0user/frame/quit.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/quit.f ;\
	mv quit.o $(SCRATCH)/frame/a2/quit.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(runs_summary.o):\
  $(d0library)/d0user/frame/runs_summary.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/runs_summary.f ;\
	mv runs_summary.o $(SCRATCH)/frame/a2/runs_summary.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(stdout.o):\
  $(d0library)/d0user/frame/stdout.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/stdout.f ;\
	mv stdout.o $(SCRATCH)/frame/a2/stdout.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(strhst.o):\
  $(d0library)/d0user/frame/strhst.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/strhst.f ;\
	mv strhst.o $(SCRATCH)/frame/a2/strhst.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(sumary.o):\
  $(d0library)/d0user/frame/sumary.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/sumary.f ;\
	mv sumary.o $(SCRATCH)/frame/a2/sumary.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(uflset.o):\
  $(d0library)/d0user/frame/uflset.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/uflset.f ;\
	mv uflset.o $(SCRATCH)/frame/a2/uflset.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(ureqst.o):\
  $(d0library)/d0user/frame/ureqst.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/ureqst.f ;\
	mv ureqst.o $(SCRATCH)/frame/a2/ureqst.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(usnvrn.o):\
  $(d0library)/d0user/frame/usnvrn.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/usnvrn.f ;\
	mv usnvrn.o $(SCRATCH)/frame/a2/usnvrn.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(wrhead.o):\
  $(d0library)/d0user/frame/wrhead.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/wrhead.f ;\
	mv wrhead.o $(SCRATCH)/frame/a2/wrhead.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(wrostr.o):\
  $(d0library)/d0user/frame/wrostr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/wrostr.f ;\
	mv wrostr.o $(SCRATCH)/frame/a2/wrostr.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(zbinf2.o):\
  $(d0library)/d0user/frame/zbinf2.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/zbinf2.f ;\
	mv zbinf2.o $(SCRATCH)/frame/a3/zbinf2.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(zbinpf.o):\
  $(d0library)/d0user/frame/zbinpf.f\
  $(d0library)/inc/fatcom.inc\
  $(d0library)/params/fatpara.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/zbinpf.f ;\
	mv zbinpf.o $(SCRATCH)/frame/a3/zbinpf.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(zboutf.o):\
  $(d0library)/d0user/frame/zboutf.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/zboutf.f ;\
	mv zboutf.o $(SCRATCH)/frame/a3/zboutf.o ;\
	)
$(d0root)/test/d0user/deb_frame.a(zeroev.o):\
  $(d0library)/d0user/frame/zeroev.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/frame/zeroev.f ;\
	mv zeroev.o $(SCRATCH)/frame/a3/zeroev.o ;\
	)
deb_physics.a : $(d0root)/test/d0user/deb_physics.a
	@ echo deb_physics.a is up to date
$(d0root)/test/d0user/deb_physics.a:: $(SCRATCH)/physics/nothing.nl
$(d0root)/test/d0user/deb_physics.a::\
  $(d0root)/test/d0user/deb_physics.a(all_d_ddf573.o)\
  $(d0root)/test/d0user/deb_physics.a(all_d_dmp592.o)\
  $(d0root)/test/d0user/deb_physics.a(all_d_evt606.o)\
  $(d0root)/test/d0user/deb_physics.a(all_d_evz612.o)\
  $(d0root)/test/d0user/deb_physics.a(all_d_ini591.o)\
  $(d0root)/test/d0user/deb_physics.a(all_d_par594.o)\
  $(d0root)/test/d0user/deb_physics.a(cadefd.o)\
  $(d0root)/test/d0user/deb_physics.a(canali.o)\
  $(d0root)/test/d0user/deb_physics.a(canini.o)\
  $(d0root)/test/d0user/deb_physics.a(dst_analysis.o)\
  $(d0root)/test/d0user/deb_physics.a(dst_defd.o)\
  $(d0root)/test/d0user/deb_physics.a(dst_dump.o)\
  $(d0root)/test/d0user/deb_physics.a(mcdata.o)
	ar $(ARFLAGS) $(d0root)/test/d0user/deb_physics.a `ls $(SCRATCH)/physics/a1/*.o 2> /dev/null`
	rmdirp $(SCRATCH)/physics
$(SCRATCH)/physics/nothing.nl:
	rmdirp $(SCRATCH)/physics
	mkdirp $(SCRATCH)/physics/a1
$(d0root)/test/d0user/deb_physics.a(all_d_ddf573.o):\
  $(d0library)/d0user/physics/all_display_ddf.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/physics/all_display_ddf.f ;\
	mv all_display_ddf.o $(SCRATCH)/physics/a1/all_d_ddf573.o ;\
	)
$(d0root)/test/d0user/deb_physics.a(all_d_dmp592.o):\
  $(d0library)/d0user/physics/all_display_dmp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/physics/all_display_dmp.f ;\
	mv all_display_dmp.o $(SCRATCH)/physics/a1/all_d_dmp592.o ;\
	)
$(d0root)/test/d0user/deb_physics.a(all_d_evt606.o):\
  $(d0library)/d0user/physics/all_display_evt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/physics/all_display_evt.f ;\
	mv all_display_evt.o $(SCRATCH)/physics/a1/all_d_evt606.o ;\
	)
$(d0root)/test/d0user/deb_physics.a(all_d_evz612.o):\
  $(d0library)/d0user/physics/all_display_evz.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/physics/all_display_evz.f ;\
	mv all_display_evz.o $(SCRATCH)/physics/a1/all_d_evz612.o ;\
	)
$(d0root)/test/d0user/deb_physics.a(all_d_ini591.o):\
  $(d0library)/d0user/physics/all_display_ini.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/physics/all_display_ini.f ;\
	mv all_display_ini.o $(SCRATCH)/physics/a1/all_d_ini591.o ;\
	)
$(d0root)/test/d0user/deb_physics.a(all_d_par594.o):\
  $(d0library)/d0user/physics/all_display_par.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/physics/all_display_par.f ;\
	mv all_display_par.o $(SCRATCH)/physics/a1/all_d_par594.o ;\
	)
$(d0root)/test/d0user/deb_physics.a(cadefd.o):\
  $(d0library)/d0user/physics/cadefd.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/physics/cadefd.f ;\
	mv cadefd.o $(SCRATCH)/physics/a1/cadefd.o ;\
	)
$(d0root)/test/d0user/deb_physics.a(canali.o):\
  $(d0library)/d0user/physics/canali.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/physics/canali.f ;\
	mv canali.o $(SCRATCH)/physics/a1/canali.o ;\
	)
$(d0root)/test/d0user/deb_physics.a(canini.o):\
  $(d0library)/d0user/physics/canini.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/physics/canini.f ;\
	mv canini.o $(SCRATCH)/physics/a1/canini.o ;\
	)
$(d0root)/test/d0user/deb_physics.a(dst_analysis.o):\
  $(d0library)/d0user/physics/dst_analysis.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/physics/dst_analysis.f ;\
	mv dst_analysis.o $(SCRATCH)/physics/a1/dst_analysis.o ;\
	)
$(d0root)/test/d0user/deb_physics.a(dst_defd.o):\
  $(d0library)/d0user/physics/dst_defd.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/physics/dst_defd.f ;\
	mv dst_defd.o $(SCRATCH)/physics/a1/dst_defd.o ;\
	)
$(d0root)/test/d0user/deb_physics.a(dst_dump.o):\
  $(d0library)/d0user/physics/dst_dump.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/physics/dst_dump.f ;\
	mv dst_dump.o $(SCRATCH)/physics/a1/dst_dump.o ;\
	)
$(d0root)/test/d0user/deb_physics.a(mcdata.o):\
  $(d0library)/d0user/physics/mcdata.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izgean.link\
  $(d0library)/links/izisae.link\
  $(d0library)/links/izfake.link\
  $(d0library)/links/izreco.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/d0user/physics/mcdata.f ;\
	mv mcdata.o $(SCRATCH)/physics/a1/mcdata.o ;\
	)
pre:\
  $(d0library)/d0user/physics/all_display_ddf.f\
  $(d0library)/d0user/physics/all_display_dmp.f\
  $(d0library)/d0user/physics/all_display_evt.f\
  $(d0library)/d0user/physics/all_display_evz.f\
  $(d0library)/d0user/physics/all_display_ini.f\
  $(d0library)/d0user/physics/all_display_par.f\
  $(d0library)/d0user/physics/cadefd.f\
  $(d0library)/d0user/physics/canali.f\
  $(d0library)/d0user/physics/canini.f\
  $(d0library)/d0user/frame/d0user.f\
  $(d0library)/d0user/frame/dial_events.f\
  $(d0library)/d0user/frame/dishis.f\
  $(d0library)/d0user/frame/disusr.f\
  $(d0library)/d0user/physics/dst_analysis.f\
  $(d0library)/d0user/physics/dst_defd.f\
  $(d0library)/d0user/physics/dst_dump.f\
  $(d0library)/d0user/frame/endjob.f\
  $(d0library)/d0user/frame/endrun.f\
  $(d0library)/d0user/frame/events.f\
  $(d0library)/d0user/frame/examin.f\
  $(d0library)/d0user/frame/finish.f\
  $(d0library)/d0user/frame/get_daq_event.f\
  $(d0library)/d0user/frame/goon.f\
  $(d0library)/d0user/frame/hbdial.f\
  $(d0library)/d0user/frame/hispak.f\
  $(d0library)/d0user/frame/iniflg.f\
  $(d0library)/d0user/frame/inigen.f\
  $(d0library)/d0user/frame/inigra.f\
  $(d0library)/d0user/frame/inijob.f\
  $(d0library)/d0user/frame/inimen.f\
  $(d0library)/d0user/frame/inirun.f\
  $(d0library)/d0user/frame/interr.f\
  $(d0library)/d0user/frame/intrpt.f\
  $(d0library)/d0user/physics/mcdata.f\
  $(d0library)/d0user/frame/mkreco.f\
  $(d0library)/d0user/frame/mtista.f\
  $(d0library)/d0user/frame/mtitle.f\
  $(d0library)/d0user/frame/newrun.f\
  $(d0library)/d0user/frame/nomore.f\
  $(d0library)/d0user/frame/ostrm.f\
  $(d0library)/d0user/frame/out_only_events.f\
  $(d0library)/d0user/frame/proces.f\
  $(d0library)/d0user/frame/ptusev.f\
  $(d0library)/d0user/frame/quit.f\
  $(d0library)/d0user/frame/runs_summary.f\
  $(d0library)/d0user/frame/stdout.f\
  $(d0library)/d0user/frame/strhst.f\
  $(d0library)/d0user/frame/sumary.f\
  $(d0library)/d0user/frame/uflset.f\
  $(d0library)/d0user/frame/ureqst.f\
  $(d0library)/d0user/frame/usnvrn.f\
  $(d0library)/d0user/frame/wrhead.f\
  $(d0library)/d0user/frame/wrostr.f\
  $(d0library)/d0user/frame/zbinf2.f\
  $(d0library)/d0user/frame/zbinpf.f\
  $(d0library)/d0user/frame/zboutf.f\
  $(d0library)/d0user/frame/zeroev.f
$(d0library)/d0user/physics/all_display_ddf.f:\
  $(d0library)/d0user/physics/all_display_ddf.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/physics/all_display_ddf.for | vmstounix > $(d0library)/d0user/physics/all_display_ddf.f
$(d0library)/d0user/physics/all_display_dmp.f:\
  $(d0library)/d0user/physics/all_display_dmp.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/physics/all_display_dmp.for | vmstounix > $(d0library)/d0user/physics/all_display_dmp.f
$(d0library)/d0user/physics/all_display_evt.f:\
  $(d0library)/d0user/physics/all_display_evt.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/physics/all_display_evt.for | vmstounix > $(d0library)/d0user/physics/all_display_evt.f
$(d0library)/d0user/physics/all_display_evz.f:\
  $(d0library)/d0user/physics/all_display_evz.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/physics/all_display_evz.for | vmstounix > $(d0library)/d0user/physics/all_display_evz.f
$(d0library)/d0user/physics/all_display_ini.f:\
  $(d0library)/d0user/physics/all_display_ini.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/physics/all_display_ini.for | vmstounix > $(d0library)/d0user/physics/all_display_ini.f
$(d0library)/d0user/physics/all_display_par.f:\
  $(d0library)/d0user/physics/all_display_par.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/physics/all_display_par.for | vmstounix > $(d0library)/d0user/physics/all_display_par.f
$(d0library)/d0user/physics/cadefd.f:\
  $(d0library)/d0user/physics/cadefd.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/physics/cadefd.for | vmstounix > $(d0library)/d0user/physics/cadefd.f
$(d0library)/d0user/physics/canali.f:\
  $(d0library)/d0user/physics/canali.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/physics/canali.for | vmstounix > $(d0library)/d0user/physics/canali.f
$(d0library)/d0user/physics/canini.f:\
  $(d0library)/d0user/physics/canini.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/physics/canini.for | vmstounix > $(d0library)/d0user/physics/canini.f
$(d0library)/d0user/frame/d0user.f:\
  $(d0library)/d0user/frame/d0user.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/d0user.for | vmstounix > $(d0library)/d0user/frame/d0user.f
$(d0library)/d0user/frame/dial_events.f:\
  $(d0library)/d0user/frame/dial_events.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/dial_events.for | vmstounix > $(d0library)/d0user/frame/dial_events.f
$(d0library)/d0user/frame/dishis.f:\
  $(d0library)/d0user/frame/dishis.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/dishis.for | vmstounix > $(d0library)/d0user/frame/dishis.f
$(d0library)/d0user/frame/disusr.f:\
  $(d0library)/d0user/frame/disusr.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/disusr.for | vmstounix > $(d0library)/d0user/frame/disusr.f
$(d0library)/d0user/physics/dst_analysis.f:\
  $(d0library)/d0user/physics/dst_analysis.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/physics/dst_analysis.for | vmstounix > $(d0library)/d0user/physics/dst_analysis.f
$(d0library)/d0user/physics/dst_defd.f:\
  $(d0library)/d0user/physics/dst_defd.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/physics/dst_defd.for | vmstounix > $(d0library)/d0user/physics/dst_defd.f
$(d0library)/d0user/physics/dst_dump.f:\
  $(d0library)/d0user/physics/dst_dump.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/physics/dst_dump.for | vmstounix > $(d0library)/d0user/physics/dst_dump.f
$(d0library)/d0user/frame/endjob.f:\
  $(d0library)/d0user/frame/endjob.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/endjob.for | vmstounix > $(d0library)/d0user/frame/endjob.f
$(d0library)/d0user/frame/endrun.f:\
  $(d0library)/d0user/frame/endrun.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/endrun.for | vmstounix > $(d0library)/d0user/frame/endrun.f
$(d0library)/d0user/frame/events.f:\
  $(d0library)/d0user/frame/events.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/events.for | vmstounix > $(d0library)/d0user/frame/events.f
$(d0library)/d0user/frame/examin.f:\
  $(d0library)/d0user/frame/examin.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/examin.for | vmstounix > $(d0library)/d0user/frame/examin.f
$(d0library)/d0user/frame/finish.f:\
  $(d0library)/d0user/frame/finish.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/finish.for | vmstounix > $(d0library)/d0user/frame/finish.f
$(d0library)/d0user/frame/get_daq_event.f:\
  $(d0library)/d0user/frame/get_daq_event.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/get_daq_event.for | vmstounix > $(d0library)/d0user/frame/get_daq_event.f
$(d0library)/d0user/frame/goon.f:\
  $(d0library)/d0user/frame/goon.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/goon.for | vmstounix > $(d0library)/d0user/frame/goon.f
$(d0library)/d0user/frame/hbdial.f:\
  $(d0library)/d0user/frame/hbdial.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/hbdial.for | vmstounix > $(d0library)/d0user/frame/hbdial.f
$(d0library)/d0user/frame/hispak.f:\
  $(d0library)/d0user/frame/hispak.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/hispak.for | vmstounix > $(d0library)/d0user/frame/hispak.f
$(d0library)/d0user/frame/iniflg.f:\
  $(d0library)/d0user/frame/iniflg.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/iniflg.for | vmstounix > $(d0library)/d0user/frame/iniflg.f
$(d0library)/d0user/frame/inigen.f:\
  $(d0library)/d0user/frame/inigen.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/inigen.for | vmstounix > $(d0library)/d0user/frame/inigen.f
$(d0library)/d0user/frame/inigra.f:\
  $(d0library)/d0user/frame/inigra.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/inigra.for | vmstounix > $(d0library)/d0user/frame/inigra.f
$(d0library)/d0user/frame/inijob.f:\
  $(d0library)/d0user/frame/inijob.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/inijob.for | vmstounix > $(d0library)/d0user/frame/inijob.f
$(d0library)/d0user/frame/inimen.f:\
  $(d0library)/d0user/frame/inimen.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/inimen.for | vmstounix > $(d0library)/d0user/frame/inimen.f
$(d0library)/d0user/frame/inirun.f:\
  $(d0library)/d0user/frame/inirun.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/inirun.for | vmstounix > $(d0library)/d0user/frame/inirun.f
$(d0library)/d0user/frame/interr.f:\
  $(d0library)/d0user/frame/interr.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/interr.for | vmstounix > $(d0library)/d0user/frame/interr.f
$(d0library)/d0user/frame/intrpt.f:\
  $(d0library)/d0user/frame/intrpt.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/intrpt.for | vmstounix > $(d0library)/d0user/frame/intrpt.f
$(d0library)/d0user/physics/mcdata.f:\
  $(d0library)/d0user/physics/mcdata.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izgean.link\
  $(d0library)/links/izisae.link\
  $(d0library)/links/izfake.link\
  $(d0library)/links/izreco.link
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/physics/mcdata.for | vmstounix > $(d0library)/d0user/physics/mcdata.f
$(d0library)/d0user/frame/mkreco.f:\
  $(d0library)/d0user/frame/mkreco.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/mkreco.for | vmstounix > $(d0library)/d0user/frame/mkreco.f
$(d0library)/d0user/frame/mtista.f:\
  $(d0library)/d0user/frame/mtista.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/mtista.for | vmstounix > $(d0library)/d0user/frame/mtista.f
$(d0library)/d0user/frame/mtitle.f:\
  $(d0library)/d0user/frame/mtitle.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/mtitle.for | vmstounix > $(d0library)/d0user/frame/mtitle.f
$(d0library)/d0user/frame/newrun.f:\
  $(d0library)/d0user/frame/newrun.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/newrun.for | vmstounix > $(d0library)/d0user/frame/newrun.f
$(d0library)/d0user/frame/nomore.f:\
  $(d0library)/d0user/frame/nomore.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/nomore.for | vmstounix > $(d0library)/d0user/frame/nomore.f
$(d0library)/d0user/frame/ostrm.f:\
  $(d0library)/d0user/frame/ostrm.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/ostrm.for | vmstounix > $(d0library)/d0user/frame/ostrm.f
$(d0library)/d0user/frame/out_only_events.f:\
  $(d0library)/d0user/frame/out_only_events.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/out_only_events.for | vmstounix > $(d0library)/d0user/frame/out_only_events.f
$(d0library)/d0user/frame/proces.f:\
  $(d0library)/d0user/frame/proces.for\
  $(d0library)/inc/fatcom.inc\
  $(d0library)/params/fatpara.def
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/proces.for | vmstounix > $(d0library)/d0user/frame/proces.f
$(d0library)/d0user/frame/ptusev.f:\
  $(d0library)/d0user/frame/ptusev.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/ptusev.for | vmstounix > $(d0library)/d0user/frame/ptusev.f
$(d0library)/d0user/frame/quit.f:\
  $(d0library)/d0user/frame/quit.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/quit.for | vmstounix > $(d0library)/d0user/frame/quit.f
$(d0library)/d0user/frame/runs_summary.f:\
  $(d0library)/d0user/frame/runs_summary.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/runs_summary.for | vmstounix > $(d0library)/d0user/frame/runs_summary.f
$(d0library)/d0user/frame/stdout.f:\
  $(d0library)/d0user/frame/stdout.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/stdout.for | vmstounix > $(d0library)/d0user/frame/stdout.f
$(d0library)/d0user/frame/strhst.f:\
  $(d0library)/d0user/frame/strhst.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/strhst.for | vmstounix > $(d0library)/d0user/frame/strhst.f
$(d0library)/d0user/frame/sumary.f:\
  $(d0library)/d0user/frame/sumary.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/sumary.for | vmstounix > $(d0library)/d0user/frame/sumary.f
$(d0library)/d0user/frame/uflset.f:\
  $(d0library)/d0user/frame/uflset.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/uflset.for | vmstounix > $(d0library)/d0user/frame/uflset.f
$(d0library)/d0user/frame/ureqst.f:\
  $(d0library)/d0user/frame/ureqst.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/ureqst.for | vmstounix > $(d0library)/d0user/frame/ureqst.f
$(d0library)/d0user/frame/usnvrn.f:\
  $(d0library)/d0user/frame/usnvrn.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/usnvrn.for | vmstounix > $(d0library)/d0user/frame/usnvrn.f
$(d0library)/d0user/frame/wrhead.f:\
  $(d0library)/d0user/frame/wrhead.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/wrhead.for | vmstounix > $(d0library)/d0user/frame/wrhead.f
$(d0library)/d0user/frame/wrostr.f:\
  $(d0library)/d0user/frame/wrostr.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/wrostr.for | vmstounix > $(d0library)/d0user/frame/wrostr.f
$(d0library)/d0user/frame/zbinf2.f:\
  $(d0library)/d0user/frame/zbinf2.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/zbinf2.for | vmstounix > $(d0library)/d0user/frame/zbinf2.f
$(d0library)/d0user/frame/zbinpf.f:\
  $(d0library)/d0user/frame/zbinpf.for\
  $(d0library)/inc/fatcom.inc\
  $(d0library)/params/fatpara.def
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/zbinpf.for | vmstounix > $(d0library)/d0user/frame/zbinpf.f
$(d0library)/d0user/frame/zboutf.f:\
  $(d0library)/d0user/frame/zboutf.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/zboutf.for | vmstounix > $(d0library)/d0user/frame/zboutf.f
$(d0library)/d0user/frame/zeroev.f:\
  $(d0library)/d0user/frame/zeroev.for
	tasteofd0 $(FLAVOR) < $(d0library)/d0user/frame/zeroev.for | vmstounix > $(d0library)/d0user/frame/zeroev.f
for:
	touch $(d0library)/d0user/physics/all_display_ddf.for
	touch $(d0library)/d0user/physics/all_display_dmp.for
	touch $(d0library)/d0user/physics/all_display_evt.for
	touch $(d0library)/d0user/physics/all_display_evz.for
	touch $(d0library)/d0user/physics/all_display_ini.for
	touch $(d0library)/d0user/physics/all_display_par.for
	touch $(d0library)/d0user/physics/cadefd.for
	touch $(d0library)/d0user/physics/canali.for
	touch $(d0library)/d0user/physics/canini.for
	touch $(d0library)/d0user/frame/d0user.for
	touch $(d0library)/d0user/frame/dial_events.for
	touch $(d0library)/d0user/frame/dishis.for
	touch $(d0library)/d0user/frame/disusr.for
	touch $(d0library)/d0user/physics/dst_analysis.for
	touch $(d0library)/d0user/physics/dst_defd.for
	touch $(d0library)/d0user/physics/dst_dump.for
	touch $(d0library)/d0user/frame/endjob.for
	touch $(d0library)/d0user/frame/endrun.for
	touch $(d0library)/d0user/frame/events.for
	touch $(d0library)/d0user/frame/examin.for
	touch $(d0library)/d0user/frame/finish.for
	touch $(d0library)/d0user/frame/get_daq_event.for
	touch $(d0library)/d0user/frame/goon.for
	touch $(d0library)/d0user/frame/hbdial.for
	touch $(d0library)/d0user/frame/hispak.for
	touch $(d0library)/d0user/frame/iniflg.for
	touch $(d0library)/d0user/frame/inigen.for
	touch $(d0library)/d0user/frame/inigra.for
	touch $(d0library)/d0user/frame/inijob.for
	touch $(d0library)/d0user/frame/inimen.for
	touch $(d0library)/d0user/frame/inirun.for
	touch $(d0library)/d0user/frame/interr.for
	touch $(d0library)/d0user/frame/intrpt.for
	touch $(d0library)/d0user/physics/mcdata.for
	touch $(d0library)/d0user/frame/mkreco.for
	touch $(d0library)/d0user/frame/mtista.for
	touch $(d0library)/d0user/frame/mtitle.for
	touch $(d0library)/d0user/frame/newrun.for
	touch $(d0library)/d0user/frame/nomore.for
	touch $(d0library)/d0user/frame/ostrm.for
	touch $(d0library)/d0user/frame/out_only_events.for
	touch $(d0library)/d0user/frame/proces.for
	touch $(d0library)/d0user/frame/ptusev.for
	touch $(d0library)/d0user/frame/quit.for
	touch $(d0library)/d0user/frame/runs_summary.for
	touch $(d0library)/d0user/frame/stdout.for
	touch $(d0library)/d0user/frame/strhst.for
	touch $(d0library)/d0user/frame/sumary.for
	touch $(d0library)/d0user/frame/uflset.for
	touch $(d0library)/d0user/frame/ureqst.for
	touch $(d0library)/d0user/frame/usnvrn.for
	touch $(d0library)/d0user/frame/wrhead.for
	touch $(d0library)/d0user/frame/wrostr.for
	touch $(d0library)/d0user/frame/zbinf2.for
	touch $(d0library)/d0user/frame/zbinpf.for
	touch $(d0library)/d0user/frame/zboutf.for
	touch $(d0library)/d0user/frame/zeroev.for
