SHELL = /bin/sh
CCFLAGS = -c -o32 -dollar -G 0 -DD0FLAVOR=SIUNIX -woff 1172 -Wl,-LD_MSG:off=84 -I$(d0root)/test/unix/source -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
LCCFLAGS = -c -o32 -dollar -DD0FLAVOR=SIUNIX -woff 1167,1172,1116,1110 -Wl,-LD_MSG:off=84 -I$(d0root)/test/unix/source -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
F77FLAGS = -c -o32 -nocpp -static -col72 -backslash -G 0 -Wl,-LD_MSG:off=84
DEBUG = -O0 -g2
OPT = -O2
ARFLAGS = crsl
SCRATCH = $(d0library)/tmp/d0librar/userlib/1214848/unix
FLAVOR = SIUNIX
CC = cc
LEX = lex
F77 = f77
.IGNORE:
.SUFFIXES:
opt :\
  unix.a
unix.a : $(d0root)/test/unix/unix.a
	@ echo unix.a is up to date
$(d0root)/test/unix/unix.a:: $(SCRATCH)/nothing.nl
$(d0root)/test/unix/unix.a::\
  $(d0root)/test/unix/unix.a(avoid_nfs.o)\
  $(d0root)/test/unix/unix.a(c_depend.o)\
  $(d0root)/test/unix/unix.a(cfget.o)\
  $(d0root)/test/unix/unix.a(check_exec.o)\
  $(d0root)/test/unix/unix.a(cstring.o)\
  $(d0root)/test/unix/unix.a(d0_loc.o)\
  $(d0root)/test/unix/unix.a(d0_nodename.o)\
  $(d0root)/test/unix/unix.a(d0_readlink.o)\
  $(d0root)/test/unix/unix.a(d0_tz_offset.o)\
  $(d0root)/test/unix/unix.a(d0echo.o)\
  $(d0root)/test/unix/unix.a(d0rexec.o)\
  $(d0root)/test/unix/unix.a(d0rl.o)\
  $(d0root)/test/unix/unix.a(date.o)\
  $(d0root)/test/unix/unix.a(daylifset132.o)\
  $(d0root)/test/unix/unix.a(exit.o)\
  $(d0root)/test/unix/unix.a(f_depend.o)\
  $(d0root)/test/unix/unix.a(fa.o)\
  $(d0root)/test/unix/unix.a(fatmepand380.o)\
  $(d0root)/test/unix/unix.a(fatme_dir794.o)\
  $(d0root)/test/unix/unix.a(fatmen_find.o)\
  $(d0root)/test/unix/unix.a(ff.o)\
  $(d0root)/test/unix/unix.a(find_file.o)\
  $(d0root)/test/unix/unix.a(fl.o)\
  $(d0root)/test/unix/unix.a(flush_std.o)\
  $(d0root)/test/unix/unix.a(fn.o)\
  $(d0root)/test/unix/unix.a(for_depend.o)\
  $(d0root)/test/unix/unix.a(fstring.o)\
  $(d0root)/test/unix/unix.a(getpassnetrc.o)\
  $(d0root)/test/unix/unix.a(getusernetrc.o)\
  $(d0root)/test/unix/unix.a(getword.o)\
  $(d0root)/test/unix/unix.a(glob.o)\
  $(d0root)/test/unix/unix.a(idate.o)\
  $(d0root)/test/unix/unix.a(jpi_cputim.o)\
  $(d0root)/test/unix/unix.a(lib_dtime356.o)\
  $(d0root)/test/unix/unix.a(lib_day.o)\
  $(d0root)/test/unix/unix.a(lib_dfile554.o)\
  $(d0root)/test/unix/unix.a(lib_ffile344.o)\
  $(d0root)/test/unix/unix.a(lib_free_vm.o)\
  $(d0root)/test/unix/unix.a(lib_geign577.o)\
  $(d0root)/test/unix/unix.a(lib_get_vm.o)\
  $(d0root)/test/unix/unix.a(lib_int_over.o)\
  $(d0root)/test/unix/unix.a(lib_rfile559.o)\
  $(d0root)/test/unix/unix.a(lib_signal.o)\
  $(d0root)/test/unix/unix.a(lib_stop.o)\
  $(d0root)/test/unix/unix.a(lib_wait.o)\
  $(d0root)/test/unix/unix.a(make__rcp701.o)\
  $(d0root)/test/unix/unix.a(segv_fault.o)\
  $(d0root)/test/unix/unix.a(smg_bdate843.o)\
  $(d0root)/test/unix/unix.a(smg_bell.o)\
  $(d0root)/test/unix/unix.a(smg_cport037.o)\
  $(d0root)/test/unix/unix.a(smg_cplay769.o)\
  $(d0root)/test/unix/unix.a(smg_cplay598.o)\
  $(d0root)/test/unix/unix.a(smg_coard216.o)\
  $(d0root)/test/unix/unix.a(smg_cport051.o)\
  $(d0root)/test/unix/unix.a(smg_cplay783.o)\
  $(d0root)/test/unix/unix.a(smg_coard874.o)\
  $(d0root)/test/unix/unix.a(smg_clumn851.o)\
  $(d0root)/test/unix/unix.a(smg_c_row541.o)\
  $(d0root)/test/unix/unix.a(smg_doard215.o)\
  $(d0root)/test/unix/unix.a(smg_dport050.o)\
  $(d0root)/test/unix/unix.a(smg_dplay782.o)\
  $(d0root)/test/unix/unix.a(smg_doard873.o)\
  $(d0root)/test/unix/unix.a(smg_dnput093.o)\
  $(d0root)/test/unix/unix.a(smg_enput984.o)\
  $(d0root)/test/unix/unix.a(smg_edate637.o)\
  $(d0root)/test/unix/unix.a(smg_eplay813.o)\
  $(d0root)/test/unix/unix.a(smg_eline479.o)\
  $(d0root)/test/unix/unix.a(smg_eoard116.o)\
  $(d0root)/test/unix/unix.a(smg_error.o)\
  $(d0root)/test/unix/unix.a(smg_gattr143.o)\
  $(d0root)/test/unix/unix.a(smg_ginfo128.o)\
  $(d0root)/test/unix/unix.a(smg_gchar252.o)\
  $(d0root)/test/unix/unix.a(smg_getch.o)\
  $(d0root)/test/unix/unix.a(smg_getstr.o)\
  $(d0root)/test/unix/unix.a(smg_lrder677.o)\
  $(d0root)/test/unix/unix.a(smg_mtext419.o)\
  $(d0root)/test/unix/unix.a(smg_mplay594.o)\
  $(d0root)/test/unix/unix.a(smg_pplay826.o)\
  $(d0root)/test/unix/unix.a(smg_pplay696.o)\
  $(d0root)/test/unix/unix.a(smg_phars401.o)\
  $(d0root)/test/unix/unix.a(smg_pwide921.o)\
  $(d0root)/test/unix/unix.a(smg_put_line.o)\
  $(d0root)/test/unix/unix.a(smg_proll066.o)\
  $(d0root)/test/unix/unix.a(smg_rplay228.o)\
  $(d0root)/test/unix/unix.a(smg_rroke932.o)\
  $(d0root)/test/unix/unix.a(smg_rring602.o)\
  $(d0root)/test/unix/unix.a(smg_rtion404.o)\
  $(d0root)/test/unix/unix.a(smg_rreen922.o)\
  $(d0root)/test/unix/unix.a(smg_rplay911.o)\
  $(d0root)/test/unix/unix.a(smg_rterm514.o)\
  $(d0root)/test/unix/unix.a(smg_r_pos302.o)\
  $(d0root)/test/unix/unix.a(smg_scroll.o)\
  $(d0root)/test/unix/unix.a(smg_s_abs934.o)\
  $(d0root)/test/unix/unix.a(smg_smode045.o)\
  $(d0root)/test/unix/unix.a(smg_sgion106.o)\
  $(d0root)/test/unix/unix.a(smg_uplay923.o)\
  $(d0root)/test/unix/unix.a(smg_update.o)\
  $(d0root)/test/unix/unix.a(smg_vplay946.o)\
  $(d0root)/test/unix/unix.a(stdintosock.o)\
  $(d0root)/test/unix/unix.a(stopme.o)\
  $(d0root)/test/unix/unix.a(str_trim.o)\
  $(d0root)/test/unix/unix.a(str_upcase.o)\
  $(d0root)/test/unix/unix.a(sys_asctim.o)\
  $(d0root)/test/unix/unix.a(sys_bintim.o)\
  $(d0root)/test/unix/unix.a(sys_gettim.o)\
  $(d0root)/test/unix/unix.a(sys_numtim.o)\
  $(d0root)/test/unix/unix.a(tasteofd0.o)\
  $(d0root)/test/unix/unix.a(time.o)\
  $(d0root)/test/unix/unix.a(unix_time747.o)\
  $(d0root)/test/unix/unix.a(utc_tfset417.o)\
  $(d0root)/test/unix/unix.a(vfa.o)\
  $(d0root)/test/unix/unix.a(vff.o)\
  $(d0root)/test/unix/unix.a(vfl.o)\
  $(d0root)/test/unix/unix.a(vfn.o)\
  $(d0root)/test/unix/unix.a(vms_ttime747.o)\
  $(d0root)/test/unix/unix.a(vmstounix.o)\
  $(d0root)/test/unix/unix.a(vxinvb.o)\
  $(d0root)/test/unix/unix.a(wscrl.o)
	ar $(ARFLAGS) $(d0root)/test/unix/unix.a `ls $(SCRATCH)/a1/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/test/unix/unix.a `ls $(SCRATCH)/a2/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/test/unix/unix.a `ls $(SCRATCH)/a3/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/test/unix/unix.a `ls $(SCRATCH)/a4/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/test/unix/unix.a `ls $(SCRATCH)/a5/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/test/unix/unix.a `ls $(SCRATCH)/a6/*.o 2> /dev/null`
	rmdirp $(SCRATCH)
$(SCRATCH)/nothing.nl:
	rmdirp $(SCRATCH)
	mkdirp $(SCRATCH)/a1
	mkdirp $(SCRATCH)/a2
	mkdirp $(SCRATCH)/a3
	mkdirp $(SCRATCH)/a4
	mkdirp $(SCRATCH)/a5
	mkdirp $(SCRATCH)/a6
$(d0root)/test/unix/unix.a(avoid_nfs.o):\
  $(d0library)/unix/source/avoid_nfs.c
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/avoid_nfs.c ;\
	mv avoid_nfs.o $(SCRATCH)/a1/avoid_nfs.o ;\
	)
$(d0root)/test/unix/unix.a(c_depend.o):\
  $(d0library)/unix/source/c_depend.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(LCCFLAGS) $(OPT) $(d0library)/unix/source/c_depend.c ;\
	mv c_depend.o $(SCRATCH)/a1/c_depend.o ;\
	)
$(d0root)/test/unix/unix.a(cfget.o):\
  $(d0library)/unix/source/cfget.c
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/cfget.c ;\
	mv cfget.o $(SCRATCH)/a1/cfget.o ;\
	)
$(d0root)/test/unix/unix.a(check_exec.o):\
  $(d0library)/unix/source/check_exec.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/check_exec.c ;\
	mv check_exec.o $(SCRATCH)/a1/check_exec.o ;\
	)
$(d0root)/test/unix/unix.a(cstring.o):\
  $(d0library)/unix/source/cstring.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/cstring.c ;\
	mv cstring.o $(SCRATCH)/a1/cstring.o ;\
	)
$(d0root)/test/unix/unix.a(d0_loc.o):\
  $(d0library)/unix/source/d0_loc.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/d0_loc.c ;\
	mv d0_loc.o $(SCRATCH)/a1/d0_loc.o ;\
	)
$(d0root)/test/unix/unix.a(d0_nodename.o):\
  $(d0library)/unix/source/d0_nodename.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/d0_nodename.c ;\
	mv d0_nodename.o $(SCRATCH)/a1/d0_nodename.o ;\
	)
$(d0root)/test/unix/unix.a(d0_readlink.o):\
  $(d0library)/unix/source/d0_readlink.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/d0_readlink.c ;\
	mv d0_readlink.o $(SCRATCH)/a1/d0_readlink.o ;\
	)
$(d0root)/test/unix/unix.a(d0_tz_offset.o):\
  $(d0library)/unix/source/d0_tz_offset.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/d0_tz_offset.c ;\
	mv d0_tz_offset.o $(SCRATCH)/a1/d0_tz_offset.o ;\
	)
$(d0root)/test/unix/unix.a(d0echo.o):\
  $(d0library)/unix/source/d0echo.c
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/d0echo.c ;\
	mv d0echo.o $(SCRATCH)/a1/d0echo.o ;\
	)
$(d0root)/test/unix/unix.a(d0rexec.o):\
  $(d0library)/unix/source/d0rexec.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/d0rexec.c ;\
	mv d0rexec.o $(SCRATCH)/a1/d0rexec.o ;\
	)
$(d0root)/test/unix/unix.a(d0rl.o):\
  $(d0library)/unix/source/d0rl.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/d0rl.c ;\
	mv d0rl.o $(SCRATCH)/a1/d0rl.o ;\
	)
$(d0root)/test/unix/unix.a(date.o):\
  $(d0library)/unix/source/date.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/unix/source/date.f ;\
	mv date.o $(SCRATCH)/a1/date.o ;\
	)
$(d0root)/test/unix/unix.a(daylifset132.o):\
  $(d0library)/unix/source/daylight_time_offset.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/daylight_time_offset.c ;\
	mv daylight_time_offset.o $(SCRATCH)/a1/daylifset132.o ;\
	)
$(d0root)/test/unix/unix.a(exit.o):\
  $(d0library)/unix/source/exit.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/exit.c ;\
	mv exit.o $(SCRATCH)/a1/exit.o ;\
	)
$(d0root)/test/unix/unix.a(f_depend.o):\
  $(d0library)/unix/source/f_depend.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(LCCFLAGS) $(OPT) $(d0library)/unix/source/f_depend.c ;\
	mv f_depend.o $(SCRATCH)/a1/f_depend.o ;\
	)
$(d0root)/test/unix/unix.a(fa.o):\
  $(d0library)/unix/source/fa.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/fa.c ;\
	mv fa.o $(SCRATCH)/a1/fa.o ;\
	)
$(d0root)/test/unix/unix.a(fatmepand380.o):\
  $(d0library)/unix/source/fatmen_expand.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/fatmen_expand.c ;\
	mv fatmen_expand.o $(SCRATCH)/a2/fatmepand380.o ;\
	)
$(d0root)/test/unix/unix.a(fatme_dir794.o):\
  $(d0library)/unix/source/fatmen_expand_dir.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/fatmen_expand_dir.c ;\
	mv fatmen_expand_dir.o $(SCRATCH)/a2/fatme_dir794.o ;\
	)
$(d0root)/test/unix/unix.a(fatmen_find.o):\
  $(d0library)/unix/source/fatmen_find.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/fatmen_find.c ;\
	mv fatmen_find.o $(SCRATCH)/a2/fatmen_find.o ;\
	)
$(d0root)/test/unix/unix.a(ff.o):\
  $(d0library)/unix/source/ff.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/ff.c ;\
	mv ff.o $(SCRATCH)/a2/ff.o ;\
	)
$(d0root)/test/unix/unix.a(find_file.o):\
  $(d0library)/unix/source/find_file.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/find_file.c ;\
	mv find_file.o $(SCRATCH)/a2/find_file.o ;\
	)
$(d0root)/test/unix/unix.a(fl.o):\
  $(d0library)/unix/source/fl.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/fl.c ;\
	mv fl.o $(SCRATCH)/a2/fl.o ;\
	)
$(d0root)/test/unix/unix.a(flush_std.o):\
  $(d0library)/unix/source/flush_std.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/flush_std.c ;\
	mv flush_std.o $(SCRATCH)/a2/flush_std.o ;\
	)
$(d0root)/test/unix/unix.a(fn.o):\
  $(d0library)/unix/source/fn.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/fn.c ;\
	mv fn.o $(SCRATCH)/a2/fn.o ;\
	)
$(d0root)/test/unix/unix.a(for_depend.o):\
  $(d0library)/unix/source/for_depend.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(LCCFLAGS) $(OPT) $(d0library)/unix/source/for_depend.c ;\
	mv for_depend.o $(SCRATCH)/a1/for_depend.o ;\
	)
$(d0root)/test/unix/unix.a(fstring.o):\
  $(d0library)/unix/source/fstring.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/fstring.c ;\
	mv fstring.o $(SCRATCH)/a2/fstring.o ;\
	)
$(d0root)/test/unix/unix.a(getpassnetrc.o):\
  $(d0library)/unix/source/getpassnetrc.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/getpassnetrc.c ;\
	mv getpassnetrc.o $(SCRATCH)/a2/getpassnetrc.o ;\
	)
$(d0root)/test/unix/unix.a(getusernetrc.o):\
  $(d0library)/unix/source/getusernetrc.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/getusernetrc.c ;\
	mv getusernetrc.o $(SCRATCH)/a2/getusernetrc.o ;\
	)
$(d0root)/test/unix/unix.a(getword.o):\
  $(d0library)/unix/source/getword.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/getword.c ;\
	mv getword.o $(SCRATCH)/a2/getword.o ;\
	)
$(d0root)/test/unix/unix.a(glob.o):\
  $(d0library)/unix/source/glob.c\
  $(d0library)/unix/source/glob.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/glob.c ;\
	mv glob.o $(SCRATCH)/a2/glob.o ;\
	)
$(d0root)/test/unix/unix.a(idate.o):\
  $(d0library)/unix/source/idate.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/unix/source/idate.f ;\
	mv idate.o $(SCRATCH)/a2/idate.o ;\
	)
$(d0root)/test/unix/unix.a(jpi_cputim.o):\
  $(d0library)/unix/source/jpi_cputim.c
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/jpi_cputim.c ;\
	mv jpi_cputim.o $(SCRATCH)/a2/jpi_cputim.o ;\
	)
$(d0root)/test/unix/unix.a(lib_dtime356.o):\
  $(d0library)/unix/source/lib_date_time.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/unix/source/lib_date_time.f ;\
	mv lib_date_time.o $(SCRATCH)/a2/lib_dtime356.o ;\
	)
$(d0root)/test/unix/unix.a(lib_day.o):\
  $(d0library)/unix/source/lib_day.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/lib_day.c ;\
	mv lib_day.o $(SCRATCH)/a2/lib_day.o ;\
	)
$(d0root)/test/unix/unix.a(lib_dfile554.o):\
  $(d0library)/unix/source/lib_delete_file.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/lib_delete_file.c ;\
	mv lib_delete_file.o $(SCRATCH)/a2/lib_dfile554.o ;\
	)
$(d0root)/test/unix/unix.a(lib_ffile344.o):\
  $(d0library)/unix/source/lib_find_file.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/lib_find_file.c ;\
	mv lib_find_file.o $(SCRATCH)/a2/lib_ffile344.o ;\
	)
$(d0root)/test/unix/unix.a(lib_free_vm.o):\
  $(d0library)/unix/source/lib_free_vm.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/lib_free_vm.c ;\
	mv lib_free_vm.o $(SCRATCH)/a2/lib_free_vm.o ;\
	)
$(d0root)/test/unix/unix.a(lib_geign577.o):\
  $(d0library)/unix/source/lib_get_foreign.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/unix/source/lib_get_foreign.f ;\
	mv lib_get_foreign.o $(SCRATCH)/a3/lib_geign577.o ;\
	)
$(d0root)/test/unix/unix.a(lib_get_vm.o):\
  $(d0library)/unix/source/lib_get_vm.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/lib_get_vm.c ;\
	mv lib_get_vm.o $(SCRATCH)/a3/lib_get_vm.o ;\
	)
$(d0root)/test/unix/unix.a(lib_int_over.o):\
  $(d0library)/unix/source/lib_int_over.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/lib_int_over.c ;\
	mv lib_int_over.o $(SCRATCH)/a3/lib_int_over.o ;\
	)
$(d0root)/test/unix/unix.a(lib_rfile559.o):\
  $(d0library)/unix/source/lib_rename_file.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/lib_rename_file.c ;\
	mv lib_rename_file.o $(SCRATCH)/a3/lib_rfile559.o ;\
	)
$(d0root)/test/unix/unix.a(lib_signal.o):\
  $(d0library)/unix/source/lib_signal.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/unix/source/lib_signal.f ;\
	mv lib_signal.o $(SCRATCH)/a3/lib_signal.o ;\
	)
$(d0root)/test/unix/unix.a(lib_stop.o):\
  $(d0library)/unix/source/lib_stop.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/unix/source/lib_stop.f ;\
	mv lib_stop.o $(SCRATCH)/a3/lib_stop.o ;\
	)
$(d0root)/test/unix/unix.a(lib_wait.o):\
  $(d0library)/unix/source/lib_wait.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/lib_wait.c ;\
	mv lib_wait.o $(SCRATCH)/a3/lib_wait.o ;\
	)
$(d0root)/test/unix/unix.a(make__rcp701.o):\
  $(d0library)/unix/source/make_copy_fz_rcp.c
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/make_copy_fz_rcp.c ;\
	mv make_copy_fz_rcp.o $(SCRATCH)/a3/make__rcp701.o ;\
	)
$(d0root)/test/unix/unix.a(segv_fault.o):\
  $(d0library)/unix/source/segv_fault.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/segv_fault.c ;\
	mv segv_fault.o $(SCRATCH)/a3/segv_fault.o ;\
	)
$(d0root)/test/unix/unix.a(smg_bdate843.o):\
  $(d0library)/unix/source/smg_begin_pasteboard_update.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_begin_pasteboard_update.c ;\
	mv smg_begin_pasteboard_update.o $(SCRATCH)/a3/smg_bdate843.o ;\
	)
$(d0root)/test/unix/unix.a(smg_bell.o):\
  $(d0library)/unix/source/smg_bell.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_bell.c ;\
	mv smg_bell.o $(SCRATCH)/a3/smg_bell.o ;\
	)
$(d0root)/test/unix/unix.a(smg_cport037.o):\
  $(d0library)/unix/source/smg_change_viewport.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_change_viewport.c ;\
	mv smg_change_viewport.o $(SCRATCH)/a3/smg_cport037.o ;\
	)
$(d0root)/test/unix/unix.a(smg_cplay769.o):\
  $(d0library)/unix/source/smg_change_virtual_display.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_change_virtual_display.c ;\
	mv smg_change_virtual_display.o $(SCRATCH)/a3/smg_cplay769.o ;\
	)
$(d0root)/test/unix/unix.a(smg_cplay598.o):\
  $(d0library)/unix/source/smg_copy_virtual_display.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_copy_virtual_display.c ;\
	mv smg_copy_virtual_display.o $(SCRATCH)/a3/smg_cplay598.o ;\
	)
$(d0root)/test/unix/unix.a(smg_coard216.o):\
  $(d0library)/unix/source/smg_create_pasteboard.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_create_pasteboard.c ;\
	mv smg_create_pasteboard.o $(SCRATCH)/a3/smg_coard216.o ;\
	)
$(d0root)/test/unix/unix.a(smg_cport051.o):\
  $(d0library)/unix/source/smg_create_viewport.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_create_viewport.c ;\
	mv smg_create_viewport.o $(SCRATCH)/a3/smg_cport051.o ;\
	)
$(d0root)/test/unix/unix.a(smg_cplay783.o):\
  $(d0library)/unix/source/smg_create_virtual_display.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_create_virtual_display.c ;\
	mv smg_create_virtual_display.o $(SCRATCH)/a3/smg_cplay783.o ;\
	)
$(d0root)/test/unix/unix.a(smg_coard874.o):\
  $(d0library)/unix/source/smg_create_virtual_keyboard.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_create_virtual_keyboard.c ;\
	mv smg_create_virtual_keyboard.o $(SCRATCH)/a3/smg_coard874.o ;\
	)
$(d0root)/test/unix/unix.a(smg_clumn851.o):\
  $(d0library)/unix/source/smg_cursor_column.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_cursor_column.c ;\
	mv smg_cursor_column.o $(SCRATCH)/a3/smg_clumn851.o ;\
	)
$(d0root)/test/unix/unix.a(smg_c_row541.o):\
  $(d0library)/unix/source/smg_cursor_row.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_cursor_row.c ;\
	mv smg_cursor_row.o $(SCRATCH)/a3/smg_c_row541.o ;\
	)
$(d0root)/test/unix/unix.a(smg_doard215.o):\
  $(d0library)/unix/source/smg_delete_pasteboard.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_delete_pasteboard.c ;\
	mv smg_delete_pasteboard.o $(SCRATCH)/a4/smg_doard215.o ;\
	)
$(d0root)/test/unix/unix.a(smg_dport050.o):\
  $(d0library)/unix/source/smg_delete_viewport.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_delete_viewport.c ;\
	mv smg_delete_viewport.o $(SCRATCH)/a4/smg_dport050.o ;\
	)
$(d0root)/test/unix/unix.a(smg_dplay782.o):\
  $(d0library)/unix/source/smg_delete_virtual_display.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_delete_virtual_display.c ;\
	mv smg_delete_virtual_display.o $(SCRATCH)/a4/smg_dplay782.o ;\
	)
$(d0root)/test/unix/unix.a(smg_doard873.o):\
  $(d0library)/unix/source/smg_delete_virtual_keyboard.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_delete_virtual_keyboard.c ;\
	mv smg_delete_virtual_keyboard.o $(SCRATCH)/a4/smg_doard873.o ;\
	)
$(d0root)/test/unix/unix.a(smg_dnput093.o):\
  $(d0library)/unix/source/smg_disable_unsolicited_input.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_disable_unsolicited_input.c ;\
	mv smg_disable_unsolicited_input.o $(SCRATCH)/a4/smg_dnput093.o ;\
	)
$(d0root)/test/unix/unix.a(smg_enput984.o):\
  $(d0library)/unix/source/smg_enable_unsolicited_input.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_enable_unsolicited_input.c ;\
	mv smg_enable_unsolicited_input.o $(SCRATCH)/a4/smg_enput984.o ;\
	)
$(d0root)/test/unix/unix.a(smg_edate637.o):\
  $(d0library)/unix/source/smg_end_pasteboard_update.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_end_pasteboard_update.c ;\
	mv smg_end_pasteboard_update.o $(SCRATCH)/a4/smg_edate637.o ;\
	)
$(d0root)/test/unix/unix.a(smg_eplay813.o):\
  $(d0library)/unix/source/smg_erase_display.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_erase_display.c ;\
	mv smg_erase_display.o $(SCRATCH)/a4/smg_eplay813.o ;\
	)
$(d0root)/test/unix/unix.a(smg_eline479.o):\
  $(d0library)/unix/source/smg_erase_line.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_erase_line.c ;\
	mv smg_erase_line.o $(SCRATCH)/a4/smg_eline479.o ;\
	)
$(d0root)/test/unix/unix.a(smg_eoard116.o):\
  $(d0library)/unix/source/smg_erase_pasteboard.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_erase_pasteboard.c ;\
	mv smg_erase_pasteboard.o $(SCRATCH)/a4/smg_eoard116.o ;\
	)
$(d0root)/test/unix/unix.a(smg_error.o):\
  $(d0library)/unix/source/smg_error.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_error.c ;\
	mv smg_error.o $(SCRATCH)/a4/smg_error.o ;\
	)
$(d0root)/test/unix/unix.a(smg_gattr143.o):\
  $(d0library)/unix/source/smg_get_display_attr.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_get_display_attr.c ;\
	mv smg_get_display_attr.o $(SCRATCH)/a4/smg_gattr143.o ;\
	)
$(d0root)/test/unix/unix.a(smg_ginfo128.o):\
  $(d0library)/unix/source/smg_get_pasting_info.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_get_pasting_info.c ;\
	mv smg_get_pasting_info.o $(SCRATCH)/a4/smg_ginfo128.o ;\
	)
$(d0root)/test/unix/unix.a(smg_gchar252.o):\
  $(d0library)/unix/source/smg_get_viewport_char.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_get_viewport_char.c ;\
	mv smg_get_viewport_char.o $(SCRATCH)/a4/smg_gchar252.o ;\
	)
$(d0root)/test/unix/unix.a(smg_getch.o):\
  $(d0library)/unix/source/smg_getch.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_getch.c ;\
	mv smg_getch.o $(SCRATCH)/a4/smg_getch.o ;\
	)
$(d0root)/test/unix/unix.a(smg_getstr.o):\
  $(d0library)/unix/source/smg_getstr.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_getstr.c ;\
	mv smg_getstr.o $(SCRATCH)/a4/smg_getstr.o ;\
	)
$(d0root)/test/unix/unix.a(smg_lrder677.o):\
  $(d0library)/unix/source/smg_label_border.c
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_label_border.c ;\
	mv smg_label_border.o $(SCRATCH)/a4/smg_lrder677.o ;\
	)
$(d0root)/test/unix/unix.a(smg_mtext419.o):\
  $(d0library)/unix/source/smg_move_text.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_move_text.c ;\
	mv smg_move_text.o $(SCRATCH)/a4/smg_mtext419.o ;\
	)
$(d0root)/test/unix/unix.a(smg_mplay594.o):\
  $(d0library)/unix/source/smg_move_virtual_display.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_move_virtual_display.c ;\
	mv smg_move_virtual_display.o $(SCRATCH)/a4/smg_mplay594.o ;\
	)
$(d0root)/test/unix/unix.a(smg_pplay826.o):\
  $(d0library)/unix/source/smg_paste_display.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_paste_display.c ;\
	mv smg_paste_display.o $(SCRATCH)/a4/smg_pplay826.o ;\
	)
$(d0root)/test/unix/unix.a(smg_pplay696.o):\
  $(d0library)/unix/source/smg_paste_virtual_display.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_paste_virtual_display.c ;\
	mv smg_paste_virtual_display.o $(SCRATCH)/a5/smg_pplay696.o ;\
	)
$(d0root)/test/unix/unix.a(smg_phars401.o):\
  $(d0library)/unix/source/smg_put_chars.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_put_chars.c ;\
	mv smg_put_chars.o $(SCRATCH)/a5/smg_phars401.o ;\
	)
$(d0root)/test/unix/unix.a(smg_pwide921.o):\
  $(d0library)/unix/source/smg_put_chars_wide.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_put_chars_wide.c ;\
	mv smg_put_chars_wide.o $(SCRATCH)/a5/smg_pwide921.o ;\
	)
$(d0root)/test/unix/unix.a(smg_put_line.o):\
  $(d0library)/unix/source/smg_put_line.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_put_line.c ;\
	mv smg_put_line.o $(SCRATCH)/a5/smg_put_line.o ;\
	)
$(d0root)/test/unix/unix.a(smg_proll066.o):\
  $(d0library)/unix/source/smg_put_with_scroll.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_put_with_scroll.c ;\
	mv smg_put_with_scroll.o $(SCRATCH)/a5/smg_proll066.o ;\
	)
$(d0root)/test/unix/unix.a(smg_rplay228.o):\
  $(d0library)/unix/source/smg_read_from_display.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_read_from_display.c ;\
	mv smg_read_from_display.o $(SCRATCH)/a5/smg_rplay228.o ;\
	)
$(d0root)/test/unix/unix.a(smg_rroke932.o):\
  $(d0library)/unix/source/smg_read_keystroke.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_read_keystroke.c ;\
	mv smg_read_keystroke.o $(SCRATCH)/a5/smg_rroke932.o ;\
	)
$(d0root)/test/unix/unix.a(smg_rring602.o):\
  $(d0library)/unix/source/smg_read_string.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_read_string.c ;\
	mv smg_read_string.o $(SCRATCH)/a5/smg_rring602.o ;\
	)
$(d0root)/test/unix/unix.a(smg_rtion404.o):\
  $(d0library)/unix/source/smg_rendition.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_rendition.c ;\
	mv smg_rendition.o $(SCRATCH)/a5/smg_rtion404.o ;\
	)
$(d0root)/test/unix/unix.a(smg_rreen922.o):\
  $(d0library)/unix/source/smg_repaint_screen.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_repaint_screen.c ;\
	mv smg_repaint_screen.o $(SCRATCH)/a5/smg_rreen922.o ;\
	)
$(d0root)/test/unix/unix.a(smg_rplay911.o):\
  $(d0library)/unix/source/smg_repaste_virtual_display.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_repaste_virtual_display.c ;\
	mv smg_repaste_virtual_display.o $(SCRATCH)/a5/smg_rplay911.o ;\
	)
$(d0root)/test/unix/unix.a(smg_rterm514.o):\
  $(d0library)/unix/source/smg_reset_term.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_reset_term.c ;\
	mv smg_reset_term.o $(SCRATCH)/a5/smg_rterm514.o ;\
	)
$(d0root)/test/unix/unix.a(smg_r_pos302.o):\
  $(d0library)/unix/source/smg_return_cursor_pos.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_return_cursor_pos.c ;\
	mv smg_return_cursor_pos.o $(SCRATCH)/a5/smg_r_pos302.o ;\
	)
$(d0root)/test/unix/unix.a(smg_scroll.o):\
  $(d0library)/unix/source/smg_scroll.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_scroll.c ;\
	mv smg_scroll.o $(SCRATCH)/a5/smg_scroll.o ;\
	)
$(d0root)/test/unix/unix.a(smg_s_abs934.o):\
  $(d0library)/unix/source/smg_set_cursor_abs.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_set_cursor_abs.c ;\
	mv smg_set_cursor_abs.o $(SCRATCH)/a5/smg_s_abs934.o ;\
	)
$(d0root)/test/unix/unix.a(smg_smode045.o):\
  $(d0library)/unix/source/smg_set_cursor_mode.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_set_cursor_mode.c ;\
	mv smg_set_cursor_mode.o $(SCRATCH)/a5/smg_smode045.o ;\
	)
$(d0root)/test/unix/unix.a(smg_sgion106.o):\
  $(d0library)/unix/source/smg_set_display_scroll_region.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_set_display_scroll_region.c ;\
	mv smg_set_display_scroll_region.o $(SCRATCH)/a5/smg_sgion106.o ;\
	)
$(d0root)/test/unix/unix.a(smg_uplay923.o):\
  $(d0library)/unix/source/smg_unpaste_virtual_display.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_unpaste_virtual_display.c ;\
	mv smg_unpaste_virtual_display.o $(SCRATCH)/a5/smg_uplay923.o ;\
	)
$(d0root)/test/unix/unix.a(smg_update.o):\
  $(d0library)/unix/source/smg_update.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_update.c ;\
	mv smg_update.o $(SCRATCH)/a5/smg_update.o ;\
	)
$(d0root)/test/unix/unix.a(smg_vplay946.o):\
  $(d0library)/unix/source/smg_verify_display.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/smg_verify_display.c ;\
	mv smg_verify_display.o $(SCRATCH)/a5/smg_vplay946.o ;\
	)
$(d0root)/test/unix/unix.a(stdintosock.o):\
  $(d0library)/unix/source/stdintosock.c
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/stdintosock.c ;\
	mv stdintosock.o $(SCRATCH)/a6/stdintosock.o ;\
	)
$(d0root)/test/unix/unix.a(stopme.o):\
  $(d0library)/unix/source/stopme.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/stopme.c ;\
	mv stopme.o $(SCRATCH)/a6/stopme.o ;\
	)
$(d0root)/test/unix/unix.a(str_trim.o):\
  $(d0library)/unix/source/str_trim.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/unix/source/str_trim.f ;\
	mv str_trim.o $(SCRATCH)/a6/str_trim.o ;\
	)
$(d0root)/test/unix/unix.a(str_upcase.o):\
  $(d0library)/unix/source/str_upcase.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/unix/source/str_upcase.f ;\
	mv str_upcase.o $(SCRATCH)/a6/str_upcase.o ;\
	)
$(d0root)/test/unix/unix.a(sys_asctim.o):\
  $(d0library)/unix/source/sys_asctim.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/sys_asctim.c ;\
	mv sys_asctim.o $(SCRATCH)/a6/sys_asctim.o ;\
	)
$(d0root)/test/unix/unix.a(sys_bintim.o):\
  $(d0library)/unix/source/sys_bintim.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/sys_bintim.c ;\
	mv sys_bintim.o $(SCRATCH)/a6/sys_bintim.o ;\
	)
$(d0root)/test/unix/unix.a(sys_gettim.o):\
  $(d0library)/unix/source/sys_gettim.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/sys_gettim.c ;\
	mv sys_gettim.o $(SCRATCH)/a6/sys_gettim.o ;\
	)
$(d0root)/test/unix/unix.a(sys_numtim.o):\
  $(d0library)/unix/source/sys_numtim.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/sys_numtim.c ;\
	mv sys_numtim.o $(SCRATCH)/a6/sys_numtim.o ;\
	)
$(d0root)/test/unix/unix.a(tasteofd0.o):\
  $(d0library)/unix/source/tasteofd0.c
	(cd $(d0root);\
	$(CC) $(LCCFLAGS) $(OPT) $(d0library)/unix/source/tasteofd0.c ;\
	mv tasteofd0.o $(SCRATCH)/a1/tasteofd0.o ;\
	)
$(d0root)/test/unix/unix.a(time.o):\
  $(d0library)/unix/source/time.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/unix/source/time.f ;\
	mv time.o $(SCRATCH)/a6/time.o ;\
	)
$(d0root)/test/unix/unix.a(unix_time747.o):\
  $(d0library)/unix/source/unix_to_vms_time.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/unix/source/unix_to_vms_time.f ;\
	mv unix_to_vms_time.o $(SCRATCH)/a6/unix_time747.o ;\
	)
$(d0root)/test/unix/unix.a(utc_tfset417.o):\
  $(d0library)/unix/source/utc_tz_offset.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/utc_tz_offset.c ;\
	mv utc_tz_offset.o $(SCRATCH)/a6/utc_tfset417.o ;\
	)
$(d0root)/test/unix/unix.a(vfa.o):\
  $(d0library)/unix/source/vfa.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/vfa.c ;\
	mv vfa.o $(SCRATCH)/a6/vfa.o ;\
	)
$(d0root)/test/unix/unix.a(vff.o):\
  $(d0library)/unix/source/vff.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/vff.c ;\
	mv vff.o $(SCRATCH)/a6/vff.o ;\
	)
$(d0root)/test/unix/unix.a(vfl.o):\
  $(d0library)/unix/source/vfl.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/vfl.c ;\
	mv vfl.o $(SCRATCH)/a6/vfl.o ;\
	)
$(d0root)/test/unix/unix.a(vfn.o):\
  $(d0library)/unix/source/vfn.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/vfn.c ;\
	mv vfn.o $(SCRATCH)/a6/vfn.o ;\
	)
$(d0root)/test/unix/unix.a(vms_ttime747.o):\
  $(d0library)/unix/source/vms_to_unix_time.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/unix/source/vms_to_unix_time.f ;\
	mv vms_to_unix_time.o $(SCRATCH)/a6/vms_ttime747.o ;\
	)
$(d0root)/test/unix/unix.a(vmstounix.o):\
  $(d0library)/unix/source/vmstounix.c\
  $(d0library)/unix/source/unix.h
	(cd $(d0root);\
	$(CC) $(LCCFLAGS) $(OPT) $(d0library)/unix/source/vmstounix.c ;\
	mv vmstounix.o $(SCRATCH)/a1/vmstounix.o ;\
	)
$(d0root)/test/unix/unix.a(vxinvb.o):\
  $(d0library)/unix/source/vxinvb.c
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/vxinvb.c ;\
	mv vxinvb.o $(SCRATCH)/a6/vxinvb.o ;\
	)
$(d0root)/test/unix/unix.a(wscrl.o):\
  $(d0library)/unix/source/wscrl.c\
  $(d0library)/unix/source/smg.h
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) $(d0library)/unix/source/wscrl.c ;\
	mv wscrl.o $(SCRATCH)/a6/wscrl.o ;\
	)
pre:\
  $(d0library)/unix/source/c_depend.c\
  $(d0library)/unix/source/f_depend.c\
  $(d0library)/unix/source/for_depend.c\
  $(d0library)/unix/source/tasteofd0.c\
  $(d0library)/unix/source/vmstounix.c
$(d0library)/unix/source/c_depend.c:\
  $(d0library)/unix/source/c_depend.lex\
  $(d0library)/unix/source/unix.h
	lex -t $(d0library)/unix/source/c_depend.lex > $(d0library)/unix/source/c_depend.c
$(d0library)/unix/source/f_depend.c:\
  $(d0library)/unix/source/f_depend.lex\
  $(d0library)/unix/source/unix.h
	lex -t $(d0library)/unix/source/f_depend.lex > $(d0library)/unix/source/f_depend.c
$(d0library)/unix/source/for_depend.c:\
  $(d0library)/unix/source/for_depend.lex\
  $(d0library)/unix/source/unix.h
	lex -t $(d0library)/unix/source/for_depend.lex > $(d0library)/unix/source/for_depend.c
$(d0library)/unix/source/tasteofd0.c:\
  $(d0library)/unix/source/tasteofd0.lex
	lex -t $(d0library)/unix/source/tasteofd0.lex > $(d0library)/unix/source/tasteofd0.c
$(d0library)/unix/source/vmstounix.c:\
  $(d0library)/unix/source/vmstounix.lex\
  $(d0library)/unix/source/unix.h
	lex -t $(d0library)/unix/source/vmstounix.lex > $(d0library)/unix/source/vmstounix.c
for:
	touch $(d0library)/unix/source/for
