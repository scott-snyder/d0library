SHELL = /bin/sh
CCFLAGS = -c -dollar -G 0 -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
LCCFLAGS = -c -dollar -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
F77FLAGS = -c -nocpp -static -col72 -backslash -G 0 -Olimit 2500 -Wo,-loopunroll,2
DEBUG = -O0 -g2
OPT = -O2
ARFLAGS = crsl
SCRATCH = $(d0library)/admin/tmp/d0librar/userlib/24003/sgidi3
FLAVOR = SIUNIX
CC = cc
LEX = lex
F77 = f77
.IGNORE:
.SUFFIXES:
debug :\
  deb_sgidi3.a
opt :\
  sgidi3.a
deb_sgidi3.a : $(d0root)/graphics_util/deb_sgidi3.a
	@ echo deb_sgidi3.a is up to date
$(d0root)/graphics_util/deb_sgidi3.a:: $(SCRATCH)/nothing.nl
$(d0root)/graphics_util/deb_sgidi3.a::\
  $(d0root)/graphics_util/deb_sgidi3.a(all_dixie814.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(close_tkwind.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(d_dmpmat.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(d_matcpy.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(d_matmul.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(d_matrot.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(d_matsca.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(d_mattra.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(d_matuni.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(dev_clear.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(dev_cndow717.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(dev_color.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(dev_deflin.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(dev_draw.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(dev_force.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(dev_linewi.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(dev_mark.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(dev_move.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(dev_movscr.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(dev_ondow617.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(dev_page.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(dev_plin.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(dev_polf.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(dev_poly.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(dev_rot.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(dev_scale.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(dev_setlin.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(dev_transf.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(dev_transl.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(dev_wners947.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(dmpseg.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(hcpy_name374.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(hls_to_rgb.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(j_cross.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(j_delsegm.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(j_devtrn.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(j_dummies.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(j_dump.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(j_grey.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(j_hcoegin370.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(j_hcovice477.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(j_hcopy_end.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(j_menu3d.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(j_projs.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(just_ents429.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(open_tkwind.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(polyn.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(ps_color.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(ps_crns.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(ps_init.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(ps_linewi.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(ps_move.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(ps_poly.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(ps_setlin.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(qm_color.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(qm_crns.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(qm_init.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(qm_move.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(qm_poly.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_j1iget.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_j1strg.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_j2strg.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_j3draw.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_j3mark.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_j3move.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_j3plgn.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_j3poly.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_j3rget.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_j3strg.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_j4rget.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jaload.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jarc.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jarset.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jasave.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jaspek.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jattrb.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jbackg.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jbase.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jbegin.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jbgbat.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jbuild.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jcircl.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jclear.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jclose.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jcmark.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jcolor.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jconpk.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jconvw.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jconwv.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jcotbl.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jcview.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jdbase.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jdcolr.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jdd3d.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jddete.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jdend.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jdetec.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jdevof.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jdevon.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jdevvp.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jdevwn.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jdfont.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jdinit.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jdjust.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jdlsty.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jdlwid.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jdmark.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jdpidx.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jdpint.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jdpinx.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jdpkid.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jdraw.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jdsize.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jdvisb.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jelseg.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jenbat.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jend.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jepseg.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jescap.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jextnt.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jfiles.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jfont.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jframe.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jfsopn.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jhclip.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jhcpy.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jhilit.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jhithr.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jhstrg.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jidisa.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jienab.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jiescp.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jinten.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jiqddl.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jiqdev.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jiqdil.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jiqdim.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jiqtxt.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jisgmt.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jiwind.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jjust.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jkeybs.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jlocat.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jlstyl.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jlwide.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jmark.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jmodel.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jmodon.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jmove.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jmstrg.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jnorml.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jopen.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jp3mrk.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jparal.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jparob.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jpause.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jpecho.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jpedge.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jperob.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jpersp.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jpick.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jpidex.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jpintr.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jpkapr.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jpkid.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jplane.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jpmark.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jpolgn.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jpoly.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jpr3mr.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jprmrk.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jpurge.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jr3dra.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jr3mov.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jr3mrk.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jr3pgn.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jr3ply.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jrclos.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jrdraw.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jrect.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jright.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jrmark.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jrmove.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jropen.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jrplgn.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jrpoly.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jrrect.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jsaall.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jsasso.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jsectr.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jsetdb.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jseter.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jsgpri.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jsize.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jspick.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jsview.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jt2all.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jtrans.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jttype.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jupvec.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jvisbl.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jvload.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jvport.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jvsave.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jvspac.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jvupln.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jvupnt.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jwclip.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jwindo.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jyclip.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_jyon.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(sgi_re_3d329.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(tk_clear.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(tk_color.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(tk_crns.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(tk_drascr.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(tk_draw.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(tk_fin.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(tk_force.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(tk_line2.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(tk_move.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(tk_movscr.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(tk_out.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(tk_poly.o)\
  $(d0root)/graphics_util/deb_sgidi3.a(tk_text.o)
	ar $(ARFLAGS) $(d0root)/graphics_util/deb_sgidi3.a `ls $(SCRATCH)/a1/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/graphics_util/deb_sgidi3.a `ls $(SCRATCH)/a2/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/graphics_util/deb_sgidi3.a `ls $(SCRATCH)/a3/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/graphics_util/deb_sgidi3.a `ls $(SCRATCH)/a4/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/graphics_util/deb_sgidi3.a `ls $(SCRATCH)/a5/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/graphics_util/deb_sgidi3.a `ls $(SCRATCH)/a6/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/graphics_util/deb_sgidi3.a `ls $(SCRATCH)/a7/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/graphics_util/deb_sgidi3.a `ls $(SCRATCH)/a8/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/graphics_util/deb_sgidi3.a `ls $(SCRATCH)/a9/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/graphics_util/deb_sgidi3.a `ls $(SCRATCH)/a10/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/graphics_util/deb_sgidi3.a `ls $(SCRATCH)/a11/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/graphics_util/deb_sgidi3.a `ls $(SCRATCH)/a12/*.o 2> /dev/null`
	rmdirp $(SCRATCH)
sgidi3.a : $(d0root)/graphics_util/sgidi3.a
	@ echo sgidi3.a is up to date
$(d0root)/graphics_util/sgidi3.a:: $(SCRATCH)/nothing.nl
$(d0root)/graphics_util/sgidi3.a::\
  $(d0root)/graphics_util/sgidi3.a(all_dixie814.o)\
  $(d0root)/graphics_util/sgidi3.a(close_tkwind.o)\
  $(d0root)/graphics_util/sgidi3.a(d_dmpmat.o)\
  $(d0root)/graphics_util/sgidi3.a(d_matcpy.o)\
  $(d0root)/graphics_util/sgidi3.a(d_matmul.o)\
  $(d0root)/graphics_util/sgidi3.a(d_matrot.o)\
  $(d0root)/graphics_util/sgidi3.a(d_matsca.o)\
  $(d0root)/graphics_util/sgidi3.a(d_mattra.o)\
  $(d0root)/graphics_util/sgidi3.a(d_matuni.o)\
  $(d0root)/graphics_util/sgidi3.a(dev_clear.o)\
  $(d0root)/graphics_util/sgidi3.a(dev_cndow717.o)\
  $(d0root)/graphics_util/sgidi3.a(dev_color.o)\
  $(d0root)/graphics_util/sgidi3.a(dev_deflin.o)\
  $(d0root)/graphics_util/sgidi3.a(dev_draw.o)\
  $(d0root)/graphics_util/sgidi3.a(dev_force.o)\
  $(d0root)/graphics_util/sgidi3.a(dev_linewi.o)\
  $(d0root)/graphics_util/sgidi3.a(dev_mark.o)\
  $(d0root)/graphics_util/sgidi3.a(dev_move.o)\
  $(d0root)/graphics_util/sgidi3.a(dev_movscr.o)\
  $(d0root)/graphics_util/sgidi3.a(dev_ondow617.o)\
  $(d0root)/graphics_util/sgidi3.a(dev_page.o)\
  $(d0root)/graphics_util/sgidi3.a(dev_plin.o)\
  $(d0root)/graphics_util/sgidi3.a(dev_polf.o)\
  $(d0root)/graphics_util/sgidi3.a(dev_poly.o)\
  $(d0root)/graphics_util/sgidi3.a(dev_rot.o)\
  $(d0root)/graphics_util/sgidi3.a(dev_scale.o)\
  $(d0root)/graphics_util/sgidi3.a(dev_setlin.o)\
  $(d0root)/graphics_util/sgidi3.a(dev_transf.o)\
  $(d0root)/graphics_util/sgidi3.a(dev_transl.o)\
  $(d0root)/graphics_util/sgidi3.a(dev_wners947.o)\
  $(d0root)/graphics_util/sgidi3.a(dmpseg.o)\
  $(d0root)/graphics_util/sgidi3.a(hcpy_name374.o)\
  $(d0root)/graphics_util/sgidi3.a(hls_to_rgb.o)\
  $(d0root)/graphics_util/sgidi3.a(j_cross.o)\
  $(d0root)/graphics_util/sgidi3.a(j_delsegm.o)\
  $(d0root)/graphics_util/sgidi3.a(j_devtrn.o)\
  $(d0root)/graphics_util/sgidi3.a(j_dummies.o)\
  $(d0root)/graphics_util/sgidi3.a(j_dump.o)\
  $(d0root)/graphics_util/sgidi3.a(j_grey.o)\
  $(d0root)/graphics_util/sgidi3.a(j_hcoegin370.o)\
  $(d0root)/graphics_util/sgidi3.a(j_hcovice477.o)\
  $(d0root)/graphics_util/sgidi3.a(j_hcopy_end.o)\
  $(d0root)/graphics_util/sgidi3.a(j_menu3d.o)\
  $(d0root)/graphics_util/sgidi3.a(j_projs.o)\
  $(d0root)/graphics_util/sgidi3.a(just_ents429.o)\
  $(d0root)/graphics_util/sgidi3.a(open_tkwind.o)\
  $(d0root)/graphics_util/sgidi3.a(polyn.o)\
  $(d0root)/graphics_util/sgidi3.a(ps_color.o)\
  $(d0root)/graphics_util/sgidi3.a(ps_crns.o)\
  $(d0root)/graphics_util/sgidi3.a(ps_init.o)\
  $(d0root)/graphics_util/sgidi3.a(ps_linewi.o)\
  $(d0root)/graphics_util/sgidi3.a(ps_move.o)\
  $(d0root)/graphics_util/sgidi3.a(ps_poly.o)\
  $(d0root)/graphics_util/sgidi3.a(ps_setlin.o)\
  $(d0root)/graphics_util/sgidi3.a(qm_color.o)\
  $(d0root)/graphics_util/sgidi3.a(qm_crns.o)\
  $(d0root)/graphics_util/sgidi3.a(qm_init.o)\
  $(d0root)/graphics_util/sgidi3.a(qm_move.o)\
  $(d0root)/graphics_util/sgidi3.a(qm_poly.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_j1iget.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_j1strg.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_j2strg.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_j3draw.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_j3mark.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_j3move.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_j3plgn.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_j3poly.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_j3rget.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_j3strg.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_j4rget.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jaload.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jarc.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jarset.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jasave.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jaspek.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jattrb.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jbackg.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jbase.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jbegin.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jbgbat.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jbuild.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jcircl.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jclear.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jclose.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jcmark.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jcolor.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jconpk.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jconvw.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jconwv.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jcotbl.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jcview.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jdbase.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jdcolr.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jdd3d.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jddete.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jdend.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jdetec.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jdevof.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jdevon.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jdevvp.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jdevwn.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jdfont.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jdinit.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jdjust.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jdlsty.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jdlwid.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jdmark.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jdpidx.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jdpint.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jdpinx.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jdpkid.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jdraw.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jdsize.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jdvisb.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jelseg.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jenbat.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jend.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jepseg.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jescap.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jextnt.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jfiles.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jfont.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jframe.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jfsopn.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jhclip.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jhcpy.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jhilit.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jhithr.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jhstrg.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jidisa.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jienab.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jiescp.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jinten.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jiqddl.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jiqdev.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jiqdil.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jiqdim.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jiqtxt.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jisgmt.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jiwind.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jjust.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jkeybs.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jlocat.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jlstyl.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jlwide.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jmark.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jmodel.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jmodon.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jmove.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jmstrg.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jnorml.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jopen.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jp3mrk.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jparal.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jparob.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jpause.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jpecho.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jpedge.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jperob.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jpersp.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jpick.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jpidex.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jpintr.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jpkapr.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jpkid.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jplane.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jpmark.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jpolgn.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jpoly.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jpr3mr.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jprmrk.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jpurge.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jr3dra.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jr3mov.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jr3mrk.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jr3pgn.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jr3ply.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jrclos.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jrdraw.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jrect.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jright.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jrmark.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jrmove.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jropen.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jrplgn.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jrpoly.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jrrect.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jsaall.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jsasso.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jsectr.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jsetdb.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jseter.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jsgpri.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jsize.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jspick.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jsview.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jt2all.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jtrans.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jttype.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jupvec.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jvisbl.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jvload.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jvport.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jvsave.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jvspac.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jvupln.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jvupnt.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jwclip.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jwindo.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jyclip.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_jyon.o)\
  $(d0root)/graphics_util/sgidi3.a(sgi_re_3d329.o)\
  $(d0root)/graphics_util/sgidi3.a(tk_clear.o)\
  $(d0root)/graphics_util/sgidi3.a(tk_color.o)\
  $(d0root)/graphics_util/sgidi3.a(tk_crns.o)\
  $(d0root)/graphics_util/sgidi3.a(tk_drascr.o)\
  $(d0root)/graphics_util/sgidi3.a(tk_draw.o)\
  $(d0root)/graphics_util/sgidi3.a(tk_fin.o)\
  $(d0root)/graphics_util/sgidi3.a(tk_force.o)\
  $(d0root)/graphics_util/sgidi3.a(tk_line2.o)\
  $(d0root)/graphics_util/sgidi3.a(tk_move.o)\
  $(d0root)/graphics_util/sgidi3.a(tk_movscr.o)\
  $(d0root)/graphics_util/sgidi3.a(tk_out.o)\
  $(d0root)/graphics_util/sgidi3.a(tk_poly.o)\
  $(d0root)/graphics_util/sgidi3.a(tk_text.o)
	ar $(ARFLAGS) $(d0root)/graphics_util/sgidi3.a `ls $(SCRATCH)/a1/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/graphics_util/sgidi3.a `ls $(SCRATCH)/a2/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/graphics_util/sgidi3.a `ls $(SCRATCH)/a3/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/graphics_util/sgidi3.a `ls $(SCRATCH)/a4/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/graphics_util/sgidi3.a `ls $(SCRATCH)/a5/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/graphics_util/sgidi3.a `ls $(SCRATCH)/a6/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/graphics_util/sgidi3.a `ls $(SCRATCH)/a7/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/graphics_util/sgidi3.a `ls $(SCRATCH)/a8/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/graphics_util/sgidi3.a `ls $(SCRATCH)/a9/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/graphics_util/sgidi3.a `ls $(SCRATCH)/a10/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/graphics_util/sgidi3.a `ls $(SCRATCH)/a11/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/graphics_util/sgidi3.a `ls $(SCRATCH)/a12/*.o 2> /dev/null`
	rmdirp $(SCRATCH)
$(SCRATCH)/nothing.nl:
	rmdirp $(SCRATCH)
	mkdirp $(SCRATCH)/a1
	mkdirp $(SCRATCH)/a2
	mkdirp $(SCRATCH)/a3
	mkdirp $(SCRATCH)/a4
	mkdirp $(SCRATCH)/a5
	mkdirp $(SCRATCH)/a6
	mkdirp $(SCRATCH)/a7
	mkdirp $(SCRATCH)/a8
	mkdirp $(SCRATCH)/a9
	mkdirp $(SCRATCH)/a10
	mkdirp $(SCRATCH)/a11
	mkdirp $(SCRATCH)/a12
$(d0root)/graphics_util/deb_sgidi3.a(all_dixie814.o):\
  $(d0root)/graphics_util/sgidi3/all_display_pixie.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/all_display_pixie.f ;\
	mv all_display_pixie.o $(SCRATCH)/a1/all_dixie814.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(close_tkwind.o):\
  $(d0root)/graphics_util/sgidi3/close_tkwind.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/close_tkwind.f ;\
	mv close_tkwind.o $(SCRATCH)/a1/close_tkwind.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(d_dmpmat.o):\
  $(d0root)/graphics_util/sgidi3/d_dmpmat.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/d_dmpmat.f ;\
	mv d_dmpmat.o $(SCRATCH)/a1/d_dmpmat.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(d_matcpy.o):\
  $(d0root)/graphics_util/sgidi3/d_matcpy.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/d_matcpy.f ;\
	mv d_matcpy.o $(SCRATCH)/a1/d_matcpy.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(d_matmul.o):\
  $(d0root)/graphics_util/sgidi3/d_matmul.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/d_matmul.f ;\
	mv d_matmul.o $(SCRATCH)/a1/d_matmul.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(d_matrot.o):\
  $(d0root)/graphics_util/sgidi3/d_matrot.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/d_matrot.f ;\
	mv d_matrot.o $(SCRATCH)/a1/d_matrot.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(d_matsca.o):\
  $(d0root)/graphics_util/sgidi3/d_matsca.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/d_matsca.f ;\
	mv d_matsca.o $(SCRATCH)/a1/d_matsca.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(d_mattra.o):\
  $(d0root)/graphics_util/sgidi3/d_mattra.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/d_mattra.f ;\
	mv d_mattra.o $(SCRATCH)/a1/d_mattra.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(d_matuni.o):\
  $(d0root)/graphics_util/sgidi3/d_matuni.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/d_matuni.f ;\
	mv d_matuni.o $(SCRATCH)/a1/d_matuni.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(dev_clear.o):\
  $(d0root)/graphics_util/sgidi3/dev_clear.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/dev_clear.f ;\
	mv dev_clear.o $(SCRATCH)/a1/dev_clear.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(dev_cndow717.o):\
  $(d0root)/graphics_util/sgidi3/dev_close_window.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/dev_close_window.f ;\
	mv dev_close_window.o $(SCRATCH)/a1/dev_cndow717.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(dev_color.o):\
  $(d0root)/graphics_util/sgidi3/dev_color.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/dev_color.f ;\
	mv dev_color.o $(SCRATCH)/a1/dev_color.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(dev_deflin.o):\
  $(d0root)/graphics_util/sgidi3/dev_deflin.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/dev_deflin.f ;\
	mv dev_deflin.o $(SCRATCH)/a1/dev_deflin.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(dev_draw.o):\
  $(d0root)/graphics_util/sgidi3/dev_draw.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/dev_draw.f ;\
	mv dev_draw.o $(SCRATCH)/a1/dev_draw.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(dev_force.o):\
  $(d0root)/graphics_util/sgidi3/dev_force.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/dev_force.f ;\
	mv dev_force.o $(SCRATCH)/a1/dev_force.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(dev_linewi.o):\
  $(d0root)/graphics_util/sgidi3/dev_linewi.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/dev_linewi.f ;\
	mv dev_linewi.o $(SCRATCH)/a1/dev_linewi.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(dev_mark.o):\
  $(d0root)/graphics_util/sgidi3/dev_mark.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/dev_mark.f ;\
	mv dev_mark.o $(SCRATCH)/a1/dev_mark.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(dev_move.o):\
  $(d0root)/graphics_util/sgidi3/dev_move.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/dev_move.f ;\
	mv dev_move.o $(SCRATCH)/a1/dev_move.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(dev_movscr.o):\
  $(d0root)/graphics_util/sgidi3/dev_movscr.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/dev_movscr.f ;\
	mv dev_movscr.o $(SCRATCH)/a1/dev_movscr.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(dev_ondow617.o):\
  $(d0root)/graphics_util/sgidi3/dev_open_window.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/dev_open_window.f ;\
	mv dev_open_window.o $(SCRATCH)/a1/dev_ondow617.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(dev_page.o):\
  $(d0root)/graphics_util/sgidi3/dev_page.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/dev_page.f ;\
	mv dev_page.o $(SCRATCH)/a2/dev_page.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(dev_plin.o):\
  $(d0root)/graphics_util/sgidi3/dev_plin.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/dev_plin.f ;\
	mv dev_plin.o $(SCRATCH)/a2/dev_plin.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(dev_polf.o):\
  $(d0root)/graphics_util/sgidi3/dev_polf.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/dev_polf.f ;\
	mv dev_polf.o $(SCRATCH)/a2/dev_polf.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(dev_poly.o):\
  $(d0root)/graphics_util/sgidi3/dev_poly.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/dev_poly.f ;\
	mv dev_poly.o $(SCRATCH)/a2/dev_poly.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(dev_rot.o):\
  $(d0root)/graphics_util/sgidi3/dev_rot.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/dev_rot.f ;\
	mv dev_rot.o $(SCRATCH)/a2/dev_rot.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(dev_scale.o):\
  $(d0root)/graphics_util/sgidi3/dev_scale.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/dev_scale.f ;\
	mv dev_scale.o $(SCRATCH)/a2/dev_scale.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(dev_setlin.o):\
  $(d0root)/graphics_util/sgidi3/dev_setlin.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/dev_setlin.f ;\
	mv dev_setlin.o $(SCRATCH)/a2/dev_setlin.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(dev_transf.o):\
  $(d0root)/graphics_util/sgidi3/dev_transf.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/dev_transf.f ;\
	mv dev_transf.o $(SCRATCH)/a2/dev_transf.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(dev_transl.o):\
  $(d0root)/graphics_util/sgidi3/dev_transl.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/dev_transl.f ;\
	mv dev_transl.o $(SCRATCH)/a2/dev_transl.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(dev_wners947.o):\
  $(d0root)/graphics_util/sgidi3/dev_window_corners.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/dev_window_corners.f ;\
	mv dev_window_corners.o $(SCRATCH)/a2/dev_wners947.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(dmpseg.o):\
  $(d0root)/graphics_util/sgidi3/dmpseg.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/dmpseg.f ;\
	mv dmpseg.o $(SCRATCH)/a2/dmpseg.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(hcpy_name374.o):\
  $(d0root)/graphics_util/sgidi3/hcpy_filename.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/hcpy_filename.f ;\
	mv hcpy_filename.o $(SCRATCH)/a2/hcpy_name374.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(hls_to_rgb.o):\
  $(d0root)/graphics_util/sgidi3/hls_to_rgb.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/hls_to_rgb.f ;\
	mv hls_to_rgb.o $(SCRATCH)/a2/hls_to_rgb.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(j_cross.o):\
  $(d0root)/graphics_util/sgidi3/j_cross.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/j_cross.f ;\
	mv j_cross.o $(SCRATCH)/a2/j_cross.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(j_delsegm.o):\
  $(d0root)/graphics_util/sgidi3/j_delsegm.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/j_delsegm.f ;\
	mv j_delsegm.o $(SCRATCH)/a2/j_delsegm.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(j_devtrn.o):\
  $(d0root)/graphics_util/sgidi3/j_devtrn.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/j_devtrn.f ;\
	mv j_devtrn.o $(SCRATCH)/a2/j_devtrn.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(j_dummies.o):\
  $(d0root)/graphics_util/sgidi3/j_dummies.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/j_dummies.f ;\
	mv j_dummies.o $(SCRATCH)/a2/j_dummies.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(j_dump.o):\
  $(d0root)/graphics_util/sgidi3/j_dump.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/j_dump.f ;\
	mv j_dump.o $(SCRATCH)/a2/j_dump.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(j_grey.o):\
  $(d0root)/graphics_util/sgidi3/j_grey.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/j_grey.f ;\
	mv j_grey.o $(SCRATCH)/a2/j_grey.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(j_hcoegin370.o):\
  $(d0root)/graphics_util/sgidi3/j_hcopy_begin.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/j_hcopy_begin.f ;\
	mv j_hcopy_begin.o $(SCRATCH)/a2/j_hcoegin370.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(j_hcovice477.o):\
  $(d0root)/graphics_util/sgidi3/j_hcopy_device.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/j_hcopy_device.f ;\
	mv j_hcopy_device.o $(SCRATCH)/a3/j_hcovice477.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(j_hcopy_end.o):\
  $(d0root)/graphics_util/sgidi3/j_hcopy_end.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/j_hcopy_end.f ;\
	mv j_hcopy_end.o $(SCRATCH)/a3/j_hcopy_end.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(j_menu3d.o):\
  $(d0root)/graphics_util/sgidi3/j_menu3d.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/j_menu3d.f ;\
	mv j_menu3d.o $(SCRATCH)/a3/j_menu3d.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(j_projs.o):\
  $(d0root)/graphics_util/sgidi3/j_projs.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/j_projs.f ;\
	mv j_projs.o $(SCRATCH)/a3/j_projs.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(just_ents429.o):\
  $(d0root)/graphics_util/sgidi3/just_comments.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/just_comments.f ;\
	mv just_comments.o $(SCRATCH)/a3/just_ents429.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(open_tkwind.o):\
  $(d0root)/graphics_util/sgidi3/open_tkwind.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/open_tkwind.f ;\
	mv open_tkwind.o $(SCRATCH)/a3/open_tkwind.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(polyn.o):\
  $(d0root)/graphics_util/sgidi3/polyn.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/polyn.f ;\
	mv polyn.o $(SCRATCH)/a3/polyn.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(ps_color.o):\
  $(d0root)/graphics_util/sgidi3/ps_color.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/ps_color.f ;\
	mv ps_color.o $(SCRATCH)/a3/ps_color.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(ps_crns.o):\
  $(d0root)/graphics_util/sgidi3/ps_crns.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/ps_crns.f ;\
	mv ps_crns.o $(SCRATCH)/a3/ps_crns.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(ps_init.o):\
  $(d0root)/graphics_util/sgidi3/ps_init.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/ps_init.f ;\
	mv ps_init.o $(SCRATCH)/a3/ps_init.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(ps_linewi.o):\
  $(d0root)/graphics_util/sgidi3/ps_linewi.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/ps_linewi.f ;\
	mv ps_linewi.o $(SCRATCH)/a3/ps_linewi.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(ps_move.o):\
  $(d0root)/graphics_util/sgidi3/ps_move.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/ps_move.f ;\
	mv ps_move.o $(SCRATCH)/a3/ps_move.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(ps_poly.o):\
  $(d0root)/graphics_util/sgidi3/ps_poly.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/ps_poly.f ;\
	mv ps_poly.o $(SCRATCH)/a3/ps_poly.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(ps_setlin.o):\
  $(d0root)/graphics_util/sgidi3/ps_setlin.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/ps_setlin.f ;\
	mv ps_setlin.o $(SCRATCH)/a3/ps_setlin.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(qm_color.o):\
  $(d0root)/graphics_util/sgidi3/qm_color.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/qm_color.f ;\
	mv qm_color.o $(SCRATCH)/a3/qm_color.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(qm_crns.o):\
  $(d0root)/graphics_util/sgidi3/qm_crns.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/qm_crns.f ;\
	mv qm_crns.o $(SCRATCH)/a3/qm_crns.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(qm_init.o):\
  $(d0root)/graphics_util/sgidi3/qm_init.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/qm_init.f ;\
	mv qm_init.o $(SCRATCH)/a3/qm_init.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(qm_move.o):\
  $(d0root)/graphics_util/sgidi3/qm_move.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/qm_move.f ;\
	mv qm_move.o $(SCRATCH)/a3/qm_move.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(qm_poly.o):\
  $(d0root)/graphics_util/sgidi3/qm_poly.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/qm_poly.f ;\
	mv qm_poly.o $(SCRATCH)/a3/qm_poly.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_j1iget.o):\
  $(d0root)/graphics_util/sgidi3/sgi_j1iget.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_j1iget.f ;\
	mv sgi_j1iget.o $(SCRATCH)/a3/sgi_j1iget.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_j1strg.o):\
  $(d0root)/graphics_util/sgidi3/sgi_j1strg.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_j1strg.f ;\
	mv sgi_j1strg.o $(SCRATCH)/a4/sgi_j1strg.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_j2strg.o):\
  $(d0root)/graphics_util/sgidi3/sgi_j2strg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_j2strg.f ;\
	mv sgi_j2strg.o $(SCRATCH)/a4/sgi_j2strg.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_j3draw.o):\
  $(d0root)/graphics_util/sgidi3/sgi_j3draw.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_j3draw.f ;\
	mv sgi_j3draw.o $(SCRATCH)/a4/sgi_j3draw.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_j3mark.o):\
  $(d0root)/graphics_util/sgidi3/sgi_j3mark.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_j3mark.f ;\
	mv sgi_j3mark.o $(SCRATCH)/a4/sgi_j3mark.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_j3move.o):\
  $(d0root)/graphics_util/sgidi3/sgi_j3move.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_j3move.f ;\
	mv sgi_j3move.o $(SCRATCH)/a4/sgi_j3move.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_j3plgn.o):\
  $(d0root)/graphics_util/sgidi3/sgi_j3plgn.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_j3plgn.f ;\
	mv sgi_j3plgn.o $(SCRATCH)/a4/sgi_j3plgn.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_j3poly.o):\
  $(d0root)/graphics_util/sgidi3/sgi_j3poly.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_j3poly.f ;\
	mv sgi_j3poly.o $(SCRATCH)/a4/sgi_j3poly.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_j3rget.o):\
  $(d0root)/graphics_util/sgidi3/sgi_j3rget.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_j3rget.f ;\
	mv sgi_j3rget.o $(SCRATCH)/a4/sgi_j3rget.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_j3strg.o):\
  $(d0root)/graphics_util/sgidi3/sgi_j3strg.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc\
  $(d0library)/inc/vaxfont.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_j3strg.f ;\
	mv sgi_j3strg.o $(SCRATCH)/a4/sgi_j3strg.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_j4rget.o):\
  $(d0root)/graphics_util/sgidi3/sgi_j4rget.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_j4rget.f ;\
	mv sgi_j4rget.o $(SCRATCH)/a4/sgi_j4rget.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jaload.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jaload.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jaload.f ;\
	mv sgi_jaload.o $(SCRATCH)/a4/sgi_jaload.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jarc.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jarc.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jarc.f ;\
	mv sgi_jarc.o $(SCRATCH)/a4/sgi_jarc.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jarset.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jarset.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jarset.f ;\
	mv sgi_jarset.o $(SCRATCH)/a4/sgi_jarset.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jasave.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jasave.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jasave.f ;\
	mv sgi_jasave.o $(SCRATCH)/a4/sgi_jasave.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jaspek.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jaspek.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jaspek.f ;\
	mv sgi_jaspek.o $(SCRATCH)/a4/sgi_jaspek.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jattrb.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jattrb.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jattrb.f ;\
	mv sgi_jattrb.o $(SCRATCH)/a4/sgi_jattrb.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jbackg.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jbackg.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jbackg.f ;\
	mv sgi_jbackg.o $(SCRATCH)/a4/sgi_jbackg.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jbase.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jbase.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jbase.f ;\
	mv sgi_jbase.o $(SCRATCH)/a4/sgi_jbase.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jbegin.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jbegin.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jbegin.f ;\
	mv sgi_jbegin.o $(SCRATCH)/a4/sgi_jbegin.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jbgbat.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jbgbat.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jbgbat.f ;\
	mv sgi_jbgbat.o $(SCRATCH)/a4/sgi_jbgbat.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jbuild.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jbuild.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jbuild.f ;\
	mv sgi_jbuild.o $(SCRATCH)/a5/sgi_jbuild.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jcircl.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jcircl.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jcircl.f ;\
	mv sgi_jcircl.o $(SCRATCH)/a5/sgi_jcircl.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jclear.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jclear.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jclear.f ;\
	mv sgi_jclear.o $(SCRATCH)/a5/sgi_jclear.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jclose.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jclose.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jclose.f ;\
	mv sgi_jclose.o $(SCRATCH)/a5/sgi_jclose.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jcmark.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jcmark.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jcmark.f ;\
	mv sgi_jcmark.o $(SCRATCH)/a5/sgi_jcmark.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jcolor.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jcolor.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jcolor.f ;\
	mv sgi_jcolor.o $(SCRATCH)/a5/sgi_jcolor.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jconpk.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jconpk.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jconpk.f ;\
	mv sgi_jconpk.o $(SCRATCH)/a5/sgi_jconpk.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jconvw.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jconvw.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jconvw.f ;\
	mv sgi_jconvw.o $(SCRATCH)/a5/sgi_jconvw.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jconwv.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jconwv.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jconwv.f ;\
	mv sgi_jconwv.o $(SCRATCH)/a5/sgi_jconwv.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jcotbl.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jcotbl.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jcotbl.f ;\
	mv sgi_jcotbl.o $(SCRATCH)/a5/sgi_jcotbl.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jcview.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jcview.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jcview.f ;\
	mv sgi_jcview.o $(SCRATCH)/a5/sgi_jcview.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jdbase.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdbase.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jdbase.f ;\
	mv sgi_jdbase.o $(SCRATCH)/a5/sgi_jdbase.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jdcolr.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdcolr.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jdcolr.f ;\
	mv sgi_jdcolr.o $(SCRATCH)/a5/sgi_jdcolr.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jdd3d.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdd3d.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jdd3d.f ;\
	mv sgi_jdd3d.o $(SCRATCH)/a5/sgi_jdd3d.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jddete.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jddete.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jddete.f ;\
	mv sgi_jddete.o $(SCRATCH)/a5/sgi_jddete.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jdend.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdend.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jdend.f ;\
	mv sgi_jdend.o $(SCRATCH)/a5/sgi_jdend.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jdetec.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdetec.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jdetec.f ;\
	mv sgi_jdetec.o $(SCRATCH)/a5/sgi_jdetec.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jdevof.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdevof.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jdevof.f ;\
	mv sgi_jdevof.o $(SCRATCH)/a5/sgi_jdevof.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jdevon.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdevon.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jdevon.f ;\
	mv sgi_jdevon.o $(SCRATCH)/a5/sgi_jdevon.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jdevvp.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdevvp.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jdevvp.f ;\
	mv sgi_jdevvp.o $(SCRATCH)/a5/sgi_jdevvp.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jdevwn.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdevwn.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jdevwn.f ;\
	mv sgi_jdevwn.o $(SCRATCH)/a6/sgi_jdevwn.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jdfont.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdfont.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jdfont.f ;\
	mv sgi_jdfont.o $(SCRATCH)/a6/sgi_jdfont.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jdinit.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdinit.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jdinit.f ;\
	mv sgi_jdinit.o $(SCRATCH)/a6/sgi_jdinit.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jdjust.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdjust.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jdjust.f ;\
	mv sgi_jdjust.o $(SCRATCH)/a6/sgi_jdjust.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jdlsty.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdlsty.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jdlsty.f ;\
	mv sgi_jdlsty.o $(SCRATCH)/a6/sgi_jdlsty.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jdlwid.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdlwid.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jdlwid.f ;\
	mv sgi_jdlwid.o $(SCRATCH)/a6/sgi_jdlwid.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jdmark.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdmark.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jdmark.f ;\
	mv sgi_jdmark.o $(SCRATCH)/a6/sgi_jdmark.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jdpidx.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdpidx.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jdpidx.f ;\
	mv sgi_jdpidx.o $(SCRATCH)/a6/sgi_jdpidx.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jdpint.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdpint.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jdpint.f ;\
	mv sgi_jdpint.o $(SCRATCH)/a6/sgi_jdpint.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jdpinx.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdpinx.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jdpinx.f ;\
	mv sgi_jdpinx.o $(SCRATCH)/a6/sgi_jdpinx.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jdpkid.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdpkid.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jdpkid.f ;\
	mv sgi_jdpkid.o $(SCRATCH)/a6/sgi_jdpkid.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jdraw.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdraw.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jdraw.f ;\
	mv sgi_jdraw.o $(SCRATCH)/a6/sgi_jdraw.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jdsize.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdsize.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jdsize.f ;\
	mv sgi_jdsize.o $(SCRATCH)/a6/sgi_jdsize.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jdvisb.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdvisb.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jdvisb.f ;\
	mv sgi_jdvisb.o $(SCRATCH)/a6/sgi_jdvisb.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jelseg.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jelseg.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jelseg.f ;\
	mv sgi_jelseg.o $(SCRATCH)/a6/sgi_jelseg.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jenbat.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jenbat.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jenbat.f ;\
	mv sgi_jenbat.o $(SCRATCH)/a6/sgi_jenbat.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jend.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jend.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jend.f ;\
	mv sgi_jend.o $(SCRATCH)/a6/sgi_jend.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jepseg.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jepseg.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jepseg.f ;\
	mv sgi_jepseg.o $(SCRATCH)/a6/sgi_jepseg.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jescap.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jescap.f\
  $(d0library)/params/escape_code_names.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jescap.f ;\
	mv sgi_jescap.o $(SCRATCH)/a6/sgi_jescap.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jextnt.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jextnt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jextnt.f ;\
	mv sgi_jextnt.o $(SCRATCH)/a6/sgi_jextnt.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jfiles.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jfiles.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jfiles.f ;\
	mv sgi_jfiles.o $(SCRATCH)/a7/sgi_jfiles.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jfont.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jfont.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jfont.f ;\
	mv sgi_jfont.o $(SCRATCH)/a7/sgi_jfont.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jframe.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jframe.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jframe.f ;\
	mv sgi_jframe.o $(SCRATCH)/a7/sgi_jframe.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jfsopn.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jfsopn.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jfsopn.f ;\
	mv sgi_jfsopn.o $(SCRATCH)/a7/sgi_jfsopn.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jhclip.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jhclip.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jhclip.f ;\
	mv sgi_jhclip.o $(SCRATCH)/a7/sgi_jhclip.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jhcpy.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jhcpy.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jhcpy.f ;\
	mv sgi_jhcpy.o $(SCRATCH)/a7/sgi_jhcpy.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jhilit.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jhilit.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jhilit.f ;\
	mv sgi_jhilit.o $(SCRATCH)/a7/sgi_jhilit.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jhithr.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jhithr.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jhithr.f ;\
	mv sgi_jhithr.o $(SCRATCH)/a7/sgi_jhithr.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jhstrg.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jhstrg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jhstrg.f ;\
	mv sgi_jhstrg.o $(SCRATCH)/a7/sgi_jhstrg.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jidisa.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jidisa.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jidisa.f ;\
	mv sgi_jidisa.o $(SCRATCH)/a7/sgi_jidisa.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jienab.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jienab.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jienab.f ;\
	mv sgi_jienab.o $(SCRATCH)/a7/sgi_jienab.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jiescp.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jiescp.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jiescp.f ;\
	mv sgi_jiescp.o $(SCRATCH)/a7/sgi_jiescp.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jinten.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jinten.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jinten.f ;\
	mv sgi_jinten.o $(SCRATCH)/a7/sgi_jinten.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jiqddl.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jiqddl.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jiqddl.f ;\
	mv sgi_jiqddl.o $(SCRATCH)/a7/sgi_jiqddl.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jiqdev.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jiqdev.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jiqdev.f ;\
	mv sgi_jiqdev.o $(SCRATCH)/a7/sgi_jiqdev.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jiqdil.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jiqdil.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jiqdil.f ;\
	mv sgi_jiqdil.o $(SCRATCH)/a7/sgi_jiqdil.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jiqdim.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jiqdim.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jiqdim.f ;\
	mv sgi_jiqdim.o $(SCRATCH)/a7/sgi_jiqdim.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jiqtxt.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jiqtxt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jiqtxt.f ;\
	mv sgi_jiqtxt.o $(SCRATCH)/a7/sgi_jiqtxt.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jisgmt.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jisgmt.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jisgmt.f ;\
	mv sgi_jisgmt.o $(SCRATCH)/a7/sgi_jisgmt.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jiwind.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jiwind.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jiwind.f ;\
	mv sgi_jiwind.o $(SCRATCH)/a7/sgi_jiwind.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jjust.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jjust.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jjust.f ;\
	mv sgi_jjust.o $(SCRATCH)/a8/sgi_jjust.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jkeybs.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jkeybs.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jkeybs.f ;\
	mv sgi_jkeybs.o $(SCRATCH)/a8/sgi_jkeybs.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jlocat.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jlocat.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jlocat.f ;\
	mv sgi_jlocat.o $(SCRATCH)/a8/sgi_jlocat.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jlstyl.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jlstyl.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jlstyl.f ;\
	mv sgi_jlstyl.o $(SCRATCH)/a8/sgi_jlstyl.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jlwide.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jlwide.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jlwide.f ;\
	mv sgi_jlwide.o $(SCRATCH)/a8/sgi_jlwide.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jmark.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jmark.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jmark.f ;\
	mv sgi_jmark.o $(SCRATCH)/a8/sgi_jmark.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jmodel.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jmodel.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jmodel.f ;\
	mv sgi_jmodel.o $(SCRATCH)/a8/sgi_jmodel.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jmodon.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jmodon.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jmodon.f ;\
	mv sgi_jmodon.o $(SCRATCH)/a8/sgi_jmodon.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jmove.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jmove.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jmove.f ;\
	mv sgi_jmove.o $(SCRATCH)/a8/sgi_jmove.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jmstrg.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jmstrg.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jmstrg.f ;\
	mv sgi_jmstrg.o $(SCRATCH)/a8/sgi_jmstrg.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jnorml.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jnorml.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jnorml.f ;\
	mv sgi_jnorml.o $(SCRATCH)/a8/sgi_jnorml.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jopen.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jopen.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jopen.f ;\
	mv sgi_jopen.o $(SCRATCH)/a8/sgi_jopen.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jp3mrk.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jp3mrk.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jp3mrk.f ;\
	mv sgi_jp3mrk.o $(SCRATCH)/a8/sgi_jp3mrk.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jparal.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jparal.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jparal.f ;\
	mv sgi_jparal.o $(SCRATCH)/a8/sgi_jparal.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jparob.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jparob.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jparob.f ;\
	mv sgi_jparob.o $(SCRATCH)/a8/sgi_jparob.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jpause.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpause.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jpause.f ;\
	mv sgi_jpause.o $(SCRATCH)/a8/sgi_jpause.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jpecho.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpecho.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jpecho.f ;\
	mv sgi_jpecho.o $(SCRATCH)/a8/sgi_jpecho.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jpedge.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpedge.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jpedge.f ;\
	mv sgi_jpedge.o $(SCRATCH)/a8/sgi_jpedge.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jperob.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jperob.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jperob.f ;\
	mv sgi_jperob.o $(SCRATCH)/a8/sgi_jperob.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jpersp.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpersp.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jpersp.f ;\
	mv sgi_jpersp.o $(SCRATCH)/a8/sgi_jpersp.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jpick.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpick.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jpick.f ;\
	mv sgi_jpick.o $(SCRATCH)/a9/sgi_jpick.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jpidex.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpidex.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jpidex.f ;\
	mv sgi_jpidex.o $(SCRATCH)/a9/sgi_jpidex.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jpintr.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpintr.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jpintr.f ;\
	mv sgi_jpintr.o $(SCRATCH)/a9/sgi_jpintr.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jpkapr.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpkapr.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jpkapr.f ;\
	mv sgi_jpkapr.o $(SCRATCH)/a9/sgi_jpkapr.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jpkid.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpkid.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jpkid.f ;\
	mv sgi_jpkid.o $(SCRATCH)/a9/sgi_jpkid.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jplane.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jplane.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jplane.f ;\
	mv sgi_jplane.o $(SCRATCH)/a9/sgi_jplane.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jpmark.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpmark.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jpmark.f ;\
	mv sgi_jpmark.o $(SCRATCH)/a9/sgi_jpmark.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jpolgn.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpolgn.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jpolgn.f ;\
	mv sgi_jpolgn.o $(SCRATCH)/a9/sgi_jpolgn.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jpoly.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpoly.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jpoly.f ;\
	mv sgi_jpoly.o $(SCRATCH)/a9/sgi_jpoly.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jpr3mr.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpr3mr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jpr3mr.f ;\
	mv sgi_jpr3mr.o $(SCRATCH)/a9/sgi_jpr3mr.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jprmrk.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jprmrk.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jprmrk.f ;\
	mv sgi_jprmrk.o $(SCRATCH)/a9/sgi_jprmrk.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jpurge.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpurge.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jpurge.f ;\
	mv sgi_jpurge.o $(SCRATCH)/a9/sgi_jpurge.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jr3dra.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jr3dra.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jr3dra.f ;\
	mv sgi_jr3dra.o $(SCRATCH)/a9/sgi_jr3dra.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jr3mov.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jr3mov.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jr3mov.f ;\
	mv sgi_jr3mov.o $(SCRATCH)/a9/sgi_jr3mov.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jr3mrk.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jr3mrk.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jr3mrk.f ;\
	mv sgi_jr3mrk.o $(SCRATCH)/a9/sgi_jr3mrk.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jr3pgn.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jr3pgn.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jr3pgn.f ;\
	mv sgi_jr3pgn.o $(SCRATCH)/a9/sgi_jr3pgn.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jr3ply.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jr3ply.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jr3ply.f ;\
	mv sgi_jr3ply.o $(SCRATCH)/a9/sgi_jr3ply.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jrclos.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jrclos.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jrclos.f ;\
	mv sgi_jrclos.o $(SCRATCH)/a9/sgi_jrclos.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jrdraw.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jrdraw.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jrdraw.f ;\
	mv sgi_jrdraw.o $(SCRATCH)/a9/sgi_jrdraw.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jrect.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jrect.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jrect.f ;\
	mv sgi_jrect.o $(SCRATCH)/a9/sgi_jrect.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jright.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jright.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jright.f ;\
	mv sgi_jright.o $(SCRATCH)/a10/sgi_jright.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jrmark.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jrmark.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jrmark.f ;\
	mv sgi_jrmark.o $(SCRATCH)/a10/sgi_jrmark.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jrmove.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jrmove.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jrmove.f ;\
	mv sgi_jrmove.o $(SCRATCH)/a10/sgi_jrmove.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jropen.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jropen.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jropen.f ;\
	mv sgi_jropen.o $(SCRATCH)/a10/sgi_jropen.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jrplgn.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jrplgn.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jrplgn.f ;\
	mv sgi_jrplgn.o $(SCRATCH)/a10/sgi_jrplgn.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jrpoly.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jrpoly.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jrpoly.f ;\
	mv sgi_jrpoly.o $(SCRATCH)/a10/sgi_jrpoly.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jrrect.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jrrect.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jrrect.f ;\
	mv sgi_jrrect.o $(SCRATCH)/a10/sgi_jrrect.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jsaall.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jsaall.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jsaall.f ;\
	mv sgi_jsaall.o $(SCRATCH)/a10/sgi_jsaall.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jsasso.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jsasso.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jsasso.f ;\
	mv sgi_jsasso.o $(SCRATCH)/a10/sgi_jsasso.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jsectr.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jsectr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jsectr.f ;\
	mv sgi_jsectr.o $(SCRATCH)/a10/sgi_jsectr.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jsetdb.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jsetdb.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jsetdb.f ;\
	mv sgi_jsetdb.o $(SCRATCH)/a10/sgi_jsetdb.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jseter.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jseter.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jseter.f ;\
	mv sgi_jseter.o $(SCRATCH)/a10/sgi_jseter.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jsgpri.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jsgpri.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jsgpri.f ;\
	mv sgi_jsgpri.o $(SCRATCH)/a10/sgi_jsgpri.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jsize.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jsize.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jsize.f ;\
	mv sgi_jsize.o $(SCRATCH)/a10/sgi_jsize.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jspick.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jspick.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jspick.f ;\
	mv sgi_jspick.o $(SCRATCH)/a10/sgi_jspick.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jsview.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jsview.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jsview.f ;\
	mv sgi_jsview.o $(SCRATCH)/a10/sgi_jsview.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jt2all.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jt2all.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jt2all.f ;\
	mv sgi_jt2all.o $(SCRATCH)/a10/sgi_jt2all.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jtrans.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jtrans.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jtrans.f ;\
	mv sgi_jtrans.o $(SCRATCH)/a10/sgi_jtrans.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jttype.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jttype.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jttype.f ;\
	mv sgi_jttype.o $(SCRATCH)/a10/sgi_jttype.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jupvec.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jupvec.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jupvec.f ;\
	mv sgi_jupvec.o $(SCRATCH)/a10/sgi_jupvec.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jvisbl.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jvisbl.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jvisbl.f ;\
	mv sgi_jvisbl.o $(SCRATCH)/a11/sgi_jvisbl.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jvload.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jvload.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jvload.f ;\
	mv sgi_jvload.o $(SCRATCH)/a11/sgi_jvload.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jvport.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jvport.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jvport.f ;\
	mv sgi_jvport.o $(SCRATCH)/a11/sgi_jvport.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jvsave.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jvsave.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jvsave.f ;\
	mv sgi_jvsave.o $(SCRATCH)/a11/sgi_jvsave.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jvspac.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jvspac.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jvspac.f ;\
	mv sgi_jvspac.o $(SCRATCH)/a11/sgi_jvspac.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jvupln.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jvupln.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jvupln.f ;\
	mv sgi_jvupln.o $(SCRATCH)/a11/sgi_jvupln.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jvupnt.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jvupnt.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jvupnt.f ;\
	mv sgi_jvupnt.o $(SCRATCH)/a11/sgi_jvupnt.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jwclip.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jwclip.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jwclip.f ;\
	mv sgi_jwclip.o $(SCRATCH)/a11/sgi_jwclip.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jwindo.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jwindo.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jwindo.f ;\
	mv sgi_jwindo.o $(SCRATCH)/a11/sgi_jwindo.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jyclip.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jyclip.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jyclip.f ;\
	mv sgi_jyclip.o $(SCRATCH)/a11/sgi_jyclip.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_jyon.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jyon.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_jyon.f ;\
	mv sgi_jyon.o $(SCRATCH)/a11/sgi_jyon.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(sgi_re_3d329.o):\
  $(d0root)/graphics_util/sgidi3/sgi_rotate_3d.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pxpara.inc\
  $(d0library)/inc/hdrinx.inc\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/sgi_rotate_3d.f ;\
	mv sgi_rotate_3d.o $(SCRATCH)/a11/sgi_re_3d329.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(tk_clear.o):\
  $(d0root)/graphics_util/sgidi3/tk_clear.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/tk_clear.f ;\
	mv tk_clear.o $(SCRATCH)/a11/tk_clear.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(tk_color.o):\
  $(d0root)/graphics_util/sgidi3/tk_color.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/tk_color.f ;\
	mv tk_color.o $(SCRATCH)/a11/tk_color.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(tk_crns.o):\
  $(d0root)/graphics_util/sgidi3/tk_crns.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/tk_crns.f ;\
	mv tk_crns.o $(SCRATCH)/a11/tk_crns.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(tk_drascr.o):\
  $(d0root)/graphics_util/sgidi3/tk_drascr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/tk_drascr.f ;\
	mv tk_drascr.o $(SCRATCH)/a11/tk_drascr.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(tk_draw.o):\
  $(d0root)/graphics_util/sgidi3/tk_draw.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/tk_draw.f ;\
	mv tk_draw.o $(SCRATCH)/a11/tk_draw.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(tk_fin.o):\
  $(d0root)/graphics_util/sgidi3/tk_fin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/tk_fin.f ;\
	mv tk_fin.o $(SCRATCH)/a11/tk_fin.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(tk_force.o):\
  $(d0root)/graphics_util/sgidi3/tk_force.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/tk_force.f ;\
	mv tk_force.o $(SCRATCH)/a11/tk_force.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(tk_line2.o):\
  $(d0root)/graphics_util/sgidi3/tk_line2.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/tk_line2.f ;\
	mv tk_line2.o $(SCRATCH)/a11/tk_line2.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(tk_move.o):\
  $(d0root)/graphics_util/sgidi3/tk_move.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/tk_move.f ;\
	mv tk_move.o $(SCRATCH)/a12/tk_move.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(tk_movscr.o):\
  $(d0root)/graphics_util/sgidi3/tk_movscr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/tk_movscr.f ;\
	mv tk_movscr.o $(SCRATCH)/a12/tk_movscr.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(tk_out.o):\
  $(d0root)/graphics_util/sgidi3/tk_out.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/tk_out.f ;\
	mv tk_out.o $(SCRATCH)/a12/tk_out.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(tk_poly.o):\
  $(d0root)/graphics_util/sgidi3/tk_poly.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/tk_poly.f ;\
	mv tk_poly.o $(SCRATCH)/a12/tk_poly.o ;\
	)
$(d0root)/graphics_util/deb_sgidi3.a(tk_text.o):\
  $(d0root)/graphics_util/sgidi3/tk_text.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgidi3/tk_text.f ;\
	mv tk_text.o $(SCRATCH)/a12/tk_text.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(all_dixie814.o):\
  $(d0root)/graphics_util/sgidi3/all_display_pixie.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/all_display_pixie.f ;\
	mv all_display_pixie.o $(SCRATCH)/a1/all_dixie814.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(close_tkwind.o):\
  $(d0root)/graphics_util/sgidi3/close_tkwind.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/close_tkwind.f ;\
	mv close_tkwind.o $(SCRATCH)/a1/close_tkwind.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(d_dmpmat.o):\
  $(d0root)/graphics_util/sgidi3/d_dmpmat.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/d_dmpmat.f ;\
	mv d_dmpmat.o $(SCRATCH)/a1/d_dmpmat.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(d_matcpy.o):\
  $(d0root)/graphics_util/sgidi3/d_matcpy.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/d_matcpy.f ;\
	mv d_matcpy.o $(SCRATCH)/a1/d_matcpy.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(d_matmul.o):\
  $(d0root)/graphics_util/sgidi3/d_matmul.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/d_matmul.f ;\
	mv d_matmul.o $(SCRATCH)/a1/d_matmul.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(d_matrot.o):\
  $(d0root)/graphics_util/sgidi3/d_matrot.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/d_matrot.f ;\
	mv d_matrot.o $(SCRATCH)/a1/d_matrot.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(d_matsca.o):\
  $(d0root)/graphics_util/sgidi3/d_matsca.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/d_matsca.f ;\
	mv d_matsca.o $(SCRATCH)/a1/d_matsca.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(d_mattra.o):\
  $(d0root)/graphics_util/sgidi3/d_mattra.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/d_mattra.f ;\
	mv d_mattra.o $(SCRATCH)/a1/d_mattra.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(d_matuni.o):\
  $(d0root)/graphics_util/sgidi3/d_matuni.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/d_matuni.f ;\
	mv d_matuni.o $(SCRATCH)/a1/d_matuni.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(dev_clear.o):\
  $(d0root)/graphics_util/sgidi3/dev_clear.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/dev_clear.f ;\
	mv dev_clear.o $(SCRATCH)/a1/dev_clear.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(dev_cndow717.o):\
  $(d0root)/graphics_util/sgidi3/dev_close_window.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/dev_close_window.f ;\
	mv dev_close_window.o $(SCRATCH)/a1/dev_cndow717.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(dev_color.o):\
  $(d0root)/graphics_util/sgidi3/dev_color.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/dev_color.f ;\
	mv dev_color.o $(SCRATCH)/a1/dev_color.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(dev_deflin.o):\
  $(d0root)/graphics_util/sgidi3/dev_deflin.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/dev_deflin.f ;\
	mv dev_deflin.o $(SCRATCH)/a1/dev_deflin.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(dev_draw.o):\
  $(d0root)/graphics_util/sgidi3/dev_draw.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/dev_draw.f ;\
	mv dev_draw.o $(SCRATCH)/a1/dev_draw.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(dev_force.o):\
  $(d0root)/graphics_util/sgidi3/dev_force.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/dev_force.f ;\
	mv dev_force.o $(SCRATCH)/a1/dev_force.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(dev_linewi.o):\
  $(d0root)/graphics_util/sgidi3/dev_linewi.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/dev_linewi.f ;\
	mv dev_linewi.o $(SCRATCH)/a1/dev_linewi.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(dev_mark.o):\
  $(d0root)/graphics_util/sgidi3/dev_mark.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/dev_mark.f ;\
	mv dev_mark.o $(SCRATCH)/a1/dev_mark.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(dev_move.o):\
  $(d0root)/graphics_util/sgidi3/dev_move.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/dev_move.f ;\
	mv dev_move.o $(SCRATCH)/a1/dev_move.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(dev_movscr.o):\
  $(d0root)/graphics_util/sgidi3/dev_movscr.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/dev_movscr.f ;\
	mv dev_movscr.o $(SCRATCH)/a1/dev_movscr.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(dev_ondow617.o):\
  $(d0root)/graphics_util/sgidi3/dev_open_window.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/dev_open_window.f ;\
	mv dev_open_window.o $(SCRATCH)/a1/dev_ondow617.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(dev_page.o):\
  $(d0root)/graphics_util/sgidi3/dev_page.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/dev_page.f ;\
	mv dev_page.o $(SCRATCH)/a2/dev_page.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(dev_plin.o):\
  $(d0root)/graphics_util/sgidi3/dev_plin.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/dev_plin.f ;\
	mv dev_plin.o $(SCRATCH)/a2/dev_plin.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(dev_polf.o):\
  $(d0root)/graphics_util/sgidi3/dev_polf.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/dev_polf.f ;\
	mv dev_polf.o $(SCRATCH)/a2/dev_polf.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(dev_poly.o):\
  $(d0root)/graphics_util/sgidi3/dev_poly.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/dev_poly.f ;\
	mv dev_poly.o $(SCRATCH)/a2/dev_poly.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(dev_rot.o):\
  $(d0root)/graphics_util/sgidi3/dev_rot.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/dev_rot.f ;\
	mv dev_rot.o $(SCRATCH)/a2/dev_rot.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(dev_scale.o):\
  $(d0root)/graphics_util/sgidi3/dev_scale.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/dev_scale.f ;\
	mv dev_scale.o $(SCRATCH)/a2/dev_scale.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(dev_setlin.o):\
  $(d0root)/graphics_util/sgidi3/dev_setlin.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/dev_setlin.f ;\
	mv dev_setlin.o $(SCRATCH)/a2/dev_setlin.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(dev_transf.o):\
  $(d0root)/graphics_util/sgidi3/dev_transf.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/dev_transf.f ;\
	mv dev_transf.o $(SCRATCH)/a2/dev_transf.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(dev_transl.o):\
  $(d0root)/graphics_util/sgidi3/dev_transl.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/dev_transl.f ;\
	mv dev_transl.o $(SCRATCH)/a2/dev_transl.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(dev_wners947.o):\
  $(d0root)/graphics_util/sgidi3/dev_window_corners.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/dev_window_corners.f ;\
	mv dev_window_corners.o $(SCRATCH)/a2/dev_wners947.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(dmpseg.o):\
  $(d0root)/graphics_util/sgidi3/dmpseg.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/dmpseg.f ;\
	mv dmpseg.o $(SCRATCH)/a2/dmpseg.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(hcpy_name374.o):\
  $(d0root)/graphics_util/sgidi3/hcpy_filename.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/hcpy_filename.f ;\
	mv hcpy_filename.o $(SCRATCH)/a2/hcpy_name374.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(hls_to_rgb.o):\
  $(d0root)/graphics_util/sgidi3/hls_to_rgb.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/hls_to_rgb.f ;\
	mv hls_to_rgb.o $(SCRATCH)/a2/hls_to_rgb.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(j_cross.o):\
  $(d0root)/graphics_util/sgidi3/j_cross.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/j_cross.f ;\
	mv j_cross.o $(SCRATCH)/a2/j_cross.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(j_delsegm.o):\
  $(d0root)/graphics_util/sgidi3/j_delsegm.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/j_delsegm.f ;\
	mv j_delsegm.o $(SCRATCH)/a2/j_delsegm.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(j_devtrn.o):\
  $(d0root)/graphics_util/sgidi3/j_devtrn.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/j_devtrn.f ;\
	mv j_devtrn.o $(SCRATCH)/a2/j_devtrn.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(j_dummies.o):\
  $(d0root)/graphics_util/sgidi3/j_dummies.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/j_dummies.f ;\
	mv j_dummies.o $(SCRATCH)/a2/j_dummies.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(j_dump.o):\
  $(d0root)/graphics_util/sgidi3/j_dump.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/j_dump.f ;\
	mv j_dump.o $(SCRATCH)/a2/j_dump.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(j_grey.o):\
  $(d0root)/graphics_util/sgidi3/j_grey.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/j_grey.f ;\
	mv j_grey.o $(SCRATCH)/a2/j_grey.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(j_hcoegin370.o):\
  $(d0root)/graphics_util/sgidi3/j_hcopy_begin.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/j_hcopy_begin.f ;\
	mv j_hcopy_begin.o $(SCRATCH)/a2/j_hcoegin370.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(j_hcovice477.o):\
  $(d0root)/graphics_util/sgidi3/j_hcopy_device.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/j_hcopy_device.f ;\
	mv j_hcopy_device.o $(SCRATCH)/a3/j_hcovice477.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(j_hcopy_end.o):\
  $(d0root)/graphics_util/sgidi3/j_hcopy_end.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/j_hcopy_end.f ;\
	mv j_hcopy_end.o $(SCRATCH)/a3/j_hcopy_end.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(j_menu3d.o):\
  $(d0root)/graphics_util/sgidi3/j_menu3d.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/j_menu3d.f ;\
	mv j_menu3d.o $(SCRATCH)/a3/j_menu3d.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(j_projs.o):\
  $(d0root)/graphics_util/sgidi3/j_projs.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/j_projs.f ;\
	mv j_projs.o $(SCRATCH)/a3/j_projs.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(just_ents429.o):\
  $(d0root)/graphics_util/sgidi3/just_comments.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/just_comments.f ;\
	mv just_comments.o $(SCRATCH)/a3/just_ents429.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(open_tkwind.o):\
  $(d0root)/graphics_util/sgidi3/open_tkwind.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/open_tkwind.f ;\
	mv open_tkwind.o $(SCRATCH)/a3/open_tkwind.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(polyn.o):\
  $(d0root)/graphics_util/sgidi3/polyn.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/polyn.f ;\
	mv polyn.o $(SCRATCH)/a3/polyn.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(ps_color.o):\
  $(d0root)/graphics_util/sgidi3/ps_color.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/ps_color.f ;\
	mv ps_color.o $(SCRATCH)/a3/ps_color.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(ps_crns.o):\
  $(d0root)/graphics_util/sgidi3/ps_crns.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/ps_crns.f ;\
	mv ps_crns.o $(SCRATCH)/a3/ps_crns.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(ps_init.o):\
  $(d0root)/graphics_util/sgidi3/ps_init.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/ps_init.f ;\
	mv ps_init.o $(SCRATCH)/a3/ps_init.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(ps_linewi.o):\
  $(d0root)/graphics_util/sgidi3/ps_linewi.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/ps_linewi.f ;\
	mv ps_linewi.o $(SCRATCH)/a3/ps_linewi.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(ps_move.o):\
  $(d0root)/graphics_util/sgidi3/ps_move.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/ps_move.f ;\
	mv ps_move.o $(SCRATCH)/a3/ps_move.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(ps_poly.o):\
  $(d0root)/graphics_util/sgidi3/ps_poly.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/ps_poly.f ;\
	mv ps_poly.o $(SCRATCH)/a3/ps_poly.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(ps_setlin.o):\
  $(d0root)/graphics_util/sgidi3/ps_setlin.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/ps_setlin.f ;\
	mv ps_setlin.o $(SCRATCH)/a3/ps_setlin.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(qm_color.o):\
  $(d0root)/graphics_util/sgidi3/qm_color.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/qm_color.f ;\
	mv qm_color.o $(SCRATCH)/a3/qm_color.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(qm_crns.o):\
  $(d0root)/graphics_util/sgidi3/qm_crns.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/qm_crns.f ;\
	mv qm_crns.o $(SCRATCH)/a3/qm_crns.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(qm_init.o):\
  $(d0root)/graphics_util/sgidi3/qm_init.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/qm_init.f ;\
	mv qm_init.o $(SCRATCH)/a3/qm_init.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(qm_move.o):\
  $(d0root)/graphics_util/sgidi3/qm_move.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/qm_move.f ;\
	mv qm_move.o $(SCRATCH)/a3/qm_move.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(qm_poly.o):\
  $(d0root)/graphics_util/sgidi3/qm_poly.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/qm_poly.f ;\
	mv qm_poly.o $(SCRATCH)/a3/qm_poly.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_j1iget.o):\
  $(d0root)/graphics_util/sgidi3/sgi_j1iget.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_j1iget.f ;\
	mv sgi_j1iget.o $(SCRATCH)/a3/sgi_j1iget.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_j1strg.o):\
  $(d0root)/graphics_util/sgidi3/sgi_j1strg.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_j1strg.f ;\
	mv sgi_j1strg.o $(SCRATCH)/a4/sgi_j1strg.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_j2strg.o):\
  $(d0root)/graphics_util/sgidi3/sgi_j2strg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_j2strg.f ;\
	mv sgi_j2strg.o $(SCRATCH)/a4/sgi_j2strg.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_j3draw.o):\
  $(d0root)/graphics_util/sgidi3/sgi_j3draw.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_j3draw.f ;\
	mv sgi_j3draw.o $(SCRATCH)/a4/sgi_j3draw.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_j3mark.o):\
  $(d0root)/graphics_util/sgidi3/sgi_j3mark.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_j3mark.f ;\
	mv sgi_j3mark.o $(SCRATCH)/a4/sgi_j3mark.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_j3move.o):\
  $(d0root)/graphics_util/sgidi3/sgi_j3move.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_j3move.f ;\
	mv sgi_j3move.o $(SCRATCH)/a4/sgi_j3move.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_j3plgn.o):\
  $(d0root)/graphics_util/sgidi3/sgi_j3plgn.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_j3plgn.f ;\
	mv sgi_j3plgn.o $(SCRATCH)/a4/sgi_j3plgn.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_j3poly.o):\
  $(d0root)/graphics_util/sgidi3/sgi_j3poly.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_j3poly.f ;\
	mv sgi_j3poly.o $(SCRATCH)/a4/sgi_j3poly.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_j3rget.o):\
  $(d0root)/graphics_util/sgidi3/sgi_j3rget.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_j3rget.f ;\
	mv sgi_j3rget.o $(SCRATCH)/a4/sgi_j3rget.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_j3strg.o):\
  $(d0root)/graphics_util/sgidi3/sgi_j3strg.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc\
  $(d0library)/inc/vaxfont.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_j3strg.f ;\
	mv sgi_j3strg.o $(SCRATCH)/a4/sgi_j3strg.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_j4rget.o):\
  $(d0root)/graphics_util/sgidi3/sgi_j4rget.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_j4rget.f ;\
	mv sgi_j4rget.o $(SCRATCH)/a4/sgi_j4rget.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jaload.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jaload.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jaload.f ;\
	mv sgi_jaload.o $(SCRATCH)/a4/sgi_jaload.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jarc.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jarc.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jarc.f ;\
	mv sgi_jarc.o $(SCRATCH)/a4/sgi_jarc.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jarset.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jarset.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jarset.f ;\
	mv sgi_jarset.o $(SCRATCH)/a4/sgi_jarset.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jasave.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jasave.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jasave.f ;\
	mv sgi_jasave.o $(SCRATCH)/a4/sgi_jasave.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jaspek.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jaspek.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jaspek.f ;\
	mv sgi_jaspek.o $(SCRATCH)/a4/sgi_jaspek.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jattrb.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jattrb.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jattrb.f ;\
	mv sgi_jattrb.o $(SCRATCH)/a4/sgi_jattrb.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jbackg.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jbackg.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jbackg.f ;\
	mv sgi_jbackg.o $(SCRATCH)/a4/sgi_jbackg.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jbase.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jbase.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jbase.f ;\
	mv sgi_jbase.o $(SCRATCH)/a4/sgi_jbase.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jbegin.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jbegin.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jbegin.f ;\
	mv sgi_jbegin.o $(SCRATCH)/a4/sgi_jbegin.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jbgbat.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jbgbat.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jbgbat.f ;\
	mv sgi_jbgbat.o $(SCRATCH)/a4/sgi_jbgbat.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jbuild.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jbuild.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jbuild.f ;\
	mv sgi_jbuild.o $(SCRATCH)/a5/sgi_jbuild.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jcircl.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jcircl.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jcircl.f ;\
	mv sgi_jcircl.o $(SCRATCH)/a5/sgi_jcircl.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jclear.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jclear.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jclear.f ;\
	mv sgi_jclear.o $(SCRATCH)/a5/sgi_jclear.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jclose.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jclose.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jclose.f ;\
	mv sgi_jclose.o $(SCRATCH)/a5/sgi_jclose.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jcmark.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jcmark.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jcmark.f ;\
	mv sgi_jcmark.o $(SCRATCH)/a5/sgi_jcmark.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jcolor.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jcolor.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jcolor.f ;\
	mv sgi_jcolor.o $(SCRATCH)/a5/sgi_jcolor.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jconpk.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jconpk.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jconpk.f ;\
	mv sgi_jconpk.o $(SCRATCH)/a5/sgi_jconpk.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jconvw.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jconvw.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jconvw.f ;\
	mv sgi_jconvw.o $(SCRATCH)/a5/sgi_jconvw.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jconwv.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jconwv.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jconwv.f ;\
	mv sgi_jconwv.o $(SCRATCH)/a5/sgi_jconwv.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jcotbl.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jcotbl.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jcotbl.f ;\
	mv sgi_jcotbl.o $(SCRATCH)/a5/sgi_jcotbl.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jcview.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jcview.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jcview.f ;\
	mv sgi_jcview.o $(SCRATCH)/a5/sgi_jcview.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jdbase.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdbase.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jdbase.f ;\
	mv sgi_jdbase.o $(SCRATCH)/a5/sgi_jdbase.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jdcolr.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdcolr.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jdcolr.f ;\
	mv sgi_jdcolr.o $(SCRATCH)/a5/sgi_jdcolr.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jdd3d.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdd3d.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jdd3d.f ;\
	mv sgi_jdd3d.o $(SCRATCH)/a5/sgi_jdd3d.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jddete.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jddete.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jddete.f ;\
	mv sgi_jddete.o $(SCRATCH)/a5/sgi_jddete.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jdend.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdend.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jdend.f ;\
	mv sgi_jdend.o $(SCRATCH)/a5/sgi_jdend.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jdetec.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdetec.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jdetec.f ;\
	mv sgi_jdetec.o $(SCRATCH)/a5/sgi_jdetec.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jdevof.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdevof.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jdevof.f ;\
	mv sgi_jdevof.o $(SCRATCH)/a5/sgi_jdevof.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jdevon.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdevon.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jdevon.f ;\
	mv sgi_jdevon.o $(SCRATCH)/a5/sgi_jdevon.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jdevvp.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdevvp.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jdevvp.f ;\
	mv sgi_jdevvp.o $(SCRATCH)/a5/sgi_jdevvp.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jdevwn.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdevwn.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jdevwn.f ;\
	mv sgi_jdevwn.o $(SCRATCH)/a6/sgi_jdevwn.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jdfont.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdfont.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jdfont.f ;\
	mv sgi_jdfont.o $(SCRATCH)/a6/sgi_jdfont.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jdinit.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdinit.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jdinit.f ;\
	mv sgi_jdinit.o $(SCRATCH)/a6/sgi_jdinit.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jdjust.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdjust.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jdjust.f ;\
	mv sgi_jdjust.o $(SCRATCH)/a6/sgi_jdjust.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jdlsty.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdlsty.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jdlsty.f ;\
	mv sgi_jdlsty.o $(SCRATCH)/a6/sgi_jdlsty.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jdlwid.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdlwid.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jdlwid.f ;\
	mv sgi_jdlwid.o $(SCRATCH)/a6/sgi_jdlwid.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jdmark.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdmark.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jdmark.f ;\
	mv sgi_jdmark.o $(SCRATCH)/a6/sgi_jdmark.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jdpidx.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdpidx.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jdpidx.f ;\
	mv sgi_jdpidx.o $(SCRATCH)/a6/sgi_jdpidx.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jdpint.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdpint.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jdpint.f ;\
	mv sgi_jdpint.o $(SCRATCH)/a6/sgi_jdpint.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jdpinx.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdpinx.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jdpinx.f ;\
	mv sgi_jdpinx.o $(SCRATCH)/a6/sgi_jdpinx.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jdpkid.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdpkid.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jdpkid.f ;\
	mv sgi_jdpkid.o $(SCRATCH)/a6/sgi_jdpkid.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jdraw.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdraw.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jdraw.f ;\
	mv sgi_jdraw.o $(SCRATCH)/a6/sgi_jdraw.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jdsize.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdsize.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jdsize.f ;\
	mv sgi_jdsize.o $(SCRATCH)/a6/sgi_jdsize.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jdvisb.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jdvisb.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jdvisb.f ;\
	mv sgi_jdvisb.o $(SCRATCH)/a6/sgi_jdvisb.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jelseg.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jelseg.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jelseg.f ;\
	mv sgi_jelseg.o $(SCRATCH)/a6/sgi_jelseg.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jenbat.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jenbat.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jenbat.f ;\
	mv sgi_jenbat.o $(SCRATCH)/a6/sgi_jenbat.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jend.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jend.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jend.f ;\
	mv sgi_jend.o $(SCRATCH)/a6/sgi_jend.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jepseg.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jepseg.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jepseg.f ;\
	mv sgi_jepseg.o $(SCRATCH)/a6/sgi_jepseg.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jescap.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jescap.f\
  $(d0library)/params/escape_code_names.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jescap.f ;\
	mv sgi_jescap.o $(SCRATCH)/a6/sgi_jescap.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jextnt.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jextnt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jextnt.f ;\
	mv sgi_jextnt.o $(SCRATCH)/a6/sgi_jextnt.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jfiles.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jfiles.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jfiles.f ;\
	mv sgi_jfiles.o $(SCRATCH)/a7/sgi_jfiles.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jfont.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jfont.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jfont.f ;\
	mv sgi_jfont.o $(SCRATCH)/a7/sgi_jfont.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jframe.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jframe.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jframe.f ;\
	mv sgi_jframe.o $(SCRATCH)/a7/sgi_jframe.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jfsopn.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jfsopn.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jfsopn.f ;\
	mv sgi_jfsopn.o $(SCRATCH)/a7/sgi_jfsopn.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jhclip.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jhclip.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jhclip.f ;\
	mv sgi_jhclip.o $(SCRATCH)/a7/sgi_jhclip.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jhcpy.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jhcpy.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jhcpy.f ;\
	mv sgi_jhcpy.o $(SCRATCH)/a7/sgi_jhcpy.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jhilit.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jhilit.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jhilit.f ;\
	mv sgi_jhilit.o $(SCRATCH)/a7/sgi_jhilit.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jhithr.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jhithr.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jhithr.f ;\
	mv sgi_jhithr.o $(SCRATCH)/a7/sgi_jhithr.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jhstrg.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jhstrg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jhstrg.f ;\
	mv sgi_jhstrg.o $(SCRATCH)/a7/sgi_jhstrg.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jidisa.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jidisa.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jidisa.f ;\
	mv sgi_jidisa.o $(SCRATCH)/a7/sgi_jidisa.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jienab.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jienab.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jienab.f ;\
	mv sgi_jienab.o $(SCRATCH)/a7/sgi_jienab.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jiescp.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jiescp.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jiescp.f ;\
	mv sgi_jiescp.o $(SCRATCH)/a7/sgi_jiescp.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jinten.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jinten.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jinten.f ;\
	mv sgi_jinten.o $(SCRATCH)/a7/sgi_jinten.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jiqddl.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jiqddl.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jiqddl.f ;\
	mv sgi_jiqddl.o $(SCRATCH)/a7/sgi_jiqddl.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jiqdev.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jiqdev.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jiqdev.f ;\
	mv sgi_jiqdev.o $(SCRATCH)/a7/sgi_jiqdev.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jiqdil.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jiqdil.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jiqdil.f ;\
	mv sgi_jiqdil.o $(SCRATCH)/a7/sgi_jiqdil.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jiqdim.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jiqdim.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jiqdim.f ;\
	mv sgi_jiqdim.o $(SCRATCH)/a7/sgi_jiqdim.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jiqtxt.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jiqtxt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jiqtxt.f ;\
	mv sgi_jiqtxt.o $(SCRATCH)/a7/sgi_jiqtxt.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jisgmt.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jisgmt.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jisgmt.f ;\
	mv sgi_jisgmt.o $(SCRATCH)/a7/sgi_jisgmt.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jiwind.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jiwind.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jiwind.f ;\
	mv sgi_jiwind.o $(SCRATCH)/a7/sgi_jiwind.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jjust.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jjust.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jjust.f ;\
	mv sgi_jjust.o $(SCRATCH)/a8/sgi_jjust.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jkeybs.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jkeybs.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jkeybs.f ;\
	mv sgi_jkeybs.o $(SCRATCH)/a8/sgi_jkeybs.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jlocat.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jlocat.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jlocat.f ;\
	mv sgi_jlocat.o $(SCRATCH)/a8/sgi_jlocat.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jlstyl.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jlstyl.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jlstyl.f ;\
	mv sgi_jlstyl.o $(SCRATCH)/a8/sgi_jlstyl.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jlwide.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jlwide.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jlwide.f ;\
	mv sgi_jlwide.o $(SCRATCH)/a8/sgi_jlwide.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jmark.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jmark.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jmark.f ;\
	mv sgi_jmark.o $(SCRATCH)/a8/sgi_jmark.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jmodel.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jmodel.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jmodel.f ;\
	mv sgi_jmodel.o $(SCRATCH)/a8/sgi_jmodel.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jmodon.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jmodon.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jmodon.f ;\
	mv sgi_jmodon.o $(SCRATCH)/a8/sgi_jmodon.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jmove.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jmove.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jmove.f ;\
	mv sgi_jmove.o $(SCRATCH)/a8/sgi_jmove.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jmstrg.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jmstrg.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jmstrg.f ;\
	mv sgi_jmstrg.o $(SCRATCH)/a8/sgi_jmstrg.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jnorml.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jnorml.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jnorml.f ;\
	mv sgi_jnorml.o $(SCRATCH)/a8/sgi_jnorml.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jopen.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jopen.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jopen.f ;\
	mv sgi_jopen.o $(SCRATCH)/a8/sgi_jopen.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jp3mrk.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jp3mrk.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jp3mrk.f ;\
	mv sgi_jp3mrk.o $(SCRATCH)/a8/sgi_jp3mrk.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jparal.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jparal.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jparal.f ;\
	mv sgi_jparal.o $(SCRATCH)/a8/sgi_jparal.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jparob.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jparob.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jparob.f ;\
	mv sgi_jparob.o $(SCRATCH)/a8/sgi_jparob.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jpause.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpause.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jpause.f ;\
	mv sgi_jpause.o $(SCRATCH)/a8/sgi_jpause.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jpecho.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpecho.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jpecho.f ;\
	mv sgi_jpecho.o $(SCRATCH)/a8/sgi_jpecho.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jpedge.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpedge.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jpedge.f ;\
	mv sgi_jpedge.o $(SCRATCH)/a8/sgi_jpedge.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jperob.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jperob.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jperob.f ;\
	mv sgi_jperob.o $(SCRATCH)/a8/sgi_jperob.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jpersp.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpersp.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jpersp.f ;\
	mv sgi_jpersp.o $(SCRATCH)/a8/sgi_jpersp.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jpick.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpick.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jpick.f ;\
	mv sgi_jpick.o $(SCRATCH)/a9/sgi_jpick.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jpidex.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpidex.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jpidex.f ;\
	mv sgi_jpidex.o $(SCRATCH)/a9/sgi_jpidex.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jpintr.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpintr.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jpintr.f ;\
	mv sgi_jpintr.o $(SCRATCH)/a9/sgi_jpintr.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jpkapr.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpkapr.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jpkapr.f ;\
	mv sgi_jpkapr.o $(SCRATCH)/a9/sgi_jpkapr.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jpkid.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpkid.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jpkid.f ;\
	mv sgi_jpkid.o $(SCRATCH)/a9/sgi_jpkid.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jplane.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jplane.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jplane.f ;\
	mv sgi_jplane.o $(SCRATCH)/a9/sgi_jplane.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jpmark.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpmark.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jpmark.f ;\
	mv sgi_jpmark.o $(SCRATCH)/a9/sgi_jpmark.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jpolgn.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpolgn.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jpolgn.f ;\
	mv sgi_jpolgn.o $(SCRATCH)/a9/sgi_jpolgn.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jpoly.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpoly.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jpoly.f ;\
	mv sgi_jpoly.o $(SCRATCH)/a9/sgi_jpoly.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jpr3mr.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpr3mr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jpr3mr.f ;\
	mv sgi_jpr3mr.o $(SCRATCH)/a9/sgi_jpr3mr.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jprmrk.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jprmrk.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jprmrk.f ;\
	mv sgi_jprmrk.o $(SCRATCH)/a9/sgi_jprmrk.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jpurge.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jpurge.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jpurge.f ;\
	mv sgi_jpurge.o $(SCRATCH)/a9/sgi_jpurge.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jr3dra.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jr3dra.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jr3dra.f ;\
	mv sgi_jr3dra.o $(SCRATCH)/a9/sgi_jr3dra.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jr3mov.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jr3mov.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jr3mov.f ;\
	mv sgi_jr3mov.o $(SCRATCH)/a9/sgi_jr3mov.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jr3mrk.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jr3mrk.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jr3mrk.f ;\
	mv sgi_jr3mrk.o $(SCRATCH)/a9/sgi_jr3mrk.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jr3pgn.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jr3pgn.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jr3pgn.f ;\
	mv sgi_jr3pgn.o $(SCRATCH)/a9/sgi_jr3pgn.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jr3ply.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jr3ply.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jr3ply.f ;\
	mv sgi_jr3ply.o $(SCRATCH)/a9/sgi_jr3ply.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jrclos.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jrclos.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jrclos.f ;\
	mv sgi_jrclos.o $(SCRATCH)/a9/sgi_jrclos.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jrdraw.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jrdraw.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jrdraw.f ;\
	mv sgi_jrdraw.o $(SCRATCH)/a9/sgi_jrdraw.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jrect.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jrect.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jrect.f ;\
	mv sgi_jrect.o $(SCRATCH)/a9/sgi_jrect.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jright.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jright.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jright.f ;\
	mv sgi_jright.o $(SCRATCH)/a10/sgi_jright.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jrmark.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jrmark.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jrmark.f ;\
	mv sgi_jrmark.o $(SCRATCH)/a10/sgi_jrmark.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jrmove.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jrmove.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jrmove.f ;\
	mv sgi_jrmove.o $(SCRATCH)/a10/sgi_jrmove.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jropen.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jropen.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jropen.f ;\
	mv sgi_jropen.o $(SCRATCH)/a10/sgi_jropen.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jrplgn.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jrplgn.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jrplgn.f ;\
	mv sgi_jrplgn.o $(SCRATCH)/a10/sgi_jrplgn.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jrpoly.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jrpoly.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jrpoly.f ;\
	mv sgi_jrpoly.o $(SCRATCH)/a10/sgi_jrpoly.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jrrect.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jrrect.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jrrect.f ;\
	mv sgi_jrrect.o $(SCRATCH)/a10/sgi_jrrect.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jsaall.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jsaall.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jsaall.f ;\
	mv sgi_jsaall.o $(SCRATCH)/a10/sgi_jsaall.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jsasso.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jsasso.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jsasso.f ;\
	mv sgi_jsasso.o $(SCRATCH)/a10/sgi_jsasso.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jsectr.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jsectr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jsectr.f ;\
	mv sgi_jsectr.o $(SCRATCH)/a10/sgi_jsectr.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jsetdb.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jsetdb.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jsetdb.f ;\
	mv sgi_jsetdb.o $(SCRATCH)/a10/sgi_jsetdb.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jseter.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jseter.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jseter.f ;\
	mv sgi_jseter.o $(SCRATCH)/a10/sgi_jseter.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jsgpri.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jsgpri.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jsgpri.f ;\
	mv sgi_jsgpri.o $(SCRATCH)/a10/sgi_jsgpri.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jsize.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jsize.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jsize.f ;\
	mv sgi_jsize.o $(SCRATCH)/a10/sgi_jsize.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jspick.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jspick.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jspick.f ;\
	mv sgi_jspick.o $(SCRATCH)/a10/sgi_jspick.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jsview.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jsview.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jsview.f ;\
	mv sgi_jsview.o $(SCRATCH)/a10/sgi_jsview.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jt2all.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jt2all.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jt2all.f ;\
	mv sgi_jt2all.o $(SCRATCH)/a10/sgi_jt2all.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jtrans.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jtrans.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jtrans.f ;\
	mv sgi_jtrans.o $(SCRATCH)/a10/sgi_jtrans.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jttype.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jttype.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jttype.f ;\
	mv sgi_jttype.o $(SCRATCH)/a10/sgi_jttype.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jupvec.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jupvec.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jupvec.f ;\
	mv sgi_jupvec.o $(SCRATCH)/a10/sgi_jupvec.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jvisbl.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jvisbl.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jvisbl.f ;\
	mv sgi_jvisbl.o $(SCRATCH)/a11/sgi_jvisbl.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jvload.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jvload.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jvload.f ;\
	mv sgi_jvload.o $(SCRATCH)/a11/sgi_jvload.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jvport.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jvport.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jvport.f ;\
	mv sgi_jvport.o $(SCRATCH)/a11/sgi_jvport.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jvsave.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jvsave.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jvsave.f ;\
	mv sgi_jvsave.o $(SCRATCH)/a11/sgi_jvsave.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jvspac.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jvspac.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jvspac.f ;\
	mv sgi_jvspac.o $(SCRATCH)/a11/sgi_jvspac.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jvupln.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jvupln.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jvupln.f ;\
	mv sgi_jvupln.o $(SCRATCH)/a11/sgi_jvupln.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jvupnt.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jvupnt.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jvupnt.f ;\
	mv sgi_jvupnt.o $(SCRATCH)/a11/sgi_jvupnt.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jwclip.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jwclip.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jwclip.f ;\
	mv sgi_jwclip.o $(SCRATCH)/a11/sgi_jwclip.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jwindo.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jwindo.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jwindo.f ;\
	mv sgi_jwindo.o $(SCRATCH)/a11/sgi_jwindo.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jyclip.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jyclip.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jyclip.f ;\
	mv sgi_jyclip.o $(SCRATCH)/a11/sgi_jyclip.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_jyon.o):\
  $(d0root)/graphics_util/sgidi3/sgi_jyon.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_jyon.f ;\
	mv sgi_jyon.o $(SCRATCH)/a11/sgi_jyon.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(sgi_re_3d329.o):\
  $(d0root)/graphics_util/sgidi3/sgi_rotate_3d.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pxpara.inc\
  $(d0library)/inc/hdrinx.inc\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/sgi_rotate_3d.f ;\
	mv sgi_rotate_3d.o $(SCRATCH)/a11/sgi_re_3d329.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(tk_clear.o):\
  $(d0root)/graphics_util/sgidi3/tk_clear.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/tk_clear.f ;\
	mv tk_clear.o $(SCRATCH)/a11/tk_clear.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(tk_color.o):\
  $(d0root)/graphics_util/sgidi3/tk_color.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/tk_color.f ;\
	mv tk_color.o $(SCRATCH)/a11/tk_color.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(tk_crns.o):\
  $(d0root)/graphics_util/sgidi3/tk_crns.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/tk_crns.f ;\
	mv tk_crns.o $(SCRATCH)/a11/tk_crns.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(tk_drascr.o):\
  $(d0root)/graphics_util/sgidi3/tk_drascr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/tk_drascr.f ;\
	mv tk_drascr.o $(SCRATCH)/a11/tk_drascr.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(tk_draw.o):\
  $(d0root)/graphics_util/sgidi3/tk_draw.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/tk_draw.f ;\
	mv tk_draw.o $(SCRATCH)/a11/tk_draw.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(tk_fin.o):\
  $(d0root)/graphics_util/sgidi3/tk_fin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/tk_fin.f ;\
	mv tk_fin.o $(SCRATCH)/a11/tk_fin.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(tk_force.o):\
  $(d0root)/graphics_util/sgidi3/tk_force.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/tk_force.f ;\
	mv tk_force.o $(SCRATCH)/a11/tk_force.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(tk_line2.o):\
  $(d0root)/graphics_util/sgidi3/tk_line2.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/tk_line2.f ;\
	mv tk_line2.o $(SCRATCH)/a11/tk_line2.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(tk_move.o):\
  $(d0root)/graphics_util/sgidi3/tk_move.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/tk_move.f ;\
	mv tk_move.o $(SCRATCH)/a12/tk_move.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(tk_movscr.o):\
  $(d0root)/graphics_util/sgidi3/tk_movscr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/tk_movscr.f ;\
	mv tk_movscr.o $(SCRATCH)/a12/tk_movscr.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(tk_out.o):\
  $(d0root)/graphics_util/sgidi3/tk_out.f\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/tk_out.f ;\
	mv tk_out.o $(SCRATCH)/a12/tk_out.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(tk_poly.o):\
  $(d0root)/graphics_util/sgidi3/tk_poly.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/tk_poly.f ;\
	mv tk_poly.o $(SCRATCH)/a12/tk_poly.o ;\
	)
$(d0root)/graphics_util/sgidi3.a(tk_text.o):\
  $(d0root)/graphics_util/sgidi3/tk_text.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgidi3/tk_text.f ;\
	mv tk_text.o $(SCRATCH)/a12/tk_text.o ;\
	)
pre:\
  $(d0root)/graphics_util/sgidi3/all_display_pixie.f\
  $(d0root)/graphics_util/sgidi3/close_tkwind.f\
  $(d0root)/graphics_util/sgidi3/d_dmpmat.f\
  $(d0root)/graphics_util/sgidi3/d_matcpy.f\
  $(d0root)/graphics_util/sgidi3/d_matmul.f\
  $(d0root)/graphics_util/sgidi3/d_matrot.f\
  $(d0root)/graphics_util/sgidi3/d_matsca.f\
  $(d0root)/graphics_util/sgidi3/d_mattra.f\
  $(d0root)/graphics_util/sgidi3/d_matuni.f\
  $(d0root)/graphics_util/sgidi3/dev_clear.f\
  $(d0root)/graphics_util/sgidi3/dev_close_window.f\
  $(d0root)/graphics_util/sgidi3/dev_color.f\
  $(d0root)/graphics_util/sgidi3/dev_deflin.f\
  $(d0root)/graphics_util/sgidi3/dev_draw.f\
  $(d0root)/graphics_util/sgidi3/dev_force.f\
  $(d0root)/graphics_util/sgidi3/dev_linewi.f\
  $(d0root)/graphics_util/sgidi3/dev_mark.f\
  $(d0root)/graphics_util/sgidi3/dev_move.f\
  $(d0root)/graphics_util/sgidi3/dev_movscr.f\
  $(d0root)/graphics_util/sgidi3/dev_open_window.f\
  $(d0root)/graphics_util/sgidi3/dev_page.f\
  $(d0root)/graphics_util/sgidi3/dev_plin.f\
  $(d0root)/graphics_util/sgidi3/dev_polf.f\
  $(d0root)/graphics_util/sgidi3/dev_poly.f\
  $(d0root)/graphics_util/sgidi3/dev_rot.f\
  $(d0root)/graphics_util/sgidi3/dev_scale.f\
  $(d0root)/graphics_util/sgidi3/dev_setlin.f\
  $(d0root)/graphics_util/sgidi3/dev_transf.f\
  $(d0root)/graphics_util/sgidi3/dev_transl.f\
  $(d0root)/graphics_util/sgidi3/dev_window_corners.f\
  $(d0root)/graphics_util/sgidi3/dmpseg.f\
  $(d0root)/graphics_util/sgidi3/hcpy_filename.f\
  $(d0root)/graphics_util/sgidi3/hls_to_rgb.f\
  $(d0root)/graphics_util/sgidi3/j_cross.f\
  $(d0root)/graphics_util/sgidi3/j_delsegm.f\
  $(d0root)/graphics_util/sgidi3/j_devtrn.f\
  $(d0root)/graphics_util/sgidi3/j_dummies.f\
  $(d0root)/graphics_util/sgidi3/j_dump.f\
  $(d0root)/graphics_util/sgidi3/j_grey.f\
  $(d0root)/graphics_util/sgidi3/j_hcopy_begin.f\
  $(d0root)/graphics_util/sgidi3/j_hcopy_device.f\
  $(d0root)/graphics_util/sgidi3/j_hcopy_end.f\
  $(d0root)/graphics_util/sgidi3/j_menu3d.f\
  $(d0root)/graphics_util/sgidi3/j_projs.f\
  $(d0root)/graphics_util/sgidi3/just_comments.f\
  $(d0root)/graphics_util/sgidi3/open_tkwind.f\
  $(d0root)/graphics_util/sgidi3/polyn.f\
  $(d0root)/graphics_util/sgidi3/ps_color.f\
  $(d0root)/graphics_util/sgidi3/ps_crns.f\
  $(d0root)/graphics_util/sgidi3/ps_init.f\
  $(d0root)/graphics_util/sgidi3/ps_linewi.f\
  $(d0root)/graphics_util/sgidi3/ps_move.f\
  $(d0root)/graphics_util/sgidi3/ps_poly.f\
  $(d0root)/graphics_util/sgidi3/ps_setlin.f\
  $(d0root)/graphics_util/sgidi3/qm_color.f\
  $(d0root)/graphics_util/sgidi3/qm_crns.f\
  $(d0root)/graphics_util/sgidi3/qm_init.f\
  $(d0root)/graphics_util/sgidi3/qm_move.f\
  $(d0root)/graphics_util/sgidi3/qm_poly.f\
  $(d0root)/graphics_util/sgidi3/sgi_j1iget.f\
  $(d0root)/graphics_util/sgidi3/sgi_j1strg.f\
  $(d0root)/graphics_util/sgidi3/sgi_j2strg.f\
  $(d0root)/graphics_util/sgidi3/sgi_j3draw.f\
  $(d0root)/graphics_util/sgidi3/sgi_j3mark.f\
  $(d0root)/graphics_util/sgidi3/sgi_j3move.f\
  $(d0root)/graphics_util/sgidi3/sgi_j3plgn.f\
  $(d0root)/graphics_util/sgidi3/sgi_j3poly.f\
  $(d0root)/graphics_util/sgidi3/sgi_j3rget.f\
  $(d0root)/graphics_util/sgidi3/sgi_j3strg.f\
  $(d0root)/graphics_util/sgidi3/sgi_j4rget.f\
  $(d0root)/graphics_util/sgidi3/sgi_jaload.f\
  $(d0root)/graphics_util/sgidi3/sgi_jarc.f\
  $(d0root)/graphics_util/sgidi3/sgi_jarset.f\
  $(d0root)/graphics_util/sgidi3/sgi_jasave.f\
  $(d0root)/graphics_util/sgidi3/sgi_jaspek.f\
  $(d0root)/graphics_util/sgidi3/sgi_jattrb.f\
  $(d0root)/graphics_util/sgidi3/sgi_jbackg.f\
  $(d0root)/graphics_util/sgidi3/sgi_jbase.f\
  $(d0root)/graphics_util/sgidi3/sgi_jbegin.f\
  $(d0root)/graphics_util/sgidi3/sgi_jbgbat.f\
  $(d0root)/graphics_util/sgidi3/sgi_jbuild.f\
  $(d0root)/graphics_util/sgidi3/sgi_jcircl.f\
  $(d0root)/graphics_util/sgidi3/sgi_jclear.f\
  $(d0root)/graphics_util/sgidi3/sgi_jclose.f\
  $(d0root)/graphics_util/sgidi3/sgi_jcmark.f\
  $(d0root)/graphics_util/sgidi3/sgi_jcolor.f\
  $(d0root)/graphics_util/sgidi3/sgi_jconpk.f\
  $(d0root)/graphics_util/sgidi3/sgi_jconvw.f\
  $(d0root)/graphics_util/sgidi3/sgi_jconwv.f\
  $(d0root)/graphics_util/sgidi3/sgi_jcotbl.f\
  $(d0root)/graphics_util/sgidi3/sgi_jcview.f\
  $(d0root)/graphics_util/sgidi3/sgi_jdbase.f\
  $(d0root)/graphics_util/sgidi3/sgi_jdcolr.f\
  $(d0root)/graphics_util/sgidi3/sgi_jdd3d.f\
  $(d0root)/graphics_util/sgidi3/sgi_jddete.f\
  $(d0root)/graphics_util/sgidi3/sgi_jdend.f\
  $(d0root)/graphics_util/sgidi3/sgi_jdetec.f\
  $(d0root)/graphics_util/sgidi3/sgi_jdevof.f\
  $(d0root)/graphics_util/sgidi3/sgi_jdevon.f\
  $(d0root)/graphics_util/sgidi3/sgi_jdevvp.f\
  $(d0root)/graphics_util/sgidi3/sgi_jdevwn.f\
  $(d0root)/graphics_util/sgidi3/sgi_jdfont.f\
  $(d0root)/graphics_util/sgidi3/sgi_jdinit.f\
  $(d0root)/graphics_util/sgidi3/sgi_jdjust.f\
  $(d0root)/graphics_util/sgidi3/sgi_jdlsty.f\
  $(d0root)/graphics_util/sgidi3/sgi_jdlwid.f\
  $(d0root)/graphics_util/sgidi3/sgi_jdmark.f\
  $(d0root)/graphics_util/sgidi3/sgi_jdpidx.f\
  $(d0root)/graphics_util/sgidi3/sgi_jdpint.f\
  $(d0root)/graphics_util/sgidi3/sgi_jdpinx.f\
  $(d0root)/graphics_util/sgidi3/sgi_jdpkid.f\
  $(d0root)/graphics_util/sgidi3/sgi_jdraw.f\
  $(d0root)/graphics_util/sgidi3/sgi_jdsize.f\
  $(d0root)/graphics_util/sgidi3/sgi_jdvisb.f\
  $(d0root)/graphics_util/sgidi3/sgi_jelseg.f\
  $(d0root)/graphics_util/sgidi3/sgi_jenbat.f\
  $(d0root)/graphics_util/sgidi3/sgi_jend.f\
  $(d0root)/graphics_util/sgidi3/sgi_jepseg.f\
  $(d0root)/graphics_util/sgidi3/sgi_jescap.f\
  $(d0root)/graphics_util/sgidi3/sgi_jextnt.f\
  $(d0root)/graphics_util/sgidi3/sgi_jfiles.f\
  $(d0root)/graphics_util/sgidi3/sgi_jfont.f\
  $(d0root)/graphics_util/sgidi3/sgi_jframe.f\
  $(d0root)/graphics_util/sgidi3/sgi_jfsopn.f\
  $(d0root)/graphics_util/sgidi3/sgi_jhclip.f\
  $(d0root)/graphics_util/sgidi3/sgi_jhcpy.f\
  $(d0root)/graphics_util/sgidi3/sgi_jhilit.f\
  $(d0root)/graphics_util/sgidi3/sgi_jhithr.f\
  $(d0root)/graphics_util/sgidi3/sgi_jhstrg.f\
  $(d0root)/graphics_util/sgidi3/sgi_jidisa.f\
  $(d0root)/graphics_util/sgidi3/sgi_jienab.f\
  $(d0root)/graphics_util/sgidi3/sgi_jiescp.f\
  $(d0root)/graphics_util/sgidi3/sgi_jinten.f\
  $(d0root)/graphics_util/sgidi3/sgi_jiqddl.f\
  $(d0root)/graphics_util/sgidi3/sgi_jiqdev.f\
  $(d0root)/graphics_util/sgidi3/sgi_jiqdil.f\
  $(d0root)/graphics_util/sgidi3/sgi_jiqdim.f\
  $(d0root)/graphics_util/sgidi3/sgi_jiqtxt.f\
  $(d0root)/graphics_util/sgidi3/sgi_jisgmt.f\
  $(d0root)/graphics_util/sgidi3/sgi_jiwind.f\
  $(d0root)/graphics_util/sgidi3/sgi_jjust.f\
  $(d0root)/graphics_util/sgidi3/sgi_jkeybs.f\
  $(d0root)/graphics_util/sgidi3/sgi_jlocat.f\
  $(d0root)/graphics_util/sgidi3/sgi_jlstyl.f\
  $(d0root)/graphics_util/sgidi3/sgi_jlwide.f\
  $(d0root)/graphics_util/sgidi3/sgi_jmark.f\
  $(d0root)/graphics_util/sgidi3/sgi_jmodel.f\
  $(d0root)/graphics_util/sgidi3/sgi_jmodon.f\
  $(d0root)/graphics_util/sgidi3/sgi_jmove.f\
  $(d0root)/graphics_util/sgidi3/sgi_jmstrg.f\
  $(d0root)/graphics_util/sgidi3/sgi_jnorml.f\
  $(d0root)/graphics_util/sgidi3/sgi_jopen.f\
  $(d0root)/graphics_util/sgidi3/sgi_jp3mrk.f\
  $(d0root)/graphics_util/sgidi3/sgi_jparal.f\
  $(d0root)/graphics_util/sgidi3/sgi_jparob.f\
  $(d0root)/graphics_util/sgidi3/sgi_jpause.f\
  $(d0root)/graphics_util/sgidi3/sgi_jpecho.f\
  $(d0root)/graphics_util/sgidi3/sgi_jpedge.f\
  $(d0root)/graphics_util/sgidi3/sgi_jperob.f\
  $(d0root)/graphics_util/sgidi3/sgi_jpersp.f\
  $(d0root)/graphics_util/sgidi3/sgi_jpick.f\
  $(d0root)/graphics_util/sgidi3/sgi_jpidex.f\
  $(d0root)/graphics_util/sgidi3/sgi_jpintr.f\
  $(d0root)/graphics_util/sgidi3/sgi_jpkapr.f\
  $(d0root)/graphics_util/sgidi3/sgi_jpkid.f\
  $(d0root)/graphics_util/sgidi3/sgi_jplane.f\
  $(d0root)/graphics_util/sgidi3/sgi_jpmark.f\
  $(d0root)/graphics_util/sgidi3/sgi_jpolgn.f\
  $(d0root)/graphics_util/sgidi3/sgi_jpoly.f\
  $(d0root)/graphics_util/sgidi3/sgi_jpr3mr.f\
  $(d0root)/graphics_util/sgidi3/sgi_jprmrk.f\
  $(d0root)/graphics_util/sgidi3/sgi_jpurge.f\
  $(d0root)/graphics_util/sgidi3/sgi_jr3dra.f\
  $(d0root)/graphics_util/sgidi3/sgi_jr3mov.f\
  $(d0root)/graphics_util/sgidi3/sgi_jr3mrk.f\
  $(d0root)/graphics_util/sgidi3/sgi_jr3pgn.f\
  $(d0root)/graphics_util/sgidi3/sgi_jr3ply.f\
  $(d0root)/graphics_util/sgidi3/sgi_jrclos.f\
  $(d0root)/graphics_util/sgidi3/sgi_jrdraw.f\
  $(d0root)/graphics_util/sgidi3/sgi_jrect.f\
  $(d0root)/graphics_util/sgidi3/sgi_jright.f\
  $(d0root)/graphics_util/sgidi3/sgi_jrmark.f\
  $(d0root)/graphics_util/sgidi3/sgi_jrmove.f\
  $(d0root)/graphics_util/sgidi3/sgi_jropen.f\
  $(d0root)/graphics_util/sgidi3/sgi_jrplgn.f\
  $(d0root)/graphics_util/sgidi3/sgi_jrpoly.f\
  $(d0root)/graphics_util/sgidi3/sgi_jrrect.f\
  $(d0root)/graphics_util/sgidi3/sgi_jsaall.f\
  $(d0root)/graphics_util/sgidi3/sgi_jsasso.f\
  $(d0root)/graphics_util/sgidi3/sgi_jsectr.f\
  $(d0root)/graphics_util/sgidi3/sgi_jsetdb.f\
  $(d0root)/graphics_util/sgidi3/sgi_jseter.f\
  $(d0root)/graphics_util/sgidi3/sgi_jsgpri.f\
  $(d0root)/graphics_util/sgidi3/sgi_jsize.f\
  $(d0root)/graphics_util/sgidi3/sgi_jspick.f\
  $(d0root)/graphics_util/sgidi3/sgi_jsview.f\
  $(d0root)/graphics_util/sgidi3/sgi_jt2all.f\
  $(d0root)/graphics_util/sgidi3/sgi_jtrans.f\
  $(d0root)/graphics_util/sgidi3/sgi_jttype.f\
  $(d0root)/graphics_util/sgidi3/sgi_jupvec.f\
  $(d0root)/graphics_util/sgidi3/sgi_jvisbl.f\
  $(d0root)/graphics_util/sgidi3/sgi_jvload.f\
  $(d0root)/graphics_util/sgidi3/sgi_jvport.f\
  $(d0root)/graphics_util/sgidi3/sgi_jvsave.f\
  $(d0root)/graphics_util/sgidi3/sgi_jvspac.f\
  $(d0root)/graphics_util/sgidi3/sgi_jvupln.f\
  $(d0root)/graphics_util/sgidi3/sgi_jvupnt.f\
  $(d0root)/graphics_util/sgidi3/sgi_jwclip.f\
  $(d0root)/graphics_util/sgidi3/sgi_jwindo.f\
  $(d0root)/graphics_util/sgidi3/sgi_jyclip.f\
  $(d0root)/graphics_util/sgidi3/sgi_jyon.f\
  $(d0root)/graphics_util/sgidi3/sgi_rotate_3d.f\
  $(d0root)/graphics_util/sgidi3/tk_clear.f\
  $(d0root)/graphics_util/sgidi3/tk_color.f\
  $(d0root)/graphics_util/sgidi3/tk_crns.f\
  $(d0root)/graphics_util/sgidi3/tk_drascr.f\
  $(d0root)/graphics_util/sgidi3/tk_draw.f\
  $(d0root)/graphics_util/sgidi3/tk_fin.f\
  $(d0root)/graphics_util/sgidi3/tk_force.f\
  $(d0root)/graphics_util/sgidi3/tk_line2.f\
  $(d0root)/graphics_util/sgidi3/tk_move.f\
  $(d0root)/graphics_util/sgidi3/tk_movscr.f\
  $(d0root)/graphics_util/sgidi3/tk_out.f\
  $(d0root)/graphics_util/sgidi3/tk_poly.f\
  $(d0root)/graphics_util/sgidi3/tk_text.f
$(d0root)/graphics_util/sgidi3/all_display_pixie.f:\
  $(d0root)/graphics_util/sgidi3/all_display_pixie.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/all_display_pixie.for | vmstounix > $(d0root)/graphics_util/sgidi3/all_display_pixie.f
$(d0root)/graphics_util/sgidi3/close_tkwind.f:\
  $(d0root)/graphics_util/sgidi3/close_tkwind.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/close_tkwind.for | vmstounix > $(d0root)/graphics_util/sgidi3/close_tkwind.f
$(d0root)/graphics_util/sgidi3/d_dmpmat.f:\
  $(d0root)/graphics_util/sgidi3/d_dmpmat.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/d_dmpmat.for | vmstounix > $(d0root)/graphics_util/sgidi3/d_dmpmat.f
$(d0root)/graphics_util/sgidi3/d_matcpy.f:\
  $(d0root)/graphics_util/sgidi3/d_matcpy.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/d_matcpy.for | vmstounix > $(d0root)/graphics_util/sgidi3/d_matcpy.f
$(d0root)/graphics_util/sgidi3/d_matmul.f:\
  $(d0root)/graphics_util/sgidi3/d_matmul.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/d_matmul.for | vmstounix > $(d0root)/graphics_util/sgidi3/d_matmul.f
$(d0root)/graphics_util/sgidi3/d_matrot.f:\
  $(d0root)/graphics_util/sgidi3/d_matrot.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/d_matrot.for | vmstounix > $(d0root)/graphics_util/sgidi3/d_matrot.f
$(d0root)/graphics_util/sgidi3/d_matsca.f:\
  $(d0root)/graphics_util/sgidi3/d_matsca.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/d_matsca.for | vmstounix > $(d0root)/graphics_util/sgidi3/d_matsca.f
$(d0root)/graphics_util/sgidi3/d_mattra.f:\
  $(d0root)/graphics_util/sgidi3/d_mattra.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/d_mattra.for | vmstounix > $(d0root)/graphics_util/sgidi3/d_mattra.f
$(d0root)/graphics_util/sgidi3/d_matuni.f:\
  $(d0root)/graphics_util/sgidi3/d_matuni.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/d_matuni.for | vmstounix > $(d0root)/graphics_util/sgidi3/d_matuni.f
$(d0root)/graphics_util/sgidi3/dev_clear.f:\
  $(d0root)/graphics_util/sgidi3/dev_clear.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/dev_clear.for | vmstounix > $(d0root)/graphics_util/sgidi3/dev_clear.f
$(d0root)/graphics_util/sgidi3/dev_close_window.f:\
  $(d0root)/graphics_util/sgidi3/dev_close_window.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/dev_close_window.for | vmstounix > $(d0root)/graphics_util/sgidi3/dev_close_window.f
$(d0root)/graphics_util/sgidi3/dev_color.f:\
  $(d0root)/graphics_util/sgidi3/dev_color.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/dev_color.for | vmstounix > $(d0root)/graphics_util/sgidi3/dev_color.f
$(d0root)/graphics_util/sgidi3/dev_deflin.f:\
  $(d0root)/graphics_util/sgidi3/dev_deflin.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/dev_deflin.for | vmstounix > $(d0root)/graphics_util/sgidi3/dev_deflin.f
$(d0root)/graphics_util/sgidi3/dev_draw.f:\
  $(d0root)/graphics_util/sgidi3/dev_draw.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/dev_draw.for | vmstounix > $(d0root)/graphics_util/sgidi3/dev_draw.f
$(d0root)/graphics_util/sgidi3/dev_force.f:\
  $(d0root)/graphics_util/sgidi3/dev_force.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/dev_force.for | vmstounix > $(d0root)/graphics_util/sgidi3/dev_force.f
$(d0root)/graphics_util/sgidi3/dev_linewi.f:\
  $(d0root)/graphics_util/sgidi3/dev_linewi.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/dev_linewi.for | vmstounix > $(d0root)/graphics_util/sgidi3/dev_linewi.f
$(d0root)/graphics_util/sgidi3/dev_mark.f:\
  $(d0root)/graphics_util/sgidi3/dev_mark.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/dev_mark.for | vmstounix > $(d0root)/graphics_util/sgidi3/dev_mark.f
$(d0root)/graphics_util/sgidi3/dev_move.f:\
  $(d0root)/graphics_util/sgidi3/dev_move.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/dev_move.for | vmstounix > $(d0root)/graphics_util/sgidi3/dev_move.f
$(d0root)/graphics_util/sgidi3/dev_movscr.f:\
  $(d0root)/graphics_util/sgidi3/dev_movscr.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/dev_movscr.for | vmstounix > $(d0root)/graphics_util/sgidi3/dev_movscr.f
$(d0root)/graphics_util/sgidi3/dev_open_window.f:\
  $(d0root)/graphics_util/sgidi3/dev_open_window.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/dev_open_window.for | vmstounix > $(d0root)/graphics_util/sgidi3/dev_open_window.f
$(d0root)/graphics_util/sgidi3/dev_page.f:\
  $(d0root)/graphics_util/sgidi3/dev_page.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/dev_page.for | vmstounix > $(d0root)/graphics_util/sgidi3/dev_page.f
$(d0root)/graphics_util/sgidi3/dev_plin.f:\
  $(d0root)/graphics_util/sgidi3/dev_plin.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/dev_plin.for | vmstounix > $(d0root)/graphics_util/sgidi3/dev_plin.f
$(d0root)/graphics_util/sgidi3/dev_polf.f:\
  $(d0root)/graphics_util/sgidi3/dev_polf.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/dev_polf.for | vmstounix > $(d0root)/graphics_util/sgidi3/dev_polf.f
$(d0root)/graphics_util/sgidi3/dev_poly.f:\
  $(d0root)/graphics_util/sgidi3/dev_poly.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/dev_poly.for | vmstounix > $(d0root)/graphics_util/sgidi3/dev_poly.f
$(d0root)/graphics_util/sgidi3/dev_rot.f:\
  $(d0root)/graphics_util/sgidi3/dev_rot.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/dev_rot.for | vmstounix > $(d0root)/graphics_util/sgidi3/dev_rot.f
$(d0root)/graphics_util/sgidi3/dev_scale.f:\
  $(d0root)/graphics_util/sgidi3/dev_scale.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/dev_scale.for | vmstounix > $(d0root)/graphics_util/sgidi3/dev_scale.f
$(d0root)/graphics_util/sgidi3/dev_setlin.f:\
  $(d0root)/graphics_util/sgidi3/dev_setlin.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/dev_setlin.for | vmstounix > $(d0root)/graphics_util/sgidi3/dev_setlin.f
$(d0root)/graphics_util/sgidi3/dev_transf.f:\
  $(d0root)/graphics_util/sgidi3/dev_transf.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/dev_transf.for | vmstounix > $(d0root)/graphics_util/sgidi3/dev_transf.f
$(d0root)/graphics_util/sgidi3/dev_transl.f:\
  $(d0root)/graphics_util/sgidi3/dev_transl.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/dev_transl.for | vmstounix > $(d0root)/graphics_util/sgidi3/dev_transl.f
$(d0root)/graphics_util/sgidi3/dev_window_corners.f:\
  $(d0root)/graphics_util/sgidi3/dev_window_corners.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/dev_window_corners.for | vmstounix > $(d0root)/graphics_util/sgidi3/dev_window_corners.f
$(d0root)/graphics_util/sgidi3/dmpseg.f:\
  $(d0root)/graphics_util/sgidi3/dmpseg.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/dmpseg.for | vmstounix > $(d0root)/graphics_util/sgidi3/dmpseg.f
$(d0root)/graphics_util/sgidi3/hcpy_filename.f:\
  $(d0root)/graphics_util/sgidi3/hcpy_filename.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/hcpy_filename.for | vmstounix > $(d0root)/graphics_util/sgidi3/hcpy_filename.f
$(d0root)/graphics_util/sgidi3/hls_to_rgb.f:\
  $(d0root)/graphics_util/sgidi3/hls_to_rgb.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/hls_to_rgb.for | vmstounix > $(d0root)/graphics_util/sgidi3/hls_to_rgb.f
$(d0root)/graphics_util/sgidi3/j_cross.f:\
  $(d0root)/graphics_util/sgidi3/j_cross.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/j_cross.for | vmstounix > $(d0root)/graphics_util/sgidi3/j_cross.f
$(d0root)/graphics_util/sgidi3/j_delsegm.f:\
  $(d0root)/graphics_util/sgidi3/j_delsegm.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/j_delsegm.for | vmstounix > $(d0root)/graphics_util/sgidi3/j_delsegm.f
$(d0root)/graphics_util/sgidi3/j_devtrn.f:\
  $(d0root)/graphics_util/sgidi3/j_devtrn.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/j_devtrn.for | vmstounix > $(d0root)/graphics_util/sgidi3/j_devtrn.f
$(d0root)/graphics_util/sgidi3/j_dummies.f:\
  $(d0root)/graphics_util/sgidi3/j_dummies.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/j_dummies.for | vmstounix > $(d0root)/graphics_util/sgidi3/j_dummies.f
$(d0root)/graphics_util/sgidi3/j_dump.f:\
  $(d0root)/graphics_util/sgidi3/j_dump.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/j_dump.for | vmstounix > $(d0root)/graphics_util/sgidi3/j_dump.f
$(d0root)/graphics_util/sgidi3/j_grey.f:\
  $(d0root)/graphics_util/sgidi3/j_grey.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/j_grey.for | vmstounix > $(d0root)/graphics_util/sgidi3/j_grey.f
$(d0root)/graphics_util/sgidi3/j_hcopy_begin.f:\
  $(d0root)/graphics_util/sgidi3/j_hcopy_begin.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/j_hcopy_begin.for | vmstounix > $(d0root)/graphics_util/sgidi3/j_hcopy_begin.f
$(d0root)/graphics_util/sgidi3/j_hcopy_device.f:\
  $(d0root)/graphics_util/sgidi3/j_hcopy_device.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/j_hcopy_device.for | vmstounix > $(d0root)/graphics_util/sgidi3/j_hcopy_device.f
$(d0root)/graphics_util/sgidi3/j_hcopy_end.f:\
  $(d0root)/graphics_util/sgidi3/j_hcopy_end.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/j_hcopy_end.for | vmstounix > $(d0root)/graphics_util/sgidi3/j_hcopy_end.f
$(d0root)/graphics_util/sgidi3/j_menu3d.f:\
  $(d0root)/graphics_util/sgidi3/j_menu3d.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/j_menu3d.for | vmstounix > $(d0root)/graphics_util/sgidi3/j_menu3d.f
$(d0root)/graphics_util/sgidi3/j_projs.f:\
  $(d0root)/graphics_util/sgidi3/j_projs.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/j_projs.for | vmstounix > $(d0root)/graphics_util/sgidi3/j_projs.f
$(d0root)/graphics_util/sgidi3/just_comments.f:\
  $(d0root)/graphics_util/sgidi3/just_comments.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/just_comments.for | vmstounix > $(d0root)/graphics_util/sgidi3/just_comments.f
$(d0root)/graphics_util/sgidi3/open_tkwind.f:\
  $(d0root)/graphics_util/sgidi3/open_tkwind.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/open_tkwind.for | vmstounix > $(d0root)/graphics_util/sgidi3/open_tkwind.f
$(d0root)/graphics_util/sgidi3/polyn.f:\
  $(d0root)/graphics_util/sgidi3/polyn.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/polyn.for | vmstounix > $(d0root)/graphics_util/sgidi3/polyn.f
$(d0root)/graphics_util/sgidi3/ps_color.f:\
  $(d0root)/graphics_util/sgidi3/ps_color.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/ps_color.for | vmstounix > $(d0root)/graphics_util/sgidi3/ps_color.f
$(d0root)/graphics_util/sgidi3/ps_crns.f:\
  $(d0root)/graphics_util/sgidi3/ps_crns.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/ps_crns.for | vmstounix > $(d0root)/graphics_util/sgidi3/ps_crns.f
$(d0root)/graphics_util/sgidi3/ps_init.f:\
  $(d0root)/graphics_util/sgidi3/ps_init.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/ps_init.for | vmstounix > $(d0root)/graphics_util/sgidi3/ps_init.f
$(d0root)/graphics_util/sgidi3/ps_linewi.f:\
  $(d0root)/graphics_util/sgidi3/ps_linewi.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/ps_linewi.for | vmstounix > $(d0root)/graphics_util/sgidi3/ps_linewi.f
$(d0root)/graphics_util/sgidi3/ps_move.f:\
  $(d0root)/graphics_util/sgidi3/ps_move.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/ps_move.for | vmstounix > $(d0root)/graphics_util/sgidi3/ps_move.f
$(d0root)/graphics_util/sgidi3/ps_poly.f:\
  $(d0root)/graphics_util/sgidi3/ps_poly.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/ps_poly.for | vmstounix > $(d0root)/graphics_util/sgidi3/ps_poly.f
$(d0root)/graphics_util/sgidi3/ps_setlin.f:\
  $(d0root)/graphics_util/sgidi3/ps_setlin.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/ps_setlin.for | vmstounix > $(d0root)/graphics_util/sgidi3/ps_setlin.f
$(d0root)/graphics_util/sgidi3/qm_color.f:\
  $(d0root)/graphics_util/sgidi3/qm_color.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/qm_color.for | vmstounix > $(d0root)/graphics_util/sgidi3/qm_color.f
$(d0root)/graphics_util/sgidi3/qm_crns.f:\
  $(d0root)/graphics_util/sgidi3/qm_crns.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/qm_crns.for | vmstounix > $(d0root)/graphics_util/sgidi3/qm_crns.f
$(d0root)/graphics_util/sgidi3/qm_init.f:\
  $(d0root)/graphics_util/sgidi3/qm_init.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/qm_init.for | vmstounix > $(d0root)/graphics_util/sgidi3/qm_init.f
$(d0root)/graphics_util/sgidi3/qm_move.f:\
  $(d0root)/graphics_util/sgidi3/qm_move.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/qm_move.for | vmstounix > $(d0root)/graphics_util/sgidi3/qm_move.f
$(d0root)/graphics_util/sgidi3/qm_poly.f:\
  $(d0root)/graphics_util/sgidi3/qm_poly.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/qm_poly.for | vmstounix > $(d0root)/graphics_util/sgidi3/qm_poly.f
$(d0root)/graphics_util/sgidi3/sgi_j1iget.f:\
  $(d0root)/graphics_util/sgidi3/sgi_j1iget.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_j1iget.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_j1iget.f
$(d0root)/graphics_util/sgidi3/sgi_j1strg.f:\
  $(d0root)/graphics_util/sgidi3/sgi_j1strg.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_j1strg.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_j1strg.f
$(d0root)/graphics_util/sgidi3/sgi_j2strg.f:\
  $(d0root)/graphics_util/sgidi3/sgi_j2strg.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_j2strg.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_j2strg.f
$(d0root)/graphics_util/sgidi3/sgi_j3draw.f:\
  $(d0root)/graphics_util/sgidi3/sgi_j3draw.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_j3draw.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_j3draw.f
$(d0root)/graphics_util/sgidi3/sgi_j3mark.f:\
  $(d0root)/graphics_util/sgidi3/sgi_j3mark.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_j3mark.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_j3mark.f
$(d0root)/graphics_util/sgidi3/sgi_j3move.f:\
  $(d0root)/graphics_util/sgidi3/sgi_j3move.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_j3move.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_j3move.f
$(d0root)/graphics_util/sgidi3/sgi_j3plgn.f:\
  $(d0root)/graphics_util/sgidi3/sgi_j3plgn.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_j3plgn.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_j3plgn.f
$(d0root)/graphics_util/sgidi3/sgi_j3poly.f:\
  $(d0root)/graphics_util/sgidi3/sgi_j3poly.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_j3poly.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_j3poly.f
$(d0root)/graphics_util/sgidi3/sgi_j3rget.f:\
  $(d0root)/graphics_util/sgidi3/sgi_j3rget.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_j3rget.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_j3rget.f
$(d0root)/graphics_util/sgidi3/sgi_j3strg.f:\
  $(d0root)/graphics_util/sgidi3/sgi_j3strg.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc\
  $(d0library)/inc/vaxfont.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_j3strg.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_j3strg.f
$(d0root)/graphics_util/sgidi3/sgi_j4rget.f:\
  $(d0root)/graphics_util/sgidi3/sgi_j4rget.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_j4rget.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_j4rget.f
$(d0root)/graphics_util/sgidi3/sgi_jaload.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jaload.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jaload.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jaload.f
$(d0root)/graphics_util/sgidi3/sgi_jarc.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jarc.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jarc.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jarc.f
$(d0root)/graphics_util/sgidi3/sgi_jarset.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jarset.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jarset.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jarset.f
$(d0root)/graphics_util/sgidi3/sgi_jasave.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jasave.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jasave.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jasave.f
$(d0root)/graphics_util/sgidi3/sgi_jaspek.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jaspek.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jaspek.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jaspek.f
$(d0root)/graphics_util/sgidi3/sgi_jattrb.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jattrb.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jattrb.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jattrb.f
$(d0root)/graphics_util/sgidi3/sgi_jbackg.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jbackg.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jbackg.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jbackg.f
$(d0root)/graphics_util/sgidi3/sgi_jbase.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jbase.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jbase.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jbase.f
$(d0root)/graphics_util/sgidi3/sgi_jbegin.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jbegin.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jbegin.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jbegin.f
$(d0root)/graphics_util/sgidi3/sgi_jbgbat.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jbgbat.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jbgbat.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jbgbat.f
$(d0root)/graphics_util/sgidi3/sgi_jbuild.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jbuild.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jbuild.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jbuild.f
$(d0root)/graphics_util/sgidi3/sgi_jcircl.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jcircl.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jcircl.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jcircl.f
$(d0root)/graphics_util/sgidi3/sgi_jclear.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jclear.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jclear.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jclear.f
$(d0root)/graphics_util/sgidi3/sgi_jclose.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jclose.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jclose.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jclose.f
$(d0root)/graphics_util/sgidi3/sgi_jcmark.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jcmark.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jcmark.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jcmark.f
$(d0root)/graphics_util/sgidi3/sgi_jcolor.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jcolor.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jcolor.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jcolor.f
$(d0root)/graphics_util/sgidi3/sgi_jconpk.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jconpk.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jconpk.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jconpk.f
$(d0root)/graphics_util/sgidi3/sgi_jconvw.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jconvw.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jconvw.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jconvw.f
$(d0root)/graphics_util/sgidi3/sgi_jconwv.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jconwv.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jconwv.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jconwv.f
$(d0root)/graphics_util/sgidi3/sgi_jcotbl.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jcotbl.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jcotbl.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jcotbl.f
$(d0root)/graphics_util/sgidi3/sgi_jcview.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jcview.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jcview.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jcview.f
$(d0root)/graphics_util/sgidi3/sgi_jdbase.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jdbase.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jdbase.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jdbase.f
$(d0root)/graphics_util/sgidi3/sgi_jdcolr.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jdcolr.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jdcolr.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jdcolr.f
$(d0root)/graphics_util/sgidi3/sgi_jdd3d.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jdd3d.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jdd3d.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jdd3d.f
$(d0root)/graphics_util/sgidi3/sgi_jddete.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jddete.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jddete.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jddete.f
$(d0root)/graphics_util/sgidi3/sgi_jdend.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jdend.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jdend.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jdend.f
$(d0root)/graphics_util/sgidi3/sgi_jdetec.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jdetec.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jdetec.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jdetec.f
$(d0root)/graphics_util/sgidi3/sgi_jdevof.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jdevof.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jdevof.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jdevof.f
$(d0root)/graphics_util/sgidi3/sgi_jdevon.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jdevon.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jdevon.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jdevon.f
$(d0root)/graphics_util/sgidi3/sgi_jdevvp.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jdevvp.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jdevvp.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jdevvp.f
$(d0root)/graphics_util/sgidi3/sgi_jdevwn.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jdevwn.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jdevwn.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jdevwn.f
$(d0root)/graphics_util/sgidi3/sgi_jdfont.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jdfont.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jdfont.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jdfont.f
$(d0root)/graphics_util/sgidi3/sgi_jdinit.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jdinit.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jdinit.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jdinit.f
$(d0root)/graphics_util/sgidi3/sgi_jdjust.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jdjust.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jdjust.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jdjust.f
$(d0root)/graphics_util/sgidi3/sgi_jdlsty.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jdlsty.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jdlsty.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jdlsty.f
$(d0root)/graphics_util/sgidi3/sgi_jdlwid.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jdlwid.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jdlwid.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jdlwid.f
$(d0root)/graphics_util/sgidi3/sgi_jdmark.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jdmark.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jdmark.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jdmark.f
$(d0root)/graphics_util/sgidi3/sgi_jdpidx.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jdpidx.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jdpidx.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jdpidx.f
$(d0root)/graphics_util/sgidi3/sgi_jdpint.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jdpint.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jdpint.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jdpint.f
$(d0root)/graphics_util/sgidi3/sgi_jdpinx.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jdpinx.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jdpinx.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jdpinx.f
$(d0root)/graphics_util/sgidi3/sgi_jdpkid.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jdpkid.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jdpkid.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jdpkid.f
$(d0root)/graphics_util/sgidi3/sgi_jdraw.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jdraw.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jdraw.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jdraw.f
$(d0root)/graphics_util/sgidi3/sgi_jdsize.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jdsize.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jdsize.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jdsize.f
$(d0root)/graphics_util/sgidi3/sgi_jdvisb.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jdvisb.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jdvisb.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jdvisb.f
$(d0root)/graphics_util/sgidi3/sgi_jelseg.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jelseg.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jelseg.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jelseg.f
$(d0root)/graphics_util/sgidi3/sgi_jenbat.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jenbat.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jenbat.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jenbat.f
$(d0root)/graphics_util/sgidi3/sgi_jend.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jend.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jend.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jend.f
$(d0root)/graphics_util/sgidi3/sgi_jepseg.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jepseg.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jepseg.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jepseg.f
$(d0root)/graphics_util/sgidi3/sgi_jescap.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jescap.for\
  $(d0library)/params/escape_code_names.def
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jescap.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jescap.f
$(d0root)/graphics_util/sgidi3/sgi_jextnt.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jextnt.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jextnt.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jextnt.f
$(d0root)/graphics_util/sgidi3/sgi_jfiles.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jfiles.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jfiles.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jfiles.f
$(d0root)/graphics_util/sgidi3/sgi_jfont.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jfont.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jfont.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jfont.f
$(d0root)/graphics_util/sgidi3/sgi_jframe.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jframe.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jframe.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jframe.f
$(d0root)/graphics_util/sgidi3/sgi_jfsopn.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jfsopn.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jfsopn.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jfsopn.f
$(d0root)/graphics_util/sgidi3/sgi_jhclip.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jhclip.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jhclip.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jhclip.f
$(d0root)/graphics_util/sgidi3/sgi_jhcpy.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jhcpy.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jhcpy.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jhcpy.f
$(d0root)/graphics_util/sgidi3/sgi_jhilit.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jhilit.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jhilit.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jhilit.f
$(d0root)/graphics_util/sgidi3/sgi_jhithr.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jhithr.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jhithr.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jhithr.f
$(d0root)/graphics_util/sgidi3/sgi_jhstrg.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jhstrg.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jhstrg.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jhstrg.f
$(d0root)/graphics_util/sgidi3/sgi_jidisa.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jidisa.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jidisa.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jidisa.f
$(d0root)/graphics_util/sgidi3/sgi_jienab.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jienab.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jienab.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jienab.f
$(d0root)/graphics_util/sgidi3/sgi_jiescp.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jiescp.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jiescp.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jiescp.f
$(d0root)/graphics_util/sgidi3/sgi_jinten.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jinten.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jinten.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jinten.f
$(d0root)/graphics_util/sgidi3/sgi_jiqddl.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jiqddl.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jiqddl.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jiqddl.f
$(d0root)/graphics_util/sgidi3/sgi_jiqdev.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jiqdev.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jiqdev.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jiqdev.f
$(d0root)/graphics_util/sgidi3/sgi_jiqdil.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jiqdil.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jiqdil.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jiqdil.f
$(d0root)/graphics_util/sgidi3/sgi_jiqdim.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jiqdim.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jiqdim.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jiqdim.f
$(d0root)/graphics_util/sgidi3/sgi_jiqtxt.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jiqtxt.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jiqtxt.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jiqtxt.f
$(d0root)/graphics_util/sgidi3/sgi_jisgmt.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jisgmt.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jisgmt.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jisgmt.f
$(d0root)/graphics_util/sgidi3/sgi_jiwind.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jiwind.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jiwind.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jiwind.f
$(d0root)/graphics_util/sgidi3/sgi_jjust.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jjust.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jjust.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jjust.f
$(d0root)/graphics_util/sgidi3/sgi_jkeybs.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jkeybs.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jkeybs.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jkeybs.f
$(d0root)/graphics_util/sgidi3/sgi_jlocat.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jlocat.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jlocat.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jlocat.f
$(d0root)/graphics_util/sgidi3/sgi_jlstyl.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jlstyl.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jlstyl.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jlstyl.f
$(d0root)/graphics_util/sgidi3/sgi_jlwide.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jlwide.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jlwide.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jlwide.f
$(d0root)/graphics_util/sgidi3/sgi_jmark.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jmark.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jmark.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jmark.f
$(d0root)/graphics_util/sgidi3/sgi_jmodel.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jmodel.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jmodel.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jmodel.f
$(d0root)/graphics_util/sgidi3/sgi_jmodon.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jmodon.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jmodon.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jmodon.f
$(d0root)/graphics_util/sgidi3/sgi_jmove.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jmove.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jmove.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jmove.f
$(d0root)/graphics_util/sgidi3/sgi_jmstrg.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jmstrg.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jmstrg.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jmstrg.f
$(d0root)/graphics_util/sgidi3/sgi_jnorml.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jnorml.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jnorml.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jnorml.f
$(d0root)/graphics_util/sgidi3/sgi_jopen.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jopen.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jopen.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jopen.f
$(d0root)/graphics_util/sgidi3/sgi_jp3mrk.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jp3mrk.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jp3mrk.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jp3mrk.f
$(d0root)/graphics_util/sgidi3/sgi_jparal.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jparal.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jparal.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jparal.f
$(d0root)/graphics_util/sgidi3/sgi_jparob.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jparob.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jparob.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jparob.f
$(d0root)/graphics_util/sgidi3/sgi_jpause.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jpause.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jpause.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jpause.f
$(d0root)/graphics_util/sgidi3/sgi_jpecho.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jpecho.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jpecho.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jpecho.f
$(d0root)/graphics_util/sgidi3/sgi_jpedge.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jpedge.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jpedge.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jpedge.f
$(d0root)/graphics_util/sgidi3/sgi_jperob.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jperob.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jperob.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jperob.f
$(d0root)/graphics_util/sgidi3/sgi_jpersp.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jpersp.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jpersp.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jpersp.f
$(d0root)/graphics_util/sgidi3/sgi_jpick.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jpick.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jpick.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jpick.f
$(d0root)/graphics_util/sgidi3/sgi_jpidex.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jpidex.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jpidex.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jpidex.f
$(d0root)/graphics_util/sgidi3/sgi_jpintr.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jpintr.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jpintr.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jpintr.f
$(d0root)/graphics_util/sgidi3/sgi_jpkapr.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jpkapr.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jpkapr.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jpkapr.f
$(d0root)/graphics_util/sgidi3/sgi_jpkid.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jpkid.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jpkid.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jpkid.f
$(d0root)/graphics_util/sgidi3/sgi_jplane.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jplane.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jplane.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jplane.f
$(d0root)/graphics_util/sgidi3/sgi_jpmark.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jpmark.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jpmark.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jpmark.f
$(d0root)/graphics_util/sgidi3/sgi_jpolgn.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jpolgn.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jpolgn.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jpolgn.f
$(d0root)/graphics_util/sgidi3/sgi_jpoly.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jpoly.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jpoly.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jpoly.f
$(d0root)/graphics_util/sgidi3/sgi_jpr3mr.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jpr3mr.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jpr3mr.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jpr3mr.f
$(d0root)/graphics_util/sgidi3/sgi_jprmrk.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jprmrk.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jprmrk.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jprmrk.f
$(d0root)/graphics_util/sgidi3/sgi_jpurge.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jpurge.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jpurge.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jpurge.f
$(d0root)/graphics_util/sgidi3/sgi_jr3dra.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jr3dra.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jr3dra.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jr3dra.f
$(d0root)/graphics_util/sgidi3/sgi_jr3mov.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jr3mov.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jr3mov.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jr3mov.f
$(d0root)/graphics_util/sgidi3/sgi_jr3mrk.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jr3mrk.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jr3mrk.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jr3mrk.f
$(d0root)/graphics_util/sgidi3/sgi_jr3pgn.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jr3pgn.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jr3pgn.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jr3pgn.f
$(d0root)/graphics_util/sgidi3/sgi_jr3ply.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jr3ply.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jr3ply.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jr3ply.f
$(d0root)/graphics_util/sgidi3/sgi_jrclos.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jrclos.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jrclos.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jrclos.f
$(d0root)/graphics_util/sgidi3/sgi_jrdraw.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jrdraw.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jrdraw.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jrdraw.f
$(d0root)/graphics_util/sgidi3/sgi_jrect.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jrect.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jrect.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jrect.f
$(d0root)/graphics_util/sgidi3/sgi_jright.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jright.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jright.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jright.f
$(d0root)/graphics_util/sgidi3/sgi_jrmark.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jrmark.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jrmark.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jrmark.f
$(d0root)/graphics_util/sgidi3/sgi_jrmove.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jrmove.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jrmove.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jrmove.f
$(d0root)/graphics_util/sgidi3/sgi_jropen.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jropen.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jropen.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jropen.f
$(d0root)/graphics_util/sgidi3/sgi_jrplgn.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jrplgn.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jrplgn.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jrplgn.f
$(d0root)/graphics_util/sgidi3/sgi_jrpoly.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jrpoly.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jrpoly.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jrpoly.f
$(d0root)/graphics_util/sgidi3/sgi_jrrect.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jrrect.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jrrect.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jrrect.f
$(d0root)/graphics_util/sgidi3/sgi_jsaall.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jsaall.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jsaall.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jsaall.f
$(d0root)/graphics_util/sgidi3/sgi_jsasso.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jsasso.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jsasso.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jsasso.f
$(d0root)/graphics_util/sgidi3/sgi_jsectr.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jsectr.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jsectr.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jsectr.f
$(d0root)/graphics_util/sgidi3/sgi_jsetdb.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jsetdb.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jsetdb.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jsetdb.f
$(d0root)/graphics_util/sgidi3/sgi_jseter.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jseter.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jseter.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jseter.f
$(d0root)/graphics_util/sgidi3/sgi_jsgpri.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jsgpri.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jsgpri.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jsgpri.f
$(d0root)/graphics_util/sgidi3/sgi_jsize.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jsize.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jsize.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jsize.f
$(d0root)/graphics_util/sgidi3/sgi_jspick.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jspick.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jspick.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jspick.f
$(d0root)/graphics_util/sgidi3/sgi_jsview.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jsview.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jsview.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jsview.f
$(d0root)/graphics_util/sgidi3/sgi_jt2all.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jt2all.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jt2all.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jt2all.f
$(d0root)/graphics_util/sgidi3/sgi_jtrans.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jtrans.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jtrans.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jtrans.f
$(d0root)/graphics_util/sgidi3/sgi_jttype.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jttype.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jttype.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jttype.f
$(d0root)/graphics_util/sgidi3/sgi_jupvec.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jupvec.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jupvec.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jupvec.f
$(d0root)/graphics_util/sgidi3/sgi_jvisbl.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jvisbl.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jvisbl.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jvisbl.f
$(d0root)/graphics_util/sgidi3/sgi_jvload.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jvload.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jvload.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jvload.f
$(d0root)/graphics_util/sgidi3/sgi_jvport.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jvport.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jvport.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jvport.f
$(d0root)/graphics_util/sgidi3/sgi_jvsave.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jvsave.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jvsave.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jvsave.f
$(d0root)/graphics_util/sgidi3/sgi_jvspac.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jvspac.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jvspac.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jvspac.f
$(d0root)/graphics_util/sgidi3/sgi_jvupln.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jvupln.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jvupln.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jvupln.f
$(d0root)/graphics_util/sgidi3/sgi_jvupnt.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jvupnt.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jvupnt.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jvupnt.f
$(d0root)/graphics_util/sgidi3/sgi_jwclip.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jwclip.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jwclip.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jwclip.f
$(d0root)/graphics_util/sgidi3/sgi_jwindo.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jwindo.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jwindo.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jwindo.f
$(d0root)/graphics_util/sgidi3/sgi_jyclip.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jyclip.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jyclip.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jyclip.f
$(d0root)/graphics_util/sgidi3/sgi_jyon.f:\
  $(d0root)/graphics_util/sgidi3/sgi_jyon.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_jyon.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_jyon.f
$(d0root)/graphics_util/sgidi3/sgi_rotate_3d.f:\
  $(d0root)/graphics_util/sgidi3/sgi_rotate_3d.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pxpara.inc\
  $(d0library)/inc/hdrinx.inc\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/sgi_rotate_3d.for | vmstounix > $(d0root)/graphics_util/sgidi3/sgi_rotate_3d.f
$(d0root)/graphics_util/sgidi3/tk_clear.f:\
  $(d0root)/graphics_util/sgidi3/tk_clear.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/tk_clear.for | vmstounix > $(d0root)/graphics_util/sgidi3/tk_clear.f
$(d0root)/graphics_util/sgidi3/tk_color.f:\
  $(d0root)/graphics_util/sgidi3/tk_color.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/tk_color.for | vmstounix > $(d0root)/graphics_util/sgidi3/tk_color.f
$(d0root)/graphics_util/sgidi3/tk_crns.f:\
  $(d0root)/graphics_util/sgidi3/tk_crns.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/tk_crns.for | vmstounix > $(d0root)/graphics_util/sgidi3/tk_crns.f
$(d0root)/graphics_util/sgidi3/tk_drascr.f:\
  $(d0root)/graphics_util/sgidi3/tk_drascr.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/tk_drascr.for | vmstounix > $(d0root)/graphics_util/sgidi3/tk_drascr.f
$(d0root)/graphics_util/sgidi3/tk_draw.f:\
  $(d0root)/graphics_util/sgidi3/tk_draw.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/tk_draw.for | vmstounix > $(d0root)/graphics_util/sgidi3/tk_draw.f
$(d0root)/graphics_util/sgidi3/tk_fin.f:\
  $(d0root)/graphics_util/sgidi3/tk_fin.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/tk_fin.for | vmstounix > $(d0root)/graphics_util/sgidi3/tk_fin.f
$(d0root)/graphics_util/sgidi3/tk_force.f:\
  $(d0root)/graphics_util/sgidi3/tk_force.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/tk_force.for | vmstounix > $(d0root)/graphics_util/sgidi3/tk_force.f
$(d0root)/graphics_util/sgidi3/tk_line2.f:\
  $(d0root)/graphics_util/sgidi3/tk_line2.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/tk_line2.for | vmstounix > $(d0root)/graphics_util/sgidi3/tk_line2.f
$(d0root)/graphics_util/sgidi3/tk_move.f:\
  $(d0root)/graphics_util/sgidi3/tk_move.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/tk_move.for | vmstounix > $(d0root)/graphics_util/sgidi3/tk_move.f
$(d0root)/graphics_util/sgidi3/tk_movscr.f:\
  $(d0root)/graphics_util/sgidi3/tk_movscr.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/tk_movscr.for | vmstounix > $(d0root)/graphics_util/sgidi3/tk_movscr.f
$(d0root)/graphics_util/sgidi3/tk_out.f:\
  $(d0root)/graphics_util/sgidi3/tk_out.for\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/tk_out.for | vmstounix > $(d0root)/graphics_util/sgidi3/tk_out.f
$(d0root)/graphics_util/sgidi3/tk_poly.f:\
  $(d0root)/graphics_util/sgidi3/tk_poly.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/tk_poly.for | vmstounix > $(d0root)/graphics_util/sgidi3/tk_poly.f
$(d0root)/graphics_util/sgidi3/tk_text.f:\
  $(d0root)/graphics_util/sgidi3/tk_text.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgidi3/tk_text.for | vmstounix > $(d0root)/graphics_util/sgidi3/tk_text.f
for:
	touch $(d0root)/graphics_util/sgidi3/all_display_pixie.for
	touch $(d0root)/graphics_util/sgidi3/close_tkwind.for
	touch $(d0root)/graphics_util/sgidi3/d_dmpmat.for
	touch $(d0root)/graphics_util/sgidi3/d_matcpy.for
	touch $(d0root)/graphics_util/sgidi3/d_matmul.for
	touch $(d0root)/graphics_util/sgidi3/d_matrot.for
	touch $(d0root)/graphics_util/sgidi3/d_matsca.for
	touch $(d0root)/graphics_util/sgidi3/d_mattra.for
	touch $(d0root)/graphics_util/sgidi3/d_matuni.for
	touch $(d0root)/graphics_util/sgidi3/dev_clear.for
	touch $(d0root)/graphics_util/sgidi3/dev_close_window.for
	touch $(d0root)/graphics_util/sgidi3/dev_color.for
	touch $(d0root)/graphics_util/sgidi3/dev_deflin.for
	touch $(d0root)/graphics_util/sgidi3/dev_draw.for
	touch $(d0root)/graphics_util/sgidi3/dev_force.for
	touch $(d0root)/graphics_util/sgidi3/dev_linewi.for
	touch $(d0root)/graphics_util/sgidi3/dev_mark.for
	touch $(d0root)/graphics_util/sgidi3/dev_move.for
	touch $(d0root)/graphics_util/sgidi3/dev_movscr.for
	touch $(d0root)/graphics_util/sgidi3/dev_open_window.for
	touch $(d0root)/graphics_util/sgidi3/dev_page.for
	touch $(d0root)/graphics_util/sgidi3/dev_plin.for
	touch $(d0root)/graphics_util/sgidi3/dev_polf.for
	touch $(d0root)/graphics_util/sgidi3/dev_poly.for
	touch $(d0root)/graphics_util/sgidi3/dev_rot.for
	touch $(d0root)/graphics_util/sgidi3/dev_scale.for
	touch $(d0root)/graphics_util/sgidi3/dev_setlin.for
	touch $(d0root)/graphics_util/sgidi3/dev_transf.for
	touch $(d0root)/graphics_util/sgidi3/dev_transl.for
	touch $(d0root)/graphics_util/sgidi3/dev_window_corners.for
	touch $(d0root)/graphics_util/sgidi3/dmpseg.for
	touch $(d0root)/graphics_util/sgidi3/hcpy_filename.for
	touch $(d0root)/graphics_util/sgidi3/hls_to_rgb.for
	touch $(d0root)/graphics_util/sgidi3/j_cross.for
	touch $(d0root)/graphics_util/sgidi3/j_delsegm.for
	touch $(d0root)/graphics_util/sgidi3/j_devtrn.for
	touch $(d0root)/graphics_util/sgidi3/j_dummies.for
	touch $(d0root)/graphics_util/sgidi3/j_dump.for
	touch $(d0root)/graphics_util/sgidi3/j_grey.for
	touch $(d0root)/graphics_util/sgidi3/j_hcopy_begin.for
	touch $(d0root)/graphics_util/sgidi3/j_hcopy_device.for
	touch $(d0root)/graphics_util/sgidi3/j_hcopy_end.for
	touch $(d0root)/graphics_util/sgidi3/j_menu3d.for
	touch $(d0root)/graphics_util/sgidi3/j_projs.for
	touch $(d0root)/graphics_util/sgidi3/just_comments.for
	touch $(d0root)/graphics_util/sgidi3/open_tkwind.for
	touch $(d0root)/graphics_util/sgidi3/polyn.for
	touch $(d0root)/graphics_util/sgidi3/ps_color.for
	touch $(d0root)/graphics_util/sgidi3/ps_crns.for
	touch $(d0root)/graphics_util/sgidi3/ps_init.for
	touch $(d0root)/graphics_util/sgidi3/ps_linewi.for
	touch $(d0root)/graphics_util/sgidi3/ps_move.for
	touch $(d0root)/graphics_util/sgidi3/ps_poly.for
	touch $(d0root)/graphics_util/sgidi3/ps_setlin.for
	touch $(d0root)/graphics_util/sgidi3/qm_color.for
	touch $(d0root)/graphics_util/sgidi3/qm_crns.for
	touch $(d0root)/graphics_util/sgidi3/qm_init.for
	touch $(d0root)/graphics_util/sgidi3/qm_move.for
	touch $(d0root)/graphics_util/sgidi3/qm_poly.for
	touch $(d0root)/graphics_util/sgidi3/sgi_j1iget.for
	touch $(d0root)/graphics_util/sgidi3/sgi_j1strg.for
	touch $(d0root)/graphics_util/sgidi3/sgi_j2strg.for
	touch $(d0root)/graphics_util/sgidi3/sgi_j3draw.for
	touch $(d0root)/graphics_util/sgidi3/sgi_j3mark.for
	touch $(d0root)/graphics_util/sgidi3/sgi_j3move.for
	touch $(d0root)/graphics_util/sgidi3/sgi_j3plgn.for
	touch $(d0root)/graphics_util/sgidi3/sgi_j3poly.for
	touch $(d0root)/graphics_util/sgidi3/sgi_j3rget.for
	touch $(d0root)/graphics_util/sgidi3/sgi_j3strg.for
	touch $(d0root)/graphics_util/sgidi3/sgi_j4rget.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jaload.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jarc.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jarset.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jasave.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jaspek.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jattrb.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jbackg.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jbase.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jbegin.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jbgbat.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jbuild.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jcircl.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jclear.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jclose.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jcmark.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jcolor.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jconpk.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jconvw.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jconwv.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jcotbl.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jcview.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jdbase.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jdcolr.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jdd3d.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jddete.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jdend.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jdetec.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jdevof.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jdevon.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jdevvp.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jdevwn.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jdfont.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jdinit.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jdjust.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jdlsty.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jdlwid.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jdmark.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jdpidx.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jdpint.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jdpinx.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jdpkid.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jdraw.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jdsize.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jdvisb.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jelseg.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jenbat.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jend.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jepseg.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jescap.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jextnt.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jfiles.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jfont.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jframe.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jfsopn.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jhclip.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jhcpy.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jhilit.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jhithr.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jhstrg.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jidisa.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jienab.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jiescp.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jinten.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jiqddl.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jiqdev.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jiqdil.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jiqdim.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jiqtxt.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jisgmt.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jiwind.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jjust.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jkeybs.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jlocat.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jlstyl.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jlwide.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jmark.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jmodel.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jmodon.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jmove.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jmstrg.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jnorml.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jopen.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jp3mrk.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jparal.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jparob.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jpause.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jpecho.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jpedge.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jperob.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jpersp.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jpick.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jpidex.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jpintr.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jpkapr.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jpkid.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jplane.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jpmark.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jpolgn.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jpoly.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jpr3mr.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jprmrk.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jpurge.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jr3dra.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jr3mov.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jr3mrk.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jr3pgn.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jr3ply.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jrclos.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jrdraw.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jrect.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jright.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jrmark.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jrmove.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jropen.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jrplgn.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jrpoly.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jrrect.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jsaall.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jsasso.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jsectr.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jsetdb.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jseter.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jsgpri.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jsize.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jspick.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jsview.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jt2all.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jtrans.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jttype.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jupvec.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jvisbl.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jvload.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jvport.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jvsave.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jvspac.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jvupln.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jvupnt.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jwclip.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jwindo.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jyclip.for
	touch $(d0root)/graphics_util/sgidi3/sgi_jyon.for
	touch $(d0root)/graphics_util/sgidi3/sgi_rotate_3d.for
	touch $(d0root)/graphics_util/sgidi3/tk_clear.for
	touch $(d0root)/graphics_util/sgidi3/tk_color.for
	touch $(d0root)/graphics_util/sgidi3/tk_crns.for
	touch $(d0root)/graphics_util/sgidi3/tk_drascr.for
	touch $(d0root)/graphics_util/sgidi3/tk_draw.for
	touch $(d0root)/graphics_util/sgidi3/tk_fin.for
	touch $(d0root)/graphics_util/sgidi3/tk_force.for
	touch $(d0root)/graphics_util/sgidi3/tk_line2.for
	touch $(d0root)/graphics_util/sgidi3/tk_move.for
	touch $(d0root)/graphics_util/sgidi3/tk_movscr.for
	touch $(d0root)/graphics_util/sgidi3/tk_out.for
	touch $(d0root)/graphics_util/sgidi3/tk_poly.for
	touch $(d0root)/graphics_util/sgidi3/tk_text.for
