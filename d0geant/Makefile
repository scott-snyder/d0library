SHELL = /bin/sh
CCFLAGS = -c -dollar -G 0 -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0test)/unix/source -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
LCCFLAGS = -c -dollar -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0test)/unix/source -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
F77FLAGS = -c -nocpp -static -col72 -backslash -G 0 -Olimit 2500 -Wo,-loopunroll,2
DEBUG = -O0 -g2
OPT = -O2
ARFLAGS = crsl
SCRATCH = $(d0library)/admin/tmp/d0librar/mmstomake/d0geant
FLAVOR = SIUNIX
F77 = f77
.IGNORE:
.SUFFIXES:
debug :\
  deb_d0geant.a\
  deb_d0geant_farm.a\
  deb_d0geant_uffarm.a\
  deb_d0geantdi.a
deb_d0geant.a : $(d0root)/d0geant/deb_d0geant.a
	@ echo deb_d0geant.a is up to date
$(d0root)/d0geant/deb_d0geant.a:: $(SCRATCH)/d0geant/nothing.nl
$(d0root)/d0geant/deb_d0geant.a::\
  $(d0root)/d0geant/deb_d0geant.a(absorb.o)\
  $(d0root)/d0geant/deb_d0geant.a(anlcal.o)\
  $(d0root)/d0geant/deb_d0geant.a(anlcdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(anlfdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(anllv0.o)\
  $(d0root)/d0geant/deb_d0geant.a(anlmuo.o)\
  $(d0root)/d0geant/deb_d0geant.a(anlsam.o)\
  $(d0root)/d0geant/deb_d0geant.a(anltrd.o)\
  $(d0root)/d0geant/deb_d0geant.a(anlvtx.o)\
  $(d0root)/d0geant/deb_d0geant.a(aphasa.o)\
  $(d0root)/d0geant/deb_d0geant.a(bkd0rg.o)\
  $(d0root)/d0geant/deb_d0geant.a(bkgcah.o)\
  $(d0root)/d0geant/deb_d0geant.a(bkgswt.o)\
  $(d0root)/d0geant/deb_d0geant.a(bkisp2.o)\
  $(d0root)/d0geant/deb_d0geant.a(bkisv2.o)\
  $(d0root)/d0geant/deb_d0geant.a(blcdd1.o)\
  $(d0root)/d0geant/deb_d0geant.a(blcdd2.o)\
  $(d0root)/d0geant/deb_d0geant.a(blcdd3.o)\
  $(d0root)/d0geant/deb_d0geant.a(bldcda.o)\
  $(d0root)/d0geant/deb_d0geant.a(bldsec.o)\
  $(d0root)/d0geant/deb_d0geant.a(blvsec.o)\
  $(d0root)/d0geant/deb_d0geant.a(blvwda.o)\
  $(d0root)/d0geant/deb_d0geant.a(blvzda.o)\
  $(d0root)/d0geant/deb_d0geant.a(blvzla.o)\
  $(d0root)/d0geant/deb_d0geant.a(bokcal.o)\
  $(d0root)/d0geant/deb_d0geant.a(bokcdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(bokfdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(boklv0.o)\
  $(d0root)/d0geant/deb_d0geant.a(bokmuo.o)\
  $(d0root)/d0geant/deb_d0geant.a(boksam.o)\
  $(d0root)/d0geant/deb_d0geant.a(boktrd.o)\
  $(d0root)/d0geant/deb_d0geant.a(bokvtx.o)\
  $(d0root)/d0geant/deb_d0geant.a(cacmpr.o)\
  $(d0root)/d0geant/deb_d0geant.a(calbld.o)\
  $(d0root)/d0geant/deb_d0geant.a(cd_head_flg.o)\
  $(d0root)/d0geant/deb_d0geant.a(cd_hstr_flg.o)\
  $(d0root)/d0geant/deb_d0geant.a(cdatdl.o)\
  $(d0root)/d0geant/deb_d0geant.a(cdatsw.o)\
  $(d0root)/d0geant/deb_d0geant.a(cdcwrt.o)\
  $(d0root)/d0geant/deb_d0geant.a(cdd1fl.o)\
  $(d0root)/d0geant/deb_d0geant.a(cdd2fl.o)\
  $(d0root)/d0geant/deb_d0geant.a(cdd3fl.o)\
  $(d0root)/d0geant/deb_d0geant.a(cdd4fl.o)\
  $(d0root)/d0geant/deb_d0geant.a(cdelay.o)\
  $(d0root)/d0geant/deb_d0geant.a(cdhits.o)\
  $(d0root)/d0geant/deb_d0geant.a(cdresp.o)\
  $(d0root)/d0geant/deb_d0geant.a(cdrift.o)\
  $(d0root)/d0geant/deb_d0geant.a(chgdiv.o)\
  $(d0root)/d0geant/deb_d0geant.a(chkbrr.o)\
  $(d0root)/d0geant/deb_d0geant.a(clanda.o)\
  $(d0root)/d0geant/deb_d0geant.a(clncal.o)\
  $(d0root)/d0geant/deb_d0geant.a(clncdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(clnfdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(clnlv0.o)\
  $(d0root)/d0geant/deb_d0geant.a(clnmuo.o)\
  $(d0root)/d0geant/deb_d0geant.a(clnsam.o)\
  $(d0root)/d0geant/deb_d0geant.a(clntrd.o)\
  $(d0root)/d0geant/deb_d0geant.a(clnvtx.o)\
  $(d0root)/d0geant/deb_d0geant.a(clsmen.o)\
  $(d0root)/d0geant/deb_d0geant.a(clucle.o)\
  $(d0root)/d0geant/deb_d0geant.a(cludel.o)\
  $(d0root)/d0geant/deb_d0geant.a(cludig.o)\
  $(d0root)/d0geant/deb_d0geant.a(clumix.o)\
  $(d0root)/d0geant/deb_d0geant.a(clumrg.o)\
  $(d0root)/d0geant/deb_d0geant.a(clupos.o)\
  $(d0root)/d0geant/deb_d0geant.a(cluprn.o)\
  $(d0root)/d0geant/deb_d0geant.a(clurad.o)\
  $(d0root)/d0geant/deb_d0geant.a(clusmr.o)\
  $(d0root)/d0geant/deb_d0geant.a(clusor.o)\
  $(d0root)/d0geant/deb_d0geant.a(clustf.o)\
  $(d0root)/d0geant/deb_d0geant.a(cpd0rg.o)\
  $(d0root)/d0geant/deb_d0geant.a(d0main.o)\
  $(d0root)/d0geant/deb_d0geant.a(d0xint.o)\
  $(d0root)/d0geant/deb_d0geant.a(datapk.o)\
  $(d0root)/d0geant/deb_d0geant.a(dchit.o)\
  $(d0root)/d0geant/deb_d0geant.a(dehit.o)\
  $(d0root)/d0geant/deb_d0geant.a(deldig.o)\
  $(d0root)/d0geant/deb_d0geant.a(detcal.o)\
  $(d0root)/d0geant/deb_d0geant.a(detcdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(detded.o)\
  $(d0root)/d0geant/deb_d0geant.a(deteca.o)\
  $(d0root)/d0geant/deb_d0geant.a(detfdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(detlv0.o)\
  $(d0root)/d0geant/deb_d0geant.a(detmuo.o)\
  $(d0root)/d0geant/deb_d0geant.a(detmuo_v1.o)\
  $(d0root)/d0geant/deb_d0geant.a(detsam.o)\
  $(d0root)/d0geant/deb_d0geant.a(detscn.o)\
  $(d0root)/d0geant/deb_d0geant.a(dettrd.o)\
  $(d0root)/d0geant/deb_d0geant.a(detuca.o)\
  $(d0root)/d0geant/deb_d0geant.a(detvtx.o)\
  $(d0root)/d0geant/deb_d0geant.a(dffini.o)\
  $(d0root)/d0geant/deb_d0geant.a(dhstor.o)\
  $(d0root)/d0geant/deb_d0geant.a(digcal.o)\
  $(d0root)/d0geant/deb_d0geant.a(digcdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(digded.o)\
  $(d0root)/d0geant/deb_d0geant.a(digeca.o)\
  $(d0root)/d0geant/deb_d0geant.a(digfdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(diglv0.o)\
  $(d0root)/d0geant/deb_d0geant.a(digmuo.o)\
  $(d0root)/d0geant/deb_d0geant.a(digmuo_v1.o)\
  $(d0root)/d0geant/deb_d0geant.a(digsam.o)\
  $(d0root)/d0geant/deb_d0geant.a(digscn.o)\
  $(d0root)/d0geant/deb_d0geant.a(digtrd.o)\
  $(d0root)/d0geant/deb_d0geant.a(diguca.o)\
  $(d0root)/d0geant/deb_d0geant.a(digvtx.o)\
  $(d0root)/d0geant/deb_d0geant.a(dodigi.o)\
  $(d0root)/d0geant/deb_d0geant.a(dschit.o)\
  $(d0root)/d0geant/deb_d0geant.a(dshit.o)\
  $(d0root)/d0geant/deb_d0geant.a(duhit.o)\
  $(d0root)/d0geant/deb_d0geant.a(dvecda.o)\
  $(d0root)/d0geant/deb_d0geant.a(dwdegr.o)\
  $(d0root)/d0geant/deb_d0geant.a(dzemat.o)\
  $(d0root)/d0geant/deb_d0geant.a(dzero.o)\
  $(d0root)/d0geant/deb_d0geant.a(ecindx.o)\
  $(d0root)/d0geant/deb_d0geant.a(edegen.o)\
  $(d0root)/d0geant/deb_d0geant.a(endzeb.o)\
  $(d0root)/d0geant/deb_d0geant.a(enicdh.o)\
  $(d0root)/d0geant/deb_d0geant.a(enjets.o)\
  $(d0root)/d0geant/deb_d0geant.a(enjpts.o)\
  $(d0root)/d0geant/deb_d0geant.a(entrd.o)\
  $(d0root)/d0geant/deb_d0geant.a(exrgen.o)\
  $(d0root)/d0geant/deb_d0geant.a(extrap.o)\
  $(d0root)/d0geant/deb_d0geant.a(fadcdl.o)\
  $(d0root)/d0geant/deb_d0geant.a(fadcfr.o)\
  $(d0root)/d0geant/deb_d0geant.a(fadcsw.o)\
  $(d0root)/d0geant/deb_d0geant.a(fadcvs.o)\
  $(d0root)/d0geant/deb_d0geant.a(fadcvz.o)\
  $(d0root)/d0geant/deb_d0geant.a(fajxyz.o)\
  $(d0root)/d0geant/deb_d0geant.a(fdcwrt.o)\
  $(d0root)/d0geant/deb_d0geant.a(ffical.o)\
  $(d0root)/d0geant/deb_d0geant.a(fficdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(ffid0.o)\
  $(d0root)/d0geant/deb_d0geant.a(ffifdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(ffilv0.o)\
  $(d0root)/d0geant/deb_d0geant.a(ffimuo.o)\
  $(d0root)/d0geant/deb_d0geant.a(ffisam.o)\
  $(d0root)/d0geant/deb_d0geant.a(ffitrd.o)\
  $(d0root)/d0geant/deb_d0geant.a(ffivtx.o)\
  $(d0root)/d0geant/deb_d0geant.a(ficdd1.o)\
  $(d0root)/d0geant/deb_d0geant.a(ficdd2.o)\
  $(d0root)/d0geant/deb_d0geant.a(ficdd3.o)\
  $(d0root)/d0geant/deb_d0geant.a(fidcda.o)\
  $(d0root)/d0geant/deb_d0geant.a(fidsec.o)\
  $(d0root)/d0geant/deb_d0geant.a(fivzda.o)\
  $(d0root)/d0geant/deb_d0geant.a(fld0rg.o)\
  $(d0root)/d0geant/deb_d0geant.a(flgcah.o)\
  $(d0root)/d0geant/deb_d0geant.a(flgval.o)\
  $(d0root)/d0geant/deb_d0geant.a(flicdh.o)\
  $(d0root)/d0geant/deb_d0geant.a(fljets.o)\
  $(d0root)/d0geant/deb_d0geant.a(fljpts.o)\
  $(d0root)/d0geant/deb_d0geant.a(fpdata.o)\
  $(d0root)/d0geant/deb_d0geant.a(fphits.o)\
  $(d0root)/d0geant/deb_d0geant.a(ftdata.o)\
  $(d0root)/d0geant/deb_d0geant.a(ftdlda.o)\
  $(d0root)/d0geant/deb_d0geant.a(fthits.o)\
  $(d0root)/d0geant/deb_d0geant.a(gdcole.o)\
  $(d0root)/d0geant/deb_d0geant.a(geaisa.o)\
  $(d0root)/d0geant/deb_d0geant.a(geecel.o)\
  $(d0root)/d0geant/deb_d0geant.a(geecip.o)\
  $(d0root)/d0geant/deb_d0geant.a(geecmh.o)\
  $(d0root)/d0geant/deb_d0geant.a(geecoh.o)\
  $(d0root)/d0geant/deb_d0geant.a(geocal.o)\
  $(d0root)/d0geant/deb_d0geant.a(geocdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(geod0.o)\
  $(d0root)/d0geant/deb_d0geant.a(geoeca.o)\
  $(d0root)/d0geant/deb_d0geant.a(geoecr.o)\
  $(d0root)/d0geant/deb_d0geant.a(geofdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(geolv0.o)\
  $(d0root)/d0geant/deb_d0geant.a(geomuo.o)\
  $(d0root)/d0geant/deb_d0geant.a(geomvl.o)\
  $(d0root)/d0geant/deb_d0geant.a(geosam.o)\
  $(d0root)/d0geant/deb_d0geant.a(geotrd.o)\
  $(d0root)/d0geant/deb_d0geant.a(geouca.o)\
  $(d0root)/d0geant/deb_d0geant.a(geoucr.o)\
  $(d0root)/d0geant/deb_d0geant.a(geovtx.o)\
  $(d0root)/d0geant/deb_d0geant.a(gescin.o)\
  $(d0root)/d0geant/deb_d0geant.a(get_pulse.o)\
  $(d0root)/d0geant/deb_d0geant.a(get_tract828.o)\
  $(d0root)/d0geant/deb_d0geant.a(getbin.o)\
  $(d0root)/d0geant/deb_d0geant.a(getcel.o)\
  $(d0root)/d0geant/deb_d0geant.a(getdet.o)\
  $(d0root)/d0geant/deb_d0geant.a(geucel.o)\
  $(d0root)/d0geant/deb_d0geant.a(geucfh.o)\
  $(d0root)/d0geant/deb_d0geant.a(geuclk.o)\
  $(d0root)/d0geant/deb_d0geant.a(geumsg.o)\
  $(d0root)/d0geant/deb_d0geant.a(gftver.o)\
  $(d0root)/d0geant/deb_d0geant.a(gfver1.o)\
  $(d0root)/d0geant/deb_d0geant.a(gget.o)\
  $(d0root)/d0geant/deb_d0geant.a(ginvol.o)\
  $(d0root)/d0geant/deb_d0geant.a(gitran.o)\
  $(d0root)/d0geant/deb_d0geant.a(gltrac.o)\
  $(d0root)/d0geant/deb_d0geant.a(gnsout.o)\
  $(d0root)/d0geant/deb_d0geant.a(goking.o)\
  $(d0root)/d0geant/deb_d0geant.a(gsmate.o)\
  $(d0root)/d0geant/deb_d0geant.a(gsmixt.o)\
  $(d0root)/d0geant/deb_d0geant.a(gspart.o)\
  $(d0root)/d0geant/deb_d0geant.a(gsrotm.o)\
  $(d0root)/d0geant/deb_d0geant.a(gtelke.o)\
  $(d0root)/d0geant/deb_d0geant.a(gtimer.o)\
  $(d0root)/d0geant/deb_d0geant.a(gtmedi.o)\
  $(d0root)/d0geant/deb_d0geant.a(gtnino.o)\
  $(d0root)/d0geant/deb_d0geant.a(gtprim.o)\
  $(d0root)/d0geant/deb_d0geant.a(gtrnsf.o)\
  $(d0root)/d0geant/deb_d0geant.a(gtscnd.o)\
  $(d0root)/d0geant/deb_d0geant.a(gtswch.o)\
  $(d0root)/d0geant/deb_d0geant.a(gudigi.o)\
  $(d0root)/d0geant/deb_d0geant.a(gufld.o)\
  $(d0root)/d0geant/deb_d0geant.a(gufld_v1.o)\
  $(d0root)/d0geant/deb_d0geant.a(guhadr.o)\
  $(d0root)/d0geant/deb_d0geant.a(guical.o)\
  $(d0root)/d0geant/deb_d0geant.a(guicdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(guifdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(guiget.o)\
  $(d0root)/d0geant/deb_d0geant.a(guilv0.o)\
  $(d0root)/d0geant/deb_d0geant.a(guimuo.o)\
  $(d0root)/d0geant/deb_d0geant.a(guisam.o)\
  $(d0root)/d0geant/deb_d0geant.a(guitrd.o)\
  $(d0root)/d0geant/deb_d0geant.a(guivtx.o)\
  $(d0root)/d0geant/deb_d0geant.a(gukine.o)\
  $(d0root)/d0geant/deb_d0geant.a(guout.o)\
  $(d0root)/d0geant/deb_d0geant.a(guphad.o)\
  $(d0root)/d0geant/deb_d0geant.a(gupxyz.o)\
  $(d0root)/d0geant/deb_d0geant.a(gustep.o)\
  $(d0root)/d0geant/deb_d0geant.a(gutrak.o)\
  $(d0root)/d0geant/deb_d0geant.a(gutrev.o)\
  $(d0root)/d0geant/deb_d0geant.a(gz_clean.o)\
  $(d0root)/d0geant/deb_d0geant.a(gzd0rg.o)\
  $(d0root)/d0geant/deb_d0geant.a(gzd0rgz.o)\
  $(d0root)/d0geant/deb_d0geant.a(gzgswt.o)\
  $(d0root)/d0geant/deb_d0geant.a(gzgswtz.o)\
  $(d0root)/d0geant/deb_d0geant.a(hcfill.o)\
  $(d0root)/d0geant/deb_d0geant.a(hlpcal.o)\
  $(d0root)/d0geant/deb_d0geant.a(hlpcdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(hlpd0.o)\
  $(d0root)/d0geant/deb_d0geant.a(hlpfdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(hlplv0.o)\
  $(d0root)/d0geant/deb_d0geant.a(hlpmuo.o)\
  $(d0root)/d0geant/deb_d0geant.a(hlpsam.o)\
  $(d0root)/d0geant/deb_d0geant.a(hlptrd.o)\
  $(d0root)/d0geant/deb_d0geant.a(hlpvtx.o)\
  $(d0root)/d0geant/deb_d0geant.a(inical.o)\
  $(d0root)/d0geant/deb_d0geant.a(inicdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(inid0.o)\
  $(d0root)/d0geant/deb_d0geant.a(inifdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(inilv0.o)\
  $(d0root)/d0geant/deb_d0geant.a(inimuo.o)\
  $(d0root)/d0geant/deb_d0geant.a(inisam.o)\
  $(d0root)/d0geant/deb_d0geant.a(initab.o)\
  $(d0root)/d0geant/deb_d0geant.a(initrd.o)\
  $(d0root)/d0geant/deb_d0geant.a(inivtx.o)\
  $(d0root)/d0geant/deb_d0geant.a(intcal.o)\
  $(d0root)/d0geant/deb_d0geant.a(intcdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(intd0.o)\
  $(d0root)/d0geant/deb_d0geant.a(interp.o)\
  $(d0root)/d0geant/deb_d0geant.a(intfdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(intlv0.o)\
  $(d0root)/d0geant/deb_d0geant.a(intmuo.o)\
  $(d0root)/d0geant/deb_d0geant.a(intsam.o)\
  $(d0root)/d0geant/deb_d0geant.a(inttrd.o)\
  $(d0root)/d0geant/deb_d0geant.a(intvtx.o)\
  $(d0root)/d0geant/deb_d0geant.a(isagea.o)\
  $(d0root)/d0geant/deb_d0geant.a(isajerack900.o)\
  $(d0root)/d0geant/deb_d0geant.a(isaje_vtx721.o)\
  $(d0root)/d0geant/deb_d0geant.a(isdrop.o)\
  $(d0root)/d0geant/deb_d0geant.a(iskine.o)\
  $(d0root)/d0geant/deb_d0geant.a(isout.o)\
  $(d0root)/d0geant/deb_d0geant.a(isp1_link.o)\
  $(d0root)/d0geant/deb_d0geant.a(israw.o)\
  $(d0root)/d0geant/deb_d0geant.a(isread.o)\
  $(d0root)/d0geant/deb_d0geant.a(isref.o)\
  $(d0root)/d0geant/deb_d0geant.a(isubfn.o)\
  $(d0root)/d0geant/deb_d0geant.a(iszeb.o)\
  $(d0root)/d0geant/deb_d0geant.a(jetend.o)\
  $(d0root)/d0geant/deb_d0geant.a(jetfil.o)\
  $(d0root)/d0geant/deb_d0geant.a(jetini.o)\
  $(d0root)/d0geant/deb_d0geant.a(kind0.o)\
  $(d0root)/d0geant/deb_d0geant.a(l0_adpect301.o)\
  $(d0root)/d0geant/deb_d0geant.a(l0_pack_adc.o)\
  $(d0root)/d0geant/deb_d0geant.a(l0_paader388.o)\
  $(d0root)/d0geant/deb_d0geant.a(l0_pailer526.o)\
  $(d0root)/d0geant/deb_d0geant.a(l0_partex441.o)\
  $(d0root)/d0geant/deb_d0geant.a(l0_ruetup404.o)\
  $(d0root)/d0geant/deb_d0geant.a(l0_ruetup405.o)\
  $(d0root)/d0geant/deb_d0geant.a(lastfdcstp.o)\
  $(d0root)/d0geant/deb_d0geant.a(lasttrdstp.o)\
  $(d0root)/d0geant/deb_d0geant.a(lddum.o)\
  $(d0root)/d0geant/deb_d0geant.a(ldfpda.o)\
  $(d0root)/d0geant/deb_d0geant.a(ldfpsc.o)\
  $(d0root)/d0geant/deb_d0geant.a(ldftda.o)\
  $(d0root)/d0geant/deb_d0geant.a(ldftsc.o)\
  $(d0root)/d0geant/deb_d0geant.a(ldvsec.o)\
  $(d0root)/d0geant/deb_d0geant.a(ldvwda.o)\
  $(d0root)/d0geant/deb_d0geant.a(ldvzda.o)\
  $(d0root)/d0geant/deb_d0geant.a(ldvzla.o)\
  $(d0root)/d0geant/deb_d0geant.a(locgeo.o)\
  $(d0root)/d0geant/deb_d0geant.a(lstcal.o)\
  $(d0root)/d0geant/deb_d0geant.a(lstcdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(lstfdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(lstlv0.o)\
  $(d0root)/d0geant/deb_d0geant.a(lstmuo.o)\
  $(d0root)/d0geant/deb_d0geant.a(lstsam.o)\
  $(d0root)/d0geant/deb_d0geant.a(lsttrd.o)\
  $(d0root)/d0geant/deb_d0geant.a(lstvtx.o)\
  $(d0root)/d0geant/deb_d0geant.a(lutra1.o)\
  $(d0root)/d0geant/deb_d0geant.a(mcalexit.o)\
  $(d0root)/d0geant/deb_d0geant.a(mencal.o)\
  $(d0root)/d0geant/deb_d0geant.a(mencdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(mend0.o)\
  $(d0root)/d0geant/deb_d0geant.a(menfdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(menlv0.o)\
  $(d0root)/d0geant/deb_d0geant.a(menmuo.o)\
  $(d0root)/d0geant/deb_d0geant.a(mensam.o)\
  $(d0root)/d0geant/deb_d0geant.a(mentrd.o)\
  $(d0root)/d0geant/deb_d0geant.a(menvtx.o)\
  $(d0root)/d0geant/deb_d0geant.a(mkdcdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(mkfphi.o)\
  $(d0root)/d0geant/deb_d0geant.a(mkfthe.o)\
  $(d0root)/d0geant/deb_d0geant.a(mkvtxs.o)\
  $(d0root)/d0geant/deb_d0geant.a(mkvtxw.o)\
  $(d0root)/d0geant/deb_d0geant.a(mscrat.o)\
  $(d0root)/d0geant/deb_d0geant.a(msdelt.o)\
  $(d0root)/d0geant/deb_d0geant.a(msdrft.o)\
  $(d0root)/d0geant/deb_d0geant.a(msgnum.o)\
  $(d0root)/d0geant/deb_d0geant.a(mshits.o)\
  $(d0root)/d0geant/deb_d0geant.a(msindx.o)\
  $(d0root)/d0geant/deb_d0geant.a(msindx_v1.o)\
  $(d0root)/d0geant/deb_d0geant.a(msmud1.o)\
  $(d0root)/d0geant/deb_d0geant.a(msmud1_v1.o)\
  $(d0root)/d0geant/deb_d0geant.a(msmudh.o)\
  $(d0root)/d0geant/deb_d0geant.a(mspad.o)\
  $(d0root)/d0geant/deb_d0geant.a(mspad_v1.o)\
  $(d0root)/d0geant/deb_d0geant.a(mspadr.o)\
  $(d0root)/d0geant/deb_d0geant.a(mspads.o)\
  $(d0root)/d0geant/deb_d0geant.a(mstrak.o)\
  $(d0root)/d0geant/deb_d0geant.a(mstvc.o)\
  $(d0root)/d0geant/deb_d0geant.a(mstvc_v1.o)\
  $(d0root)/d0geant/deb_d0geant.a(msvchk.o)\
  $(d0root)/d0geant/deb_d0geant.a(mudrft.o)\
  $(d0root)/d0geant/deb_d0geant.a(mxcal.o)\
  $(d0root)/d0geant/deb_d0geant.a(mxlv0.o)\
  $(d0root)/d0geant/deb_d0geant.a(norran.o)\
  $(d0root)/d0geant/deb_d0geant.a(nran.o)\
  $(d0root)/d0geant/deb_d0geant.a(nxtzst.o)\
  $(d0root)/d0geant/deb_d0geant.a(pcolor.o)\
  $(d0root)/d0geant/deb_d0geant.a(prgswt.o)\
  $(d0root)/d0geant/deb_d0geant.a(print_step.o)\
  $(d0root)/d0geant/deb_d0geant.a(prmmap.o)\
  $(d0root)/d0geant/deb_d0geant.a(prune_gskine.o)\
  $(d0root)/d0geant/deb_d0geant.a(rdzeb.o)\
  $(d0root)/d0geant/deb_d0geant.a(rotino.o)\
  $(d0root)/d0geant/deb_d0geant.a(rrlam.o)\
  $(d0root)/d0geant/deb_d0geant.a(rwxspc.o)\
  $(d0root)/d0geant/deb_d0geant.a(safill.o)\
  $(d0root)/d0geant/deb_d0geant.a(saicmp.o)\
  $(d0root)/d0geant/deb_d0geant.a(sam_bp.o)\
  $(d0root)/d0geant/deb_d0geant.a(sam_ma.o)\
  $(d0root)/d0geant/deb_d0geant.a(samadc.o)\
  $(d0root)/d0geant/deb_d0geant.a(samgt1.o)\
  $(d0root)/d0geant/deb_d0geant.a(samgt2.o)\
  $(d0root)/d0geant/deb_d0geant.a(saname.o)\
  $(d0root)/d0geant/deb_d0geant.a(saslb1.o)\
  $(d0root)/d0geant/deb_d0geant.a(saslb2.o)\
  $(d0root)/d0geant/deb_d0geant.a(sastat.o)\
  $(d0root)/d0geant/deb_d0geant.a(satube.o)\
  $(d0root)/d0geant/deb_d0geant.a(set_epars830.o)\
  $(d0root)/d0geant/deb_d0geant.a(set_saram725.o)\
  $(d0root)/d0geant/deb_d0geant.a(setdet.o)\
  $(d0root)/d0geant/deb_d0geant.a(setrot.o)\
  $(d0root)/d0geant/deb_d0geant.a(setvew.o)\
  $(d0root)/d0geant/deb_d0geant.a(shcopy.o)\
  $(d0root)/d0geant/deb_d0geant.a(shinit.o)\
  $(d0root)/d0geant/deb_d0geant.a(shlast.o)\
  $(d0root)/d0geant/deb_d0geant.a(shlb_cal_xyz.o)\
  $(d0root)/d0geant/deb_d0geant.a(shlb__key983.o)\
  $(d0root)/d0geant/deb_d0geant.a(shlb_rack597.o)\
  $(d0root)/d0geant/deb_d0geant.a(simpul.o)\
  $(d0root)/d0geant/deb_d0geant.a(slopan.o)\
  $(d0root)/d0geant/deb_d0geant.a(smrfac.o)\
  $(d0root)/d0geant/deb_d0geant.a(snapshot.o)\
  $(d0root)/d0geant/deb_d0geant.a(splgar.o)\
  $(d0root)/d0geant/deb_d0geant.a(stolv0.o)\
  $(d0root)/d0geant/deb_d0geant.a(store_mcen.o)\
  $(d0root)/d0geant/deb_d0geant.a(stpcal.o)\
  $(d0root)/d0geant/deb_d0geant.a(stpcdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(stpd0.o)\
  $(d0root)/d0geant/deb_d0geant.a(stpfdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(stplv0.o)\
  $(d0root)/d0geant/deb_d0geant.a(stpmuo.o)\
  $(d0root)/d0geant/deb_d0geant.a(stpmuo_v1.o)\
  $(d0root)/d0geant/deb_d0geant.a(stpsam.o)\
  $(d0root)/d0geant/deb_d0geant.a(stptrd.o)\
  $(d0root)/d0geant/deb_d0geant.a(stpvtx.o)\
  $(d0root)/d0geant/deb_d0geant.a(stswch.o)\
  $(d0root)/d0geant/deb_d0geant.a(stzdib.o)\
  $(d0root)/d0geant/deb_d0geant.a(stzdiv.o)\
  $(d0root)/d0geant/deb_d0geant.a(stzdv1.o)\
  $(d0root)/d0geant/deb_d0geant.a(super_gswt.o)\
  $(d0root)/d0geant/deb_d0geant.a(svdlay.o)\
  $(d0root)/d0geant/deb_d0geant.a(tevcal.o)\
  $(d0root)/d0geant/deb_d0geant.a(tevcdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(tevfdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(tevlv0.o)\
  $(d0root)/d0geant/deb_d0geant.a(tevmuo.o)\
  $(d0root)/d0geant/deb_d0geant.a(tevsam.o)\
  $(d0root)/d0geant/deb_d0geant.a(tevtrd.o)\
  $(d0root)/d0geant/deb_d0geant.a(tevvtx.o)\
  $(d0root)/d0geant/deb_d0geant.a(tlayer.o)\
  $(d0root)/d0geant/deb_d0geant.a(trabsl.o)\
  $(d0root)/d0geant/deb_d0geant.a(trafra.o)\
  $(d0root)/d0geant/deb_d0geant.a(trchan.o)\
  $(d0root)/d0geant/deb_d0geant.a(trdblk.o)\
  $(d0root)/d0geant/deb_d0geant.a(trdfmy.o)\
  $(d0root)/d0geant/deb_d0geant.a(trdshp.o)\
  $(d0root)/d0geant/deb_d0geant.a(trdtim.o)\
  $(d0root)/d0geant/deb_d0geant.a(trdunp.o)\
  $(d0root)/d0geant/deb_d0geant.a(trdwde.o)\
  $(d0root)/d0geant/deb_d0geant.a(trdyld.o)\
  $(d0root)/d0geant/deb_d0geant.a(trgpmc.o)\
  $(d0root)/d0geant/deb_d0geant.a(trhigh.o)\
  $(d0root)/d0geant/deb_d0geant.a(trkcal.o)\
  $(d0root)/d0geant/deb_d0geant.a(trkcdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(trkfdc.o)\
  $(d0root)/d0geant/deb_d0geant.a(trklv0.o)\
  $(d0root)/d0geant/deb_d0geant.a(trkmuo.o)\
  $(d0root)/d0geant/deb_d0geant.a(trksam.o)\
  $(d0root)/d0geant/deb_d0geant.a(trktrd.o)\
  $(d0root)/d0geant/deb_d0geant.a(trkvtx.o)\
  $(d0root)/d0geant/deb_d0geant.a(trnumb.o)\
  $(d0root)/d0geant/deb_d0geant.a(trtabl.o)\
  $(d0root)/d0geant/deb_d0geant.a(ucindx.o)\
  $(d0root)/d0geant/deb_d0geant.a(uginit.o)\
  $(d0root)/d0geant/deb_d0geant.a(uglast.o)\
  $(d0root)/d0geant/deb_d0geant.a(use_mulib.o)\
  $(d0root)/d0geant/deb_d0geant.a(use_mrary642.o)\
  $(d0root)/d0geant/deb_d0geant.a(use_shlb.o)\
  $(d0root)/d0geant/deb_d0geant.a(use_srary859.o)\
  $(d0root)/d0geant/deb_d0geant.a(vdzero.o)\
  $(d0root)/d0geant/deb_d0geant.a(vlanda.o)\
  $(d0root)/d0geant/deb_d0geant.a(volord.o)\
  $(d0root)/d0geant/deb_d0geant.a(volpos.o)\
  $(d0root)/d0geant/deb_d0geant.a(volpoz.o)\
  $(d0root)/d0geant/deb_d0geant.a(vtxpul.o)\
  $(d0root)/d0geant/deb_d0geant.a(vtxwrt.o)\
  $(d0root)/d0geant/deb_d0geant.a(wrzeb.o)\
  $(d0root)/d0geant/deb_d0geant.a(xinter.o)\
  $(d0root)/d0geant/deb_d0geant.a(xlum.o)\
  $(d0root)/d0geant/deb_d0geant.a(xrad.o)\
  $(d0root)/d0geant/deb_d0geant.a(zadped.o)\
  $(d0root)/d0geant/deb_d0geant.a(zcnvrt.o)\
  $(d0root)/d0geant/deb_d0geant.a(zebfin.o)\
  $(d0root)/d0geant/deb_d0geant.a(zebini.o)\
  $(d0root)/d0geant/deb_d0geant.a(zebout.o)\
  $(d0root)/d0geant/deb_d0geant.a(zersup.o)\
  $(d0root)/d0geant/deb_d0geant.a(zinduc.o)\
  $(d0root)/d0geant/deb_d0geant.a(zsinit.o)\
  $(d0root)/d0geant/deb_d0geant.a(zstrht.o)\
  $(d0root)/d0geant/deb_d0geant.a(zstrip.o)\
  $(d0root)/d0geant/deb_d0geant.a(zstrno.o)
	ar $(ARFLAGS) $(d0root)/d0geant/deb_d0geant.a `ls $(SCRATCH)/d0geant/a1/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/d0geant/deb_d0geant.a `ls $(SCRATCH)/d0geant/a2/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/d0geant/deb_d0geant.a `ls $(SCRATCH)/d0geant/a3/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/d0geant/deb_d0geant.a `ls $(SCRATCH)/d0geant/a4/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/d0geant/deb_d0geant.a `ls $(SCRATCH)/d0geant/a5/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/d0geant/deb_d0geant.a `ls $(SCRATCH)/d0geant/a6/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/d0geant/deb_d0geant.a `ls $(SCRATCH)/d0geant/a7/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/d0geant/deb_d0geant.a `ls $(SCRATCH)/d0geant/a8/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/d0geant/deb_d0geant.a `ls $(SCRATCH)/d0geant/a9/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/d0geant/deb_d0geant.a `ls $(SCRATCH)/d0geant/a10/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/d0geant/deb_d0geant.a `ls $(SCRATCH)/d0geant/a11/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/d0geant/deb_d0geant.a `ls $(SCRATCH)/d0geant/a12/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/d0geant/deb_d0geant.a `ls $(SCRATCH)/d0geant/a13/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/d0geant/deb_d0geant.a `ls $(SCRATCH)/d0geant/a14/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/d0geant/deb_d0geant.a `ls $(SCRATCH)/d0geant/a15/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/d0geant/deb_d0geant.a `ls $(SCRATCH)/d0geant/a16/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/d0geant/deb_d0geant.a `ls $(SCRATCH)/d0geant/a17/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/d0geant/deb_d0geant.a `ls $(SCRATCH)/d0geant/a18/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/d0geant/deb_d0geant.a `ls $(SCRATCH)/d0geant/a19/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/d0geant/deb_d0geant.a `ls $(SCRATCH)/d0geant/a20/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/d0geant/deb_d0geant.a `ls $(SCRATCH)/d0geant/a21/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/d0geant/deb_d0geant.a `ls $(SCRATCH)/d0geant/a22/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/d0geant/deb_d0geant.a `ls $(SCRATCH)/d0geant/a23/*.o 2> /dev/null`
	rmdirp $(SCRATCH)/d0geant
$(SCRATCH)/d0geant/nothing.nl:
	rmdirp $(SCRATCH)/d0geant
	mkdirp $(SCRATCH)/d0geant/a1
	mkdirp $(SCRATCH)/d0geant/a2
	mkdirp $(SCRATCH)/d0geant/a3
	mkdirp $(SCRATCH)/d0geant/a4
	mkdirp $(SCRATCH)/d0geant/a5
	mkdirp $(SCRATCH)/d0geant/a6
	mkdirp $(SCRATCH)/d0geant/a7
	mkdirp $(SCRATCH)/d0geant/a8
	mkdirp $(SCRATCH)/d0geant/a9
	mkdirp $(SCRATCH)/d0geant/a10
	mkdirp $(SCRATCH)/d0geant/a11
	mkdirp $(SCRATCH)/d0geant/a12
	mkdirp $(SCRATCH)/d0geant/a13
	mkdirp $(SCRATCH)/d0geant/a14
	mkdirp $(SCRATCH)/d0geant/a15
	mkdirp $(SCRATCH)/d0geant/a16
	mkdirp $(SCRATCH)/d0geant/a17
	mkdirp $(SCRATCH)/d0geant/a18
	mkdirp $(SCRATCH)/d0geant/a19
	mkdirp $(SCRATCH)/d0geant/a20
	mkdirp $(SCRATCH)/d0geant/a21
	mkdirp $(SCRATCH)/d0geant/a22
	mkdirp $(SCRATCH)/d0geant/a23
$(d0root)/d0geant/deb_d0geant.a(absorb.o):\
  $(d0root)/d0geant/cal/absorb.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gconst.inc\
  $(d0library)/inc/gctmed.inc\
  $(d0library)/inc/absln.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/absorb.f ;\
	mv absorb.o $(SCRATCH)/d0geant/a1/absorb.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(anlcal.o):\
  $(d0root)/d0geant/cal/anlcal.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/anlcal.f ;\
	mv anlcal.o $(SCRATCH)/d0geant/a1/anlcal.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(anlcdc.o):\
  $(d0root)/d0geant/cdc/anlcdc.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/anlcdc.f ;\
	mv anlcdc.o $(SCRATCH)/d0geant/a1/anlcdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(anlfdc.o):\
  $(d0root)/d0geant/fdc/anlfdc.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/anlfdc.f ;\
	mv anlfdc.o $(SCRATCH)/d0geant/a1/anlfdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(anllv0.o):\
  $(d0root)/d0geant/lv0/anllv0.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/lv0/anllv0.f ;\
	mv anllv0.o $(SCRATCH)/d0geant/a1/anllv0.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(anlmuo.o):\
  $(d0root)/d0geant/muo/anlmuo.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/anlmuo.f ;\
	mv anlmuo.o $(SCRATCH)/d0geant/a1/anlmuo.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(anlsam.o):\
  $(d0root)/d0geant/samus/anlsam.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/anlsam.f ;\
	mv anlsam.o $(SCRATCH)/d0geant/a1/anlsam.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(anltrd.o):\
  $(d0root)/d0geant/trd/anltrd.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/trhitw.inc\
  $(d0library)/inc/trinfo.inc\
  $(d0library)/inc/worksp.inc\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/anltrd.f ;\
	mv anltrd.o $(SCRATCH)/d0geant/a1/anltrd.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(anlvtx.o):\
  $(d0root)/d0geant/vtx/anlvtx.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/anlvtx.f ;\
	mv anlvtx.o $(SCRATCH)/d0geant/a1/anlvtx.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(aphasa.o):\
  $(d0root)/d0geant/trd/aphasa.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/aphasa.f ;\
	mv aphasa.o $(SCRATCH)/d0geant/a1/aphasa.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(bkd0rg.o):\
  $(d0root)/d0geant/utl/bkd0rg.f\
  $(d0library)/links/izd0rg.link\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gcmail.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/utl/bkd0rg.f ;\
	mv bkd0rg.o $(SCRATCH)/d0geant/a1/bkd0rg.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(bkgcah.o):\
  $(d0root)/d0geant/cal/bkgcah.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izgcah.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/bkgcah.f ;\
	mv bkgcah.o $(SCRATCH)/d0geant/a1/bkgcah.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(bkgswt.o):\
  $(d0root)/d0geant/dzero/bkgswt.f\
  $(d0library)/links/izgswt.link\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gcmail.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/bkgswt.f ;\
	mv bkgswt.o $(SCRATCH)/d0geant/a1/bkgswt.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(bkisp2.o):\
  $(d0root)/d0geant/zebio/bkisp2.f\
  $(d0library)/links/izisp2.link\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/bkisp2.f ;\
	mv bkisp2.o $(SCRATCH)/d0geant/a1/bkisp2.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(bkisv2.o):\
  $(d0root)/d0geant/zebio/bkisv2.f\
  $(d0library)/links/izisae.link\
  $(d0library)/links/izisv2.link\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/bkisv2.f ;\
	mv bkisv2.o $(SCRATCH)/d0geant/a1/bkisv2.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(blcdd1.o):\
  $(d0root)/d0geant/vtx/blcdd1.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/links/izvrft.link\
  $(d0library)/inc/vtxlnk.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/vtxpsh.inc\
  $(d0library)/inc/vtloca.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/blcdd1.f ;\
	mv blcdd1.o $(SCRATCH)/d0geant/a1/blcdd1.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(blcdd2.o):\
  $(d0root)/d0geant/cdc/blcdd2.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/links/izdrft.link\
  $(d0library)/inc/cdclnk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/blcdd2.f ;\
	mv blcdd2.o $(SCRATCH)/d0geant/a1/blcdd2.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(blcdd3.o):\
  $(d0root)/d0geant/fdc/blcdd3.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/fdclnk.inc\
  $(d0library)/links/izfdrt.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/blcdd3.f ;\
	mv blcdd3.o $(SCRATCH)/d0geant/a1/blcdd3.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(bldcda.o):\
  $(d0root)/d0geant/cdc/bldcda.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/bldcda.f ;\
	mv bldcda.o $(SCRATCH)/d0geant/a1/bldcda.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(bldsec.o):\
  $(d0root)/d0geant/cdc/bldsec.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/bldsec.f ;\
	mv bldsec.o $(SCRATCH)/d0geant/a1/bldsec.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(blvsec.o):\
  $(d0root)/d0geant/vtx/blvsec.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/vtloca.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/blvsec.f ;\
	mv blvsec.o $(SCRATCH)/d0geant/a2/blvsec.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(blvwda.o):\
  $(d0root)/d0geant/vtx/blvwda.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/vtloca.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/blvwda.f ;\
	mv blvwda.o $(SCRATCH)/d0geant/a2/blvwda.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(blvzda.o):\
  $(d0root)/d0geant/vtx/blvzda.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/vzdata.inc\
  $(d0library)/inc/vtloca.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/blvzda.f ;\
	mv blvzda.o $(SCRATCH)/d0geant/a2/blvzda.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(blvzla.o):\
  $(d0root)/d0geant/vtx/blvzla.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/vzhits.inc\
  $(d0library)/inc/vtloca.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/blvzla.f ;\
	mv blvzla.o $(SCRATCH)/d0geant/a2/blvzla.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(bokcal.o):\
  $(d0root)/d0geant/cal/bokcal.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/bokcal.f ;\
	mv bokcal.o $(SCRATCH)/d0geant/a2/bokcal.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(bokcdc.o):\
  $(d0root)/d0geant/cdc/bokcdc.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/bokcdc.f ;\
	mv bokcdc.o $(SCRATCH)/d0geant/a2/bokcdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(bokfdc.o):\
  $(d0root)/d0geant/fdc/bokfdc.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/bokfdc.f ;\
	mv bokfdc.o $(SCRATCH)/d0geant/a2/bokfdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(boklv0.o):\
  $(d0root)/d0geant/lv0/boklv0.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/lv0/boklv0.f ;\
	mv boklv0.o $(SCRATCH)/d0geant/a2/boklv0.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(bokmuo.o):\
  $(d0root)/d0geant/muo/bokmuo.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/bokmuo.f ;\
	mv bokmuo.o $(SCRATCH)/d0geant/a2/bokmuo.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(boksam.o):\
  $(d0root)/d0geant/samus/boksam.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/boksam.f ;\
	mv boksam.o $(SCRATCH)/d0geant/a2/boksam.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(boktrd.o):\
  $(d0root)/d0geant/trd/boktrd.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/trinfo.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/boktrd.f ;\
	mv boktrd.o $(SCRATCH)/d0geant/a2/boktrd.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(bokvtx.o):\
  $(d0root)/d0geant/vtx/bokvtx.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/bokvtx.f ;\
	mv bokvtx.o $(SCRATCH)/d0geant/a2/bokvtx.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(cacmpr.o):\
  $(d0root)/d0geant/caltower/cacmpr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/caltower/cacmpr.f ;\
	mv cacmpr.o $(SCRATCH)/d0geant/a2/cacmpr.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(calbld.o):\
  $(d0root)/d0geant/caltower/calbld.f\
  $(d0library)/inc/caltrk.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gconst.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/hcal.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/caltower/calbld.f ;\
	mv calbld.o $(SCRATCH)/d0geant/a2/calbld.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(cd_head_flg.o):\
  $(d0root)/d0geant/utl/cd_head_flg.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/params/byte_order.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/utl/cd_head_flg.f ;\
	mv cd_head_flg.o $(SCRATCH)/d0geant/a2/cd_head_flg.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(cd_hstr_flg.o):\
  $(d0root)/d0geant/zebio/cd_hstr_flg.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/params/byte_order.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/cd_hstr_flg.f ;\
	mv cd_hstr_flg.o $(SCRATCH)/d0geant/a2/cd_hstr_flg.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(cdatdl.o):\
  $(d0root)/d0geant/cdc/cdatdl.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/links/izdalh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/cdatdl.f ;\
	mv cdatdl.o $(SCRATCH)/d0geant/a2/cdatdl.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(cdatsw.o):\
  $(d0root)/d0geant/cdc/cdatsw.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdalh.link\
  $(d0library)/inc/gcunit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/cdatsw.f ;\
	mv cdatsw.o $(SCRATCH)/d0geant/a2/cdatsw.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(cdcwrt.o):\
  $(d0root)/d0geant/cdc/cdcwrt.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/cdclnk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/cdcwrt.f ;\
	mv cdcwrt.o $(SCRATCH)/d0geant/a2/cdcwrt.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(cdd1fl.o):\
  $(d0root)/d0geant/vtx/cdd1fl.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/links/izcdd1.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/cdd1fl.f ;\
	mv cdd1fl.o $(SCRATCH)/d0geant/a2/cdd1fl.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(cdd2fl.o):\
  $(d0root)/d0geant/cdc/cdd2fl.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/links/izcdd2.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/cdd2fl.f ;\
	mv cdd2fl.o $(SCRATCH)/d0geant/a3/cdd2fl.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(cdd3fl.o):\
  $(d0root)/d0geant/fdc/cdd3fl.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/links/izcdd3.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/cdd3fl.f ;\
	mv cdd3fl.o $(SCRATCH)/d0geant/a3/cdd3fl.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(cdd4fl.o):\
  $(d0root)/d0geant/trd/cdd4fl.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/links/izcdd4.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/cdd4fl.f ;\
	mv cdd4fl.o $(SCRATCH)/d0geant/a3/cdd4fl.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(cdelay.o):\
  $(d0root)/d0geant/cdc/cdelay.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdrft.link\
  $(d0library)/inc/gcunit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/cdelay.f ;\
	mv cdelay.o $(SCRATCH)/d0geant/a3/cdelay.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(cdhits.o):\
  $(d0root)/d0geant/cdc/cdhits.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/links/izdalh.link\
  $(d0library)/links/izdrft.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/cdhits.f ;\
	mv cdhits.o $(SCRATCH)/d0geant/a3/cdhits.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(cdresp.o):\
  $(d0root)/d0geant/cdc/cdresp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/cdresp.f ;\
	mv cdresp.o $(SCRATCH)/d0geant/a3/cdresp.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(cdrift.o):\
  $(d0root)/d0geant/cdc/cdrift.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/cdrift.f ;\
	mv cdrift.o $(SCRATCH)/d0geant/a3/cdrift.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(chgdiv.o):\
  $(d0root)/d0geant/vtx/chgdiv.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/chgdiv.f ;\
	mv chgdiv.o $(SCRATCH)/d0geant/a3/chgdiv.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(chkbrr.o):\
  $(d0root)/d0geant/zebio/chkbrr.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izd0rgz.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/chkbrr.f ;\
	mv chkbrr.o $(SCRATCH)/d0geant/a3/chkbrr.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(clanda.o):\
  $(d0root)/d0geant/cdc/clanda.f\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gckine.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/clanda.f ;\
	mv clanda.o $(SCRATCH)/d0geant/a3/clanda.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(clncal.o):\
  $(d0root)/d0geant/cal/clncal.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/dcalog.inc\
  $(d0library)/inc/drop.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/clncal.f ;\
	mv clncal.o $(SCRATCH)/d0geant/a3/clncal.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(clncdc.o):\
  $(d0root)/d0geant/cdc/clncdc.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/clncdc.f ;\
	mv clncdc.o $(SCRATCH)/d0geant/a3/clncdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(clnfdc.o):\
  $(d0root)/d0geant/fdc/clnfdc.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/clnfdc.f ;\
	mv clnfdc.o $(SCRATCH)/d0geant/a3/clnfdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(clnlv0.o):\
  $(d0root)/d0geant/lv0/clnlv0.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/lv0/clnlv0.f ;\
	mv clnlv0.o $(SCRATCH)/d0geant/a3/clnlv0.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(clnmuo.o):\
  $(d0root)/d0geant/muo/clnmuo.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/clnmuo.f ;\
	mv clnmuo.o $(SCRATCH)/d0geant/a3/clnmuo.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(clnsam.o):\
  $(d0root)/d0geant/samus/clnsam.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/clnsam.f ;\
	mv clnsam.o $(SCRATCH)/d0geant/a3/clnsam.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(clntrd.o):\
  $(d0root)/d0geant/trd/clntrd.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/clntrd.f ;\
	mv clntrd.o $(SCRATCH)/d0geant/a3/clntrd.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(clnvtx.o):\
  $(d0root)/d0geant/vtx/clnvtx.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/clnvtx.f ;\
	mv clnvtx.o $(SCRATCH)/d0geant/a3/clnvtx.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(clsmen.o):\
  $(d0root)/d0geant/trd/clsmen.f\
  $(d0library)/inc/nortrd.inc\
  $(d0library)/inc/prnevt.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/clsmen.f ;\
	mv clsmen.o $(SCRATCH)/d0geant/a3/clsmen.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(clucle.o):\
  $(d0root)/d0geant/trd/clucle.f\
  $(d0library)/inc/clusm.inc\
  $(d0library)/inc/clustr.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/clucle.f ;\
	mv clucle.o $(SCRATCH)/d0geant/a3/clucle.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(cludel.o):\
  $(d0root)/d0geant/trd/cludel.f\
  $(d0library)/inc/clucon.inc\
  $(d0library)/inc/clugen.inc\
  $(d0library)/inc/clusm.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/enetrd.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/info.inc\
  $(d0library)/inc/probde.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/cludel.f ;\
	mv cludel.o $(SCRATCH)/d0geant/a4/cludel.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(cludig.o):\
  $(d0root)/d0geant/trd/cludig.f\
  $(d0library)/inc/clusm.inc\
  $(d0library)/inc/clustr.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/enetrd.inc\
  $(d0library)/inc/faddig.inc\
  $(d0library)/inc/fadccn.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/geomtc.inc\
  $(d0library)/inc/geomtr.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/nortrd.inc\
  $(d0library)/inc/posit.inc\
  $(d0library)/inc/prnevt.inc\
  $(d0library)/inc/trinfo.inc\
  $(d0library)/inc/worksp.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gcflag.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/cludig.f ;\
	mv cludig.o $(SCRATCH)/d0geant/a4/cludig.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(clumix.o):\
  $(d0root)/d0geant/trd/clumix.f\
  $(d0library)/inc/clusm.inc\
  $(d0library)/inc/worksp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/clumix.f ;\
	mv clumix.o $(SCRATCH)/d0geant/a4/clumix.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(clumrg.o):\
  $(d0root)/d0geant/trd/clumrg.f\
  $(d0library)/inc/clusm.inc\
  $(d0library)/inc/clustr.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/info.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/clumrg.f ;\
	mv clumrg.o $(SCRATCH)/d0geant/a4/clumrg.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(clupos.o):\
  $(d0root)/d0geant/trd/clupos.f\
  $(d0library)/inc/clusm.inc\
  $(d0library)/inc/clustr.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gconst.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/geomtc.inc\
  $(d0library)/inc/geomtr.inc\
  $(d0library)/inc/info.inc\
  $(d0library)/inc/posit.inc\
  $(d0library)/inc/worksp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/clupos.f ;\
	mv clupos.o $(SCRATCH)/d0geant/a4/clupos.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(cluprn.o):\
  $(d0root)/d0geant/trd/cluprn.f\
  $(d0library)/inc/clusm.inc\
  $(d0library)/inc/clustr.inc\
  $(d0library)/inc/gcunit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/cluprn.f ;\
	mv cluprn.o $(SCRATCH)/d0geant/a4/cluprn.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(clurad.o):\
  $(d0root)/d0geant/trd/clurad.f\
  $(d0library)/inc/absor.inc\
  $(d0library)/inc/clugen.inc\
  $(d0library)/inc/clusm.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/enetrd.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/posit.inc\
  $(d0library)/inc/trinfo.inc\
  $(d0library)/inc/xray.inc\
  $(d0library)/inc/xspect.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/clurad.f ;\
	mv clurad.o $(SCRATCH)/d0geant/a4/clurad.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(clusmr.o):\
  $(d0root)/d0geant/trd/clusmr.f\
  $(d0library)/inc/clusm.inc\
  $(d0library)/inc/clustr.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/enetrd.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/info.inc\
  $(d0library)/inc/tecdsc.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/clusmr.f ;\
	mv clusmr.o $(SCRATCH)/d0geant/a4/clusmr.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(clusor.o):\
  $(d0root)/d0geant/trd/clusor.f\
  $(d0library)/inc/clusm.inc\
  $(d0library)/inc/clustr.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/worksp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/clusor.f ;\
	mv clusor.o $(SCRATCH)/d0geant/a4/clusor.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(clustf.o):\
  $(d0root)/d0geant/trd/clustf.f\
  $(d0library)/inc/clurec.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/tgrav.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/clustf.f ;\
	mv clustf.o $(SCRATCH)/d0geant/a4/clustf.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(cpd0rg.o):\
  $(d0root)/d0geant/zebio/cpd0rg.f\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/links/izd0rg.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/cpd0rg.f ;\
	mv cpd0rg.o $(SCRATCH)/d0geant/a4/cpd0rg.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(d0main.o):\
  $(d0root)/d0geant/dzero/d0main.f\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gctime.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/params/nblank.def\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gctime.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/params/nblank.def\
  $(d0library)/inc/gvect.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/params/nblank.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/d0main.f ;\
	mv d0main.o $(SCRATCH)/d0geant/a4/d0main.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(d0xint.o):\
  $(d0root)/d0geant/dzero/d0xint.f\
  $(d0library)/inc/gctime.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gcflag.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/d0xint.f ;\
	mv d0xint.o $(SCRATCH)/d0geant/a4/d0xint.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(datapk.o):\
  $(d0root)/d0geant/cdc/datapk.f\
  $(d0library)/inc/gcunit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/datapk.f ;\
	mv datapk.o $(SCRATCH)/d0geant/a4/datapk.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(dchit.o):\
  $(d0root)/d0geant/caltower/dchit.f\
  $(d0library)/inc/gconst.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/caltower/dchit.f ;\
	mv dchit.o $(SCRATCH)/d0geant/a4/dchit.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(dehit.o):\
  $(d0root)/d0geant/caltower/dehit.f\
  $(d0library)/inc/gconst.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/caltower/dehit.f ;\
	mv dehit.o $(SCRATCH)/d0geant/a4/dehit.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(deldig.o):\
  $(d0root)/d0geant/fdc/deldig.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/deldig.f ;\
	mv deldig.o $(SCRATCH)/d0geant/a4/deldig.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(detcal.o):\
  $(d0root)/d0geant/cal/detcal.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/detcal.f ;\
	mv detcal.o $(SCRATCH)/d0geant/a4/detcal.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(detcdc.o):\
  $(d0root)/d0geant/cdc/detcdc.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/detcdc.f ;\
	mv detcdc.o $(SCRATCH)/d0geant/a4/detcdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(detded.o):\
  $(d0root)/d0geant/cal/detded.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/detded.f ;\
	mv detded.o $(SCRATCH)/d0geant/a4/detded.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(deteca.o):\
  $(d0root)/d0geant/cal/deteca.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/deteca.f ;\
	mv deteca.o $(SCRATCH)/d0geant/a5/deteca.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(detfdc.o):\
  $(d0root)/d0geant/fdc/detfdc.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/detfdc.f ;\
	mv detfdc.o $(SCRATCH)/d0geant/a5/detfdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(detlv0.o):\
  $(d0root)/d0geant/lv0/detlv0.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/lv0/detlv0.f ;\
	mv detlv0.o $(SCRATCH)/d0geant/a5/detlv0.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(detmuo.o):\
  $(d0root)/d0geant/muo/detmuo.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/detmuo.f ;\
	mv detmuo.o $(SCRATCH)/d0geant/a5/detmuo.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(detmuo_v1.o):\
  $(d0root)/d0geant/muo/detmuo_v1.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/detmuo_v1.f ;\
	mv detmuo_v1.o $(SCRATCH)/d0geant/a5/detmuo_v1.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(detsam.o):\
  $(d0root)/d0geant/samus/detsam.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/detsam.f ;\
	mv detsam.o $(SCRATCH)/d0geant/a5/detsam.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(detscn.o):\
  $(d0root)/d0geant/cal/detscn.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/detscn.f ;\
	mv detscn.o $(SCRATCH)/d0geant/a5/detscn.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(dettrd.o):\
  $(d0root)/d0geant/trd/dettrd.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/dettrd.f ;\
	mv dettrd.o $(SCRATCH)/d0geant/a5/dettrd.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(detuca.o):\
  $(d0root)/d0geant/cal/detuca.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/detuca.f ;\
	mv detuca.o $(SCRATCH)/d0geant/a5/detuca.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(detvtx.o):\
  $(d0root)/d0geant/vtx/detvtx.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/detvtx.f ;\
	mv detvtx.o $(SCRATCH)/d0geant/a5/detvtx.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(dffini.o):\
  $(d0root)/d0geant/dzero/dffini.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/zebio.inc\
  $(d0library)/inc/zebiog.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/dffini.f ;\
	mv dffini.o $(SCRATCH)/d0geant/a5/dffini.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(dhstor.o):\
  $(d0root)/d0geant/caltower/dhstor.f\
  $(d0library)/inc/caltrk.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/caltower/dhstor.f ;\
	mv dhstor.o $(SCRATCH)/d0geant/a5/dhstor.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(digcal.o):\
  $(d0root)/d0geant/cal/digcal.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/dcalog.inc\
  $(d0library)/inc/hcal.inc\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/digcal.f ;\
	mv digcal.o $(SCRATCH)/d0geant/a5/digcal.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(digcdc.o):\
  $(d0root)/d0geant/cdc/digcdc.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izdrft.link\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/cdclnk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/digcdc.f ;\
	mv digcdc.o $(SCRATCH)/d0geant/a5/digcdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(digded.o):\
  $(d0root)/d0geant/cal/digded.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/hcal.inc\
  $(d0library)/inc/gcunit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/digded.f ;\
	mv digded.o $(SCRATCH)/d0geant/a5/digded.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(digeca.o):\
  $(d0root)/d0geant/cal/digeca.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/hcal.inc\
  $(d0library)/inc/gcunit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/digeca.f ;\
	mv digeca.o $(SCRATCH)/d0geant/a5/digeca.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(digfdc.o):\
  $(d0root)/d0geant/fdc/digfdc.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdclnk.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/digfdc.f ;\
	mv digfdc.o $(SCRATCH)/d0geant/a5/digfdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(diglv0.o):\
  $(d0root)/d0geant/lv0/diglv0.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/lv0param.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcunit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/lv0/diglv0.f ;\
	mv diglv0.o $(SCRATCH)/d0geant/a5/diglv0.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(digmuo.o):\
  $(d0root)/d0geant/muo/digmuo.f\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/digmuo.f ;\
	mv digmuo.o $(SCRATCH)/d0geant/a5/digmuo.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(digmuo_v1.o):\
  $(d0root)/d0geant/muo/digmuo_v1.f\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/digmuo_v1.f ;\
	mv digmuo_v1.o $(SCRATCH)/d0geant/a5/digmuo_v1.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(digsam.o):\
  $(d0root)/d0geant/samus/digsam.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/digsam.f ;\
	mv digsam.o $(SCRATCH)/d0geant/a6/digsam.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(digscn.o):\
  $(d0root)/d0geant/cal/digscn.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/hcal.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/digscn.f ;\
	mv digscn.o $(SCRATCH)/d0geant/a6/digscn.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(digtrd.o):\
  $(d0root)/d0geant/trd/digtrd.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/nortrd.inc\
  $(d0library)/inc/fadccn.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/geomtc.inc\
  $(d0library)/inc/geomtr.inc\
  $(d0library)/inc/trinfo.inc\
  $(d0library)/inc/tecdsc.inc\
  $(d0library)/inc/gtrhln.inc\
  $(d0library)/inc/worksp.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pedtrd.inc\
  $(d0library)/links/izcdd4.link\
  $(d0library)/links/izgean.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/digtrd.f ;\
	mv digtrd.o $(SCRATCH)/d0geant/a6/digtrd.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(diguca.o):\
  $(d0root)/d0geant/cal/diguca.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/hcal.inc\
  $(d0library)/inc/gcunit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/diguca.f ;\
	mv diguca.o $(SCRATCH)/d0geant/a6/diguca.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(digvtx.o):\
  $(d0root)/d0geant/vtx/digvtx.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/vtxlnk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/digvtx.f ;\
	mv digvtx.o $(SCRATCH)/d0geant/a6/digvtx.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(dodigi.o):\
  $(d0root)/d0geant/utl/dodigi.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcsets.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/utl/dodigi.f ;\
	mv dodigi.o $(SCRATCH)/d0geant/a6/dodigi.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(dschit.o):\
  $(d0root)/d0geant/caltower/dschit.f\
  $(d0library)/inc/gcsets.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/caltower/dschit.f ;\
	mv dschit.o $(SCRATCH)/d0geant/a6/dschit.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(dshit.o):\
  $(d0root)/d0geant/caltower/dshit.f\
  $(d0library)/inc/gcsets.inc\
  $(d0library)/inc/gcunit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/caltower/dshit.f ;\
	mv dshit.o $(SCRATCH)/d0geant/a6/dshit.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(duhit.o):\
  $(d0root)/d0geant/caltower/duhit.f\
  $(d0library)/inc/gconst.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/caltower/duhit.f ;\
	mv duhit.o $(SCRATCH)/d0geant/a6/duhit.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(dvecda.o):\
  $(d0root)/d0geant/cdc/dvecda.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/dvecda.f ;\
	mv dvecda.o $(SCRATCH)/d0geant/a6/dvecda.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(dwdegr.o):\
  $(d0root)/d0geant/trd/dwdegr.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/grutil.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/dwdegr.f ;\
	mv dwdegr.o $(SCRATCH)/d0geant/a6/dwdegr.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(dzemat.o):\
  $(d0root)/d0geant/dzero/dzemat.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/dzemat.f ;\
	mv dzemat.o $(SCRATCH)/d0geant/a6/dzemat.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(dzero.o):\
  $(d0root)/d0geant/dzero/dzero.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/dzero.f ;\
	mv dzero.o $(SCRATCH)/d0geant/a6/dzero.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(ecindx.o):\
  $(d0root)/d0geant/caltower/ecindx.f\
  $(d0library)/inc/gconst.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/caltower/ecindx.f ;\
	mv ecindx.o $(SCRATCH)/d0geant/a6/ecindx.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(edegen.o):\
  $(d0root)/d0geant/trd/edegen.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/disdel.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/edegen.f ;\
	mv edegen.o $(SCRATCH)/d0geant/a6/edegen.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(endzeb.o):\
  $(d0root)/d0geant/zebio/endzeb.f\
  $(d0library)/inc/zebio.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/endzeb.f ;\
	mv endzeb.o $(SCRATCH)/d0geant/a6/endzeb.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(enicdh.o):\
  $(d0root)/d0geant/cal/enicdh.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcunit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/enicdh.f ;\
	mv enicdh.o $(SCRATCH)/d0geant/a6/enicdh.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(enjets.o):\
  $(d0root)/d0geant/cal/enjets.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gconst.inc\
  $(d0library)/inc/gcunit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/enjets.f ;\
	mv enjets.o $(SCRATCH)/d0geant/a6/enjets.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(enjpts.o):\
  $(d0root)/d0geant/cal/enjpts.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izjpts.link\
  $(d0library)/params/byte_order.params\
  $(d0library)/inc/gcunit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/enjpts.f ;\
	mv enjpts.o $(SCRATCH)/d0geant/a6/enjpts.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(entrd.o):\
  $(d0root)/d0geant/trd/entrd.f\
  $(d0library)/inc/clugen.inc\
  $(d0library)/inc/clusm.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/enetrd.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/posit.inc\
  $(d0library)/inc/prnevt.inc\
  $(d0library)/inc/trinfo.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/entrd.f ;\
	mv entrd.o $(SCRATCH)/d0geant/a6/entrd.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(exrgen.o):\
  $(d0root)/d0geant/trd/exrgen.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/tabx.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/exrgen.f ;\
	mv exrgen.o $(SCRATCH)/d0geant/a7/exrgen.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(extrap.o):\
  $(d0root)/d0geant/trd/extrap.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/extrap.f ;\
	mv extrap.o $(SCRATCH)/d0geant/a7/extrap.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(fadcdl.o):\
  $(d0root)/d0geant/cdc/fadcdl.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/fadcdl.f ;\
	mv fadcdl.o $(SCRATCH)/d0geant/a7/fadcdl.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(fadcfr.o):\
  $(d0root)/d0geant/cdc/fadcfr.f\
  $(d0library)/inc/zpulpr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/fadcfr.f ;\
	mv fadcfr.o $(SCRATCH)/d0geant/a7/fadcfr.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(fadcsw.o):\
  $(d0root)/d0geant/cdc/fadcsw.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/fadcsw.f ;\
	mv fadcsw.o $(SCRATCH)/d0geant/a7/fadcsw.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(fadcvs.o):\
  $(d0root)/d0geant/vtx/fadcvs.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/fadcvs.f ;\
	mv fadcvs.o $(SCRATCH)/d0geant/a7/fadcvs.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(fadcvz.o):\
  $(d0root)/d0geant/vtx/fadcvz.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/fadcvz.f ;\
	mv fadcvz.o $(SCRATCH)/d0geant/a7/fadcvz.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(fajxyz.o):\
  $(d0root)/d0geant/dzero/fajxyz.f\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gcnum.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/cenmv.inc\
  $(d0library)/inc/gditrp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/fajxyz.f ;\
	mv fajxyz.o $(SCRATCH)/d0geant/a7/fajxyz.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(fdcwrt.o):\
  $(d0root)/d0geant/fdc/fdcwrt.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/fdclnk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/fdcwrt.f ;\
	mv fdcwrt.o $(SCRATCH)/d0geant/a7/fdcwrt.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(ffical.o):\
  $(d0root)/d0geant/cal/ffical.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/dcalog.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/ffical.f ;\
	mv ffical.o $(SCRATCH)/d0geant/a7/ffical.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(fficdc.o):\
  $(d0root)/d0geant/cdc/fficdc.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/fficdc.f ;\
	mv fficdc.o $(SCRATCH)/d0geant/a7/fficdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(ffid0.o):\
  $(d0root)/d0geant/d0/ffid0.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/d0/ffid0.f ;\
	mv ffid0.o $(SCRATCH)/d0geant/a7/ffid0.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(ffifdc.o):\
  $(d0root)/d0geant/fdc/ffifdc.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/ffifdc.f ;\
	mv ffifdc.o $(SCRATCH)/d0geant/a7/ffifdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(ffilv0.o):\
  $(d0root)/d0geant/lv0/ffilv0.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/lv0/ffilv0.f ;\
	mv ffilv0.o $(SCRATCH)/d0geant/a7/ffilv0.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(ffimuo.o):\
  $(d0root)/d0geant/muo/ffimuo.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/ffimuo.f ;\
	mv ffimuo.o $(SCRATCH)/d0geant/a7/ffimuo.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(ffisam.o):\
  $(d0root)/d0geant/samus/ffisam.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/ffisam.f ;\
	mv ffisam.o $(SCRATCH)/d0geant/a7/ffisam.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(ffitrd.o):\
  $(d0root)/d0geant/trd/ffitrd.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/ffitrd.f ;\
	mv ffitrd.o $(SCRATCH)/d0geant/a7/ffitrd.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(ffivtx.o):\
  $(d0root)/d0geant/vtx/ffivtx.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/ffivtx.f ;\
	mv ffivtx.o $(SCRATCH)/d0geant/a7/ffivtx.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(ficdd1.o):\
  $(d0root)/d0geant/vtx/ficdd1.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/vtxpsh.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/ficdd1.f ;\
	mv ficdd1.o $(SCRATCH)/d0geant/a7/ficdd1.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(ficdd2.o):\
  $(d0root)/d0geant/cdc/ficdd2.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/links/izdtmw.link\
  $(d0library)/links/izdtmd.link\
  $(d0library)/links/izdpdl.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/ficdd2.f ;\
	mv ficdd2.o $(SCRATCH)/d0geant/a7/ficdd2.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(ficdd3.o):\
  $(d0root)/d0geant/fdc/ficdd3.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/fdclnk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/ficdd3.f ;\
	mv ficdd3.o $(SCRATCH)/d0geant/a8/ficdd3.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(fidcda.o):\
  $(d0root)/d0geant/cdc/fidcda.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/cdclnk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/fidcda.f ;\
	mv fidcda.o $(SCRATCH)/d0geant/a8/fidcda.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(fidsec.o):\
  $(d0root)/d0geant/cdc/fidsec.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/cdclnk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/fidsec.f ;\
	mv fidsec.o $(SCRATCH)/d0geant/a8/fidsec.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(fivzda.o):\
  $(d0root)/d0geant/vtx/fivzda.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/vzdata.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/fivzda.f ;\
	mv fivzda.o $(SCRATCH)/d0geant/a8/fivzda.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(fld0rg.o):\
  $(d0root)/d0geant/dzero/fld0rg.f\
  $(d0library)/inc/gcbank.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/fld0rg.f ;\
	mv fld0rg.o $(SCRATCH)/d0geant/a8/fld0rg.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(flgcah.o):\
  $(d0root)/d0geant/caltower/flgcah.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/caltower/flgcah.f ;\
	mv flgcah.o $(SCRATCH)/d0geant/a8/flgcah.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(flgval.o):\
  $(d0root)/d0geant/dzero/flgval.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/flgval.f ;\
	mv flgval.o $(SCRATCH)/d0geant/a8/flgval.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(flicdh.o):\
  $(d0root)/d0geant/cal/flicdh.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gcsets.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcmate.inc\
  $(d0library)/params/byte_order.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/flicdh.f ;\
	mv flicdh.o $(SCRATCH)/d0geant/a8/flicdh.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(fljets.o):\
  $(d0root)/d0geant/cal/fljets.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gcunit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/fljets.f ;\
	mv fljets.o $(SCRATCH)/d0geant/a8/fljets.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(fljpts.o):\
  $(d0root)/d0geant/cal/fljpts.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izjpts.link\
  $(d0library)/params/byte_order.params\
  $(d0library)/inc/gcunit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/fljpts.f ;\
	mv fljpts.o $(SCRATCH)/d0geant/a8/fljpts.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(fpdata.o):\
  $(d0root)/d0geant/fdc/fpdata.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/fdloca.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/fpdata.f ;\
	mv fpdata.o $(SCRATCH)/d0geant/a8/fpdata.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(fphits.o):\
  $(d0root)/d0geant/fdc/fphits.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/fdloca.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/fphits.f ;\
	mv fphits.o $(SCRATCH)/d0geant/a8/fphits.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(ftdata.o):\
  $(d0root)/d0geant/fdc/ftdata.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/fdloca.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/ftdata.f ;\
	mv ftdata.o $(SCRATCH)/d0geant/a8/ftdata.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(ftdlda.o):\
  $(d0root)/d0geant/fdc/ftdlda.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/fdloca.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/ftdlda.f ;\
	mv ftdlda.o $(SCRATCH)/d0geant/a8/ftdlda.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(fthits.o):\
  $(d0root)/d0geant/fdc/fthits.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/fdloca.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/fthits.f ;\
	mv fthits.o $(SCRATCH)/d0geant/a8/fthits.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gdcole.o):\
  $(d0root)/d0geant/dzero/gdcole.f\
  $(d0library)/inc/gcunit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/gdcole.f ;\
	mv gdcole.o $(SCRATCH)/d0geant/a8/gdcole.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(geaisa.o):\
  $(d0root)/d0geant/zebio/geaisa.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/geaisa.f ;\
	mv geaisa.o $(SCRATCH)/d0geant/a8/geaisa.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(geecel.o):\
  $(d0root)/d0geant/cal/geecel.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/geecel.f ;\
	mv geecel.o $(SCRATCH)/d0geant/a8/geecel.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(geecip.o):\
  $(d0root)/d0geant/cal/geecip.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/geecip.f ;\
	mv geecip.o $(SCRATCH)/d0geant/a8/geecip.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(geecmh.o):\
  $(d0root)/d0geant/cal/geecmh.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/geecmh.f ;\
	mv geecmh.o $(SCRATCH)/d0geant/a8/geecmh.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(geecoh.o):\
  $(d0root)/d0geant/cal/geecoh.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/geecoh.f ;\
	mv geecoh.o $(SCRATCH)/d0geant/a9/geecoh.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(geocal.o):\
  $(d0root)/d0geant/cal/geocal.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/geocal.f ;\
	mv geocal.o $(SCRATCH)/d0geant/a9/geocal.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(geocdc.o):\
  $(d0root)/d0geant/cdc/geocdc.f\
  $(d0library)/inc/cenmv.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/links/izdwal.link\
  $(d0library)/links/izdmat.link\
  $(d0library)/links/izdrft.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/geocdc.f ;\
	mv geocdc.o $(SCRATCH)/d0geant/a9/geocdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(geod0.o):\
  $(d0root)/d0geant/d0/geod0.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/d0/geod0.f ;\
	mv geod0.o $(SCRATCH)/d0geant/a9/geod0.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(geoeca.o):\
  $(d0root)/d0geant/cal/geoeca.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/geoeca.f ;\
	mv geoeca.o $(SCRATCH)/d0geant/a9/geoeca.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(geoecr.o):\
  $(d0root)/d0geant/cal/geoecr.f\
  $(d0library)/inc/cryvln.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/srcpr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/geoecr.f ;\
	mv geoecr.o $(SCRATCH)/d0geant/a9/geoecr.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(geofdc.o):\
  $(d0root)/d0geant/fdc/geofdc.f\
  $(d0library)/inc/cenmv.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/links/izfwal.link\
  $(d0library)/links/izfwta.link\
  $(d0library)/links/izfwtb.link\
  $(d0library)/links/izfwph.link\
  $(d0library)/links/izfmat.link\
  $(d0library)/links/izfdrt.link\
  $(d0library)/links/izfdta.link\
  $(d0library)/links/izfdtb.link\
  $(d0library)/links/izfdph.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/geofdc.f ;\
	mv geofdc.o $(SCRATCH)/d0geant/a9/geofdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(geolv0.o):\
  $(d0root)/d0geant/lv0/geolv0.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/lv0/geolv0.f ;\
	mv geolv0.o $(SCRATCH)/d0geant/a9/geolv0.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(geomuo.o):\
  $(d0root)/d0geant/muo/geomuo.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/geomuo.f ;\
	mv geomuo.o $(SCRATCH)/d0geant/a9/geomuo.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(geomvl.o):\
  $(d0root)/d0geant/dzero/geomvl.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/srcpr.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/geomvl.f ;\
	mv geomvl.o $(SCRATCH)/d0geant/a9/geomvl.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(geosam.o):\
  $(d0root)/d0geant/samus/geosam.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/geosam.f ;\
	mv geosam.o $(SCRATCH)/d0geant/a9/geosam.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(geotrd.o):\
  $(d0root)/d0geant/trd/geotrd.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/trdvol.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/links/iztach.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/geotrd.f ;\
	mv geotrd.o $(SCRATCH)/d0geant/a9/geotrd.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(geouca.o):\
  $(d0root)/d0geant/cal/geouca.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/geouca.f ;\
	mv geouca.o $(SCRATCH)/d0geant/a9/geouca.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(geoucr.o):\
  $(d0root)/d0geant/cal/geoucr.f\
  $(d0library)/inc/cryvln.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/srcpr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/geoucr.f ;\
	mv geoucr.o $(SCRATCH)/d0geant/a9/geoucr.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(geovtx.o):\
  $(d0root)/d0geant/vtx/geovtx.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cenmv.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/links/izvwal.link\
  $(d0library)/links/izvmat.link\
  $(d0library)/links/izvrft.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/geovtx.f ;\
	mv geovtx.o $(SCRATCH)/d0geant/a9/geovtx.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gescin.o):\
  $(d0root)/d0geant/cal/gescin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/gescin.f ;\
	mv gescin.o $(SCRATCH)/d0geant/a9/gescin.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(get_pulse.o):\
  $(d0root)/d0geant/vtx/get_pulse.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/get_pulse.f ;\
	mv get_pulse.o $(SCRATCH)/d0geant/a9/get_pulse.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(get_tract828.o):\
  $(d0root)/d0geant/caltower/get_type_interact.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/caltower/get_type_interact.f ;\
	mv get_type_interact.o $(SCRATCH)/d0geant/a9/get_tract828.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(getbin.o):\
  $(d0root)/d0geant/caltower/getbin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/caltower/getbin.f ;\
	mv getbin.o $(SCRATCH)/d0geant/a9/getbin.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(getcel.o):\
  $(d0root)/d0geant/caltower/getcel.f\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/caltower/getcel.f ;\
	mv getcel.o $(SCRATCH)/d0geant/a9/getcel.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(getdet.o):\
  $(d0root)/d0geant/utl/getdet.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/utl/getdet.f ;\
	mv getdet.o $(SCRATCH)/d0geant/a10/getdet.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(geucel.o):\
  $(d0root)/d0geant/cal/geucel.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/geucel.f ;\
	mv geucel.o $(SCRATCH)/d0geant/a10/geucel.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(geucfh.o):\
  $(d0root)/d0geant/cal/geucfh.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/geucfh.f ;\
	mv geucfh.o $(SCRATCH)/d0geant/a10/geucfh.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(geuclk.o):\
  $(d0root)/d0geant/cal/geuclk.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/geuclk.f ;\
	mv geuclk.o $(SCRATCH)/d0geant/a10/geuclk.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(geumsg.o):\
  $(d0root)/d0geant/cal/geumsg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/geumsg.f ;\
	mv geumsg.o $(SCRATCH)/d0geant/a10/geumsg.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gftver.o):\
  $(d0root)/d0geant/dzero/gftver.f\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gcmail.inc\
  $(d0library)/inc/gcnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/gftver.f ;\
	mv gftver.o $(SCRATCH)/d0geant/a10/gftver.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gfver1.o):\
  $(d0root)/d0geant/dzero/gfver1.f\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gcmail.inc\
  $(d0library)/inc/gcnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/gfver1.f ;\
	mv gfver1.o $(SCRATCH)/d0geant/a10/gfver1.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gget.o):\
  $(d0root)/d0geant/vtx/gget.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/gget.f ;\
	mv gget.o $(SCRATCH)/d0geant/a10/gget.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(ginvol.o):\
  $(d0root)/d0geant/dzero/ginvol.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/ginvol.f ;\
	mv ginvol.o $(SCRATCH)/d0geant/a10/ginvol.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gitran.o):\
  $(d0root)/d0geant/dzero/gitran.f\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/gitran.f ;\
	mv gitran.o $(SCRATCH)/d0geant/a10/gitran.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gltrac.o):\
  $(d0root)/d0geant/dzero/gltrac.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/gltrac.f ;\
	mv gltrac.o $(SCRATCH)/d0geant/a10/gltrac.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gnsout.o):\
  $(d0root)/d0geant/dzero/gnsout.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/gnsout.f ;\
	mv gnsout.o $(SCRATCH)/d0geant/a10/gnsout.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(goking.o):\
  $(d0root)/d0geant/dzero/goking.f\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcking.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/dcalog.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/goking.f ;\
	mv goking.o $(SCRATCH)/d0geant/a10/goking.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gsmate.o):\
  $(d0root)/d0geant/dzero/gsmate.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/gsmate.f ;\
	mv gsmate.o $(SCRATCH)/d0geant/a10/gsmate.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gsmixt.o):\
  $(d0root)/d0geant/dzero/gsmixt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/gsmixt.f ;\
	mv gsmixt.o $(SCRATCH)/d0geant/a10/gsmixt.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gspart.o):\
  $(d0root)/d0geant/dzero/gspart.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/gspart.f ;\
	mv gspart.o $(SCRATCH)/d0geant/a10/gspart.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gsrotm.o):\
  $(d0root)/d0geant/dzero/gsrotm.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/gsrotm.f ;\
	mv gsrotm.o $(SCRATCH)/d0geant/a10/gsrotm.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gtelke.o):\
  $(d0root)/d0geant/cdc/gtelke.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/gtelke.f ;\
	mv gtelke.o $(SCRATCH)/d0geant/a10/gtelke.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gtimer.o):\
  $(d0root)/d0geant/utl/gtimer.f\
  $(d0library)/inc/times.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/utl/gtimer.f ;\
	mv gtimer.o $(SCRATCH)/d0geant/a10/gtimer.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gtmedi.o):\
  $(d0root)/d0geant/dzero/gtmedi.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/gtmedi.f ;\
	mv gtmedi.o $(SCRATCH)/d0geant/a10/gtmedi.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gtnino.o):\
  $(d0root)/d0geant/dzero/gtnino.f\
  $(d0library)/inc/gconst.inc\
  $(d0library)/inc/gcsets.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gctmed.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcmate.inc\
  $(d0library)/inc/gccuts.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/dcalog.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/gtnino.f ;\
	mv gtnino.o $(SCRATCH)/d0geant/a11/gtnino.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gtprim.o):\
  $(d0root)/d0geant/cdc/gtprim.f\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gckine.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/gtprim.f ;\
	mv gtprim.o $(SCRATCH)/d0geant/a11/gtprim.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gtrnsf.o):\
  $(d0root)/d0geant/dzero/gtrnsf.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/gtrnsf.f ;\
	mv gtrnsf.o $(SCRATCH)/d0geant/a11/gtrnsf.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gtscnd.o):\
  $(d0root)/d0geant/cdc/gtscnd.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/gtscnd.f ;\
	mv gtscnd.o $(SCRATCH)/d0geant/a11/gtscnd.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gtswch.o):\
  $(d0root)/d0geant/dzero/gtswch.f\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcmail.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/gtswch.f ;\
	mv gtswch.o $(SCRATCH)/d0geant/a11/gtswch.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gudigi.o):\
  $(d0root)/d0geant/dzero/gudigi.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/gudigi.f ;\
	mv gudigi.o $(SCRATCH)/d0geant/a11/gudigi.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gufld.o):\
  $(d0root)/d0geant/muo/gufld.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcvolu.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/gufld.f ;\
	mv gufld.o $(SCRATCH)/d0geant/a11/gufld.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gufld_v1.o):\
  $(d0root)/d0geant/muo/gufld_v1.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/gufld_v1.f ;\
	mv gufld_v1.o $(SCRATCH)/d0geant/a11/gufld_v1.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(guhadr.o):\
  $(d0root)/d0geant/dzero/guhadr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/guhadr.f ;\
	mv guhadr.o $(SCRATCH)/d0geant/a11/guhadr.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(guical.o):\
  $(d0root)/d0geant/cal/guical.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/guical.f ;\
	mv guical.o $(SCRATCH)/d0geant/a11/guical.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(guicdc.o):\
  $(d0root)/d0geant/cdc/guicdc.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/guicdc.f ;\
	mv guicdc.o $(SCRATCH)/d0geant/a11/guicdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(guifdc.o):\
  $(d0root)/d0geant/fdc/guifdc.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/guifdc.f ;\
	mv guifdc.o $(SCRATCH)/d0geant/a11/guifdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(guiget.o):\
  $(d0root)/d0geant/dzero/guiget.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/guiget.f ;\
	mv guiget.o $(SCRATCH)/d0geant/a11/guiget.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(guilv0.o):\
  $(d0root)/d0geant/lv0/guilv0.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/lv0/guilv0.f ;\
	mv guilv0.o $(SCRATCH)/d0geant/a11/guilv0.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(guimuo.o):\
  $(d0root)/d0geant/muo/guimuo.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/guimuo.f ;\
	mv guimuo.o $(SCRATCH)/d0geant/a11/guimuo.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(guisam.o):\
  $(d0root)/d0geant/samus/guisam.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/guisam.f ;\
	mv guisam.o $(SCRATCH)/d0geant/a11/guisam.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(guitrd.o):\
  $(d0root)/d0geant/trd/guitrd.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/guitrd.f ;\
	mv guitrd.o $(SCRATCH)/d0geant/a11/guitrd.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(guivtx.o):\
  $(d0root)/d0geant/vtx/guivtx.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/guivtx.f ;\
	mv guivtx.o $(SCRATCH)/d0geant/a11/guivtx.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gukine.o):\
  $(d0root)/d0geant/dzero/gukine.f\
  $(d0library)/inc/gcflag.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/gukine.f ;\
	mv gukine.o $(SCRATCH)/d0geant/a11/gukine.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(guout.o):\
  $(d0root)/d0geant/dzero/guout.f\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gclist.inc\
  $(d0library)/inc/gcnum.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcunit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/guout.f ;\
	mv guout.o $(SCRATCH)/d0geant/a11/guout.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(guphad.o):\
  $(d0root)/d0geant/dzero/guphad.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/guphad.f ;\
	mv guphad.o $(SCRATCH)/d0geant/a12/guphad.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gupxyz.o):\
  $(d0root)/d0geant/dzero/gupxyz.f\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcvolu.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gcnum.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gcmate.inc\
  $(d0library)/inc/gcmail.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/gupxyz.f ;\
	mv gupxyz.o $(SCRATCH)/d0geant/a12/gupxyz.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gustep.o):\
  $(d0root)/d0geant/dzero/gustep.f\
  $(d0library)/inc/caltrk.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/dcalog.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcking.inc\
  $(d0library)/inc/gctrak.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/gustep.f ;\
	mv gustep.o $(SCRATCH)/d0geant/a12/gustep.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gutrak.o):\
  $(d0root)/d0geant/dzero/gutrak.f\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/dcalog.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/gutrak.f ;\
	mv gutrak.o $(SCRATCH)/d0geant/a12/gutrak.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gutrev.o):\
  $(d0root)/d0geant/dzero/gutrev.f\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gclist.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebio.inc\
  $(d0library)/inc/zebiog.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/links/izisae.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/gutrev.f ;\
	mv gutrev.o $(SCRATCH)/d0geant/a12/gutrev.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gz_clean.o):\
  $(d0root)/d0geant/dzero/gz_clean.f\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gcnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/gz_clean.f ;\
	mv gz_clean.o $(SCRATCH)/d0geant/a12/gz_clean.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gzd0rg.o):\
  $(d0root)/d0geant/utl/gzd0rg.f\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/links/izd0rg.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/utl/gzd0rg.f ;\
	mv gzd0rg.o $(SCRATCH)/d0geant/a12/gzd0rg.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gzd0rgz.o):\
  $(d0root)/d0geant/utl/gzd0rgz.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izd0rgz.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/utl/gzd0rgz.f ;\
	mv gzd0rgz.o $(SCRATCH)/d0geant/a12/gzd0rgz.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gzgswt.o):\
  $(d0root)/d0geant/dzero/gzgswt.f\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/links/izgswt.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/gzgswt.f ;\
	mv gzgswt.o $(SCRATCH)/d0geant/a12/gzgswt.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(gzgswtz.o):\
  $(d0root)/d0geant/dzero/gzgswtz.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izgswt.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/gzgswtz.f ;\
	mv gzgswtz.o $(SCRATCH)/d0geant/a12/gzgswtz.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(hcfill.o):\
  $(d0root)/d0geant/cal/hcfill.f\
  $(d0library)/inc/hcal.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/hcfill.f ;\
	mv hcfill.o $(SCRATCH)/d0geant/a12/hcfill.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(hlpcal.o):\
  $(d0root)/d0geant/cal/hlpcal.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/hlpcal.f ;\
	mv hlpcal.o $(SCRATCH)/d0geant/a12/hlpcal.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(hlpcdc.o):\
  $(d0root)/d0geant/cdc/hlpcdc.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/hlpcdc.f ;\
	mv hlpcdc.o $(SCRATCH)/d0geant/a12/hlpcdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(hlpd0.o):\
  $(d0root)/d0geant/d0/hlpd0.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/d0/hlpd0.f ;\
	mv hlpd0.o $(SCRATCH)/d0geant/a12/hlpd0.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(hlpfdc.o):\
  $(d0root)/d0geant/fdc/hlpfdc.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/hlpfdc.f ;\
	mv hlpfdc.o $(SCRATCH)/d0geant/a12/hlpfdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(hlplv0.o):\
  $(d0root)/d0geant/lv0/hlplv0.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/lv0/hlplv0.f ;\
	mv hlplv0.o $(SCRATCH)/d0geant/a12/hlplv0.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(hlpmuo.o):\
  $(d0root)/d0geant/muo/hlpmuo.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/hlpmuo.f ;\
	mv hlpmuo.o $(SCRATCH)/d0geant/a12/hlpmuo.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(hlpsam.o):\
  $(d0root)/d0geant/samus/hlpsam.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/hlpsam.f ;\
	mv hlpsam.o $(SCRATCH)/d0geant/a12/hlpsam.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(hlptrd.o):\
  $(d0root)/d0geant/trd/hlptrd.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/hlptrd.f ;\
	mv hlptrd.o $(SCRATCH)/d0geant/a12/hlptrd.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(hlpvtx.o):\
  $(d0root)/d0geant/vtx/hlpvtx.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/hlpvtx.f ;\
	mv hlpvtx.o $(SCRATCH)/d0geant/a12/hlpvtx.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(inical.o):\
  $(d0root)/d0geant/cal/inical.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gclist.inc\
  $(d0library)/inc/caltrk.inc\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/inical.f ;\
	mv inical.o $(SCRATCH)/d0geant/a13/inical.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(inicdc.o):\
  $(d0root)/d0geant/cdc/inicdc.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/cdcpar.inc\
  $(d0library)/inc/zpulpr.inc\
  $(d0library)/inc/zzerop.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/inicdc.f ;\
	mv inicdc.o $(SCRATCH)/d0geant/a13/inicdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(inid0.o):\
  $(d0root)/d0geant/d0/inid0.f\
  $(d0library)/inc/gcunit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/d0/inid0.f ;\
	mv inid0.o $(SCRATCH)/d0geant/a13/inid0.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(inifdc.o):\
  $(d0root)/d0geant/fdc/inifdc.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/zpulpr.inc\
  $(d0library)/inc/zzerop.inc\
  $(d0library)/links/izfgeh.link\
  $(d0library)/links/izfalh.link\
  $(d0library)/links/izftmh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/inifdc.f ;\
	mv inifdc.o $(SCRATCH)/d0geant/a13/inifdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(inilv0.o):\
  $(d0root)/d0geant/lv0/inilv0.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/lv0/inilv0.f ;\
	mv inilv0.o $(SCRATCH)/d0geant/a13/inilv0.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(inimuo.o):\
  $(d0root)/d0geant/muo/inimuo.f\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/inimuo.f ;\
	mv inimuo.o $(SCRATCH)/d0geant/a13/inimuo.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(inisam.o):\
  $(d0root)/d0geant/samus/inisam.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/inisam.f ;\
	mv inisam.o $(SCRATCH)/d0geant/a13/inisam.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(initab.o):\
  $(d0root)/d0geant/trd/initab.f\
  $(d0library)/inc/absor.inc\
  $(d0library)/inc/tecdsc.inc\
  $(d0library)/inc/raddcc.inc\
  $(d0library)/inc/tecdcc.inc\
  $(d0library)/inc/windcc.inc\
  $(d0library)/inc/xray.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/initab.f ;\
	mv initab.o $(SCRATCH)/d0geant/a13/initab.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(initrd.o):\
  $(d0root)/d0geant/trd/initrd.f\
  $(d0library)/inc/disdel.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/fadccn.inc\
  $(d0library)/inc/faddig.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/geomtr.inc\
  $(d0library)/inc/nortrd.inc\
  $(d0library)/inc/tabx.inc\
  $(d0library)/inc/trinfo.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/initrd.f ;\
	mv initrd.o $(SCRATCH)/d0geant/a13/initrd.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(inivtx.o):\
  $(d0root)/d0geant/vtx/inivtx.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zpulpr.inc\
  $(d0library)/inc/zzerop.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/inivtx.f ;\
	mv inivtx.o $(SCRATCH)/d0geant/a13/inivtx.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(intcal.o):\
  $(d0root)/d0geant/cal/intcal.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/d0com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/intcal.f ;\
	mv intcal.o $(SCRATCH)/d0geant/a13/intcal.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(intcdc.o):\
  $(d0root)/d0geant/cdc/intcdc.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/d0com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/intcdc.f ;\
	mv intcdc.o $(SCRATCH)/d0geant/a13/intcdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(intd0.o):\
  $(d0root)/d0geant/d0/intd0.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/d0/intd0.f ;\
	mv intd0.o $(SCRATCH)/d0geant/a13/intd0.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(interp.o):\
  $(d0root)/d0geant/trd/interp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/interp.f ;\
	mv interp.o $(SCRATCH)/d0geant/a13/interp.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(intfdc.o):\
  $(d0root)/d0geant/fdc/intfdc.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/d0com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/intfdc.f ;\
	mv intfdc.o $(SCRATCH)/d0geant/a13/intfdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(intlv0.o):\
  $(d0root)/d0geant/lv0/intlv0.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/d0com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/lv0/intlv0.f ;\
	mv intlv0.o $(SCRATCH)/d0geant/a13/intlv0.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(intmuo.o):\
  $(d0root)/d0geant/muo/intmuo.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/d0com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/intmuo.f ;\
	mv intmuo.o $(SCRATCH)/d0geant/a13/intmuo.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(intsam.o):\
  $(d0root)/d0geant/samus/intsam.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/intsam.f ;\
	mv intsam.o $(SCRATCH)/d0geant/a13/intsam.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(inttrd.o):\
  $(d0root)/d0geant/trd/inttrd.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/d0com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/inttrd.f ;\
	mv inttrd.o $(SCRATCH)/d0geant/a13/inttrd.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(intvtx.o):\
  $(d0root)/d0geant/vtx/intvtx.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/d0com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/intvtx.f ;\
	mv intvtx.o $(SCRATCH)/d0geant/a13/intvtx.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(isagea.o):\
  $(d0root)/d0geant/zebio/isagea.f\
  $(d0library)/inc/gcunit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/isagea.f ;\
	mv isagea.o $(SCRATCH)/d0geant/a14/isagea.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(isajerack900.o):\
  $(d0root)/d0geant/zebio/isajet_geant_track.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebio.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/isajet_geant_track.f ;\
	mv isajet_geant_track.o $(SCRATCH)/d0geant/a14/isajerack900.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(isaje_vtx721.o):\
  $(d0root)/d0geant/zebio/isajet_geant_vtx.f\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/isajet_geant_vtx.f ;\
	mv isajet_geant_vtx.o $(SCRATCH)/d0geant/a14/isaje_vtx721.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(isdrop.o):\
  $(d0root)/d0geant/zebio/isdrop.f\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/isdrop.f ;\
	mv isdrop.o $(SCRATCH)/d0geant/a14/isdrop.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(iskine.o):\
  $(d0root)/d0geant/zebio/iskine.f\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcnum.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebio.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/links/izisae.link\
  $(d0library)/links/izisv1.link\
  $(d0library)/links/izisp1.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/iskine.f ;\
	mv iskine.o $(SCRATCH)/d0geant/a14/iskine.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(isout.o):\
  $(d0root)/d0geant/zebio/isout.f\
  $(d0library)/inc/gcnum.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebio.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/links/izisae.link\
  $(d0library)/links/izisv1.link\
  $(d0library)/links/izisp1.link\
  $(d0library)/links/izisv2.link\
  $(d0library)/links/izisp2.link\
  $(d0library)/links/izisp3.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/isout.f ;\
	mv isout.o $(SCRATCH)/d0geant/a14/isout.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(isp1_link.o):\
  $(d0root)/d0geant/cal/isp1_link.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izisae.link\
  $(d0library)/links/izisv1.link\
  $(d0library)/links/izisp1.link\
  $(d0library)/inc/gcnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/isp1_link.f ;\
	mv isp1_link.o $(SCRATCH)/d0geant/a14/isp1_link.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(israw.o):\
  $(d0root)/d0geant/zebio/israw.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/israw.f ;\
	mv israw.o $(SCRATCH)/d0geant/a14/israw.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(isread.o):\
  $(d0root)/d0geant/zebio/isread.f\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gcnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/isread.f ;\
	mv isread.o $(SCRATCH)/d0geant/a14/isread.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(isref.o):\
  $(d0root)/d0geant/zebio/isref.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izisae.link\
  $(d0library)/links/izisv1.link\
  $(d0library)/links/izisv2.link\
  $(d0library)/links/izisp1.link\
  $(d0library)/links/izisp2.link\
  $(d0library)/links/izisp3.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/isref.f ;\
	mv isref.o $(SCRATCH)/d0geant/a14/isref.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(isubfn.o):\
  $(d0root)/d0geant/zebio/isubfn.f\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gcunit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/isubfn.f ;\
	mv isubfn.o $(SCRATCH)/d0geant/a14/isubfn.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(iszeb.o):\
  $(d0root)/d0geant/zebio/iszeb.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebio.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/iszeb.f ;\
	mv iszeb.o $(SCRATCH)/d0geant/a14/iszeb.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(jetend.o):\
  $(d0root)/d0geant/cal/jetend.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/jetend.f ;\
	mv jetend.o $(SCRATCH)/d0geant/a14/jetend.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(jetfil.o):\
  $(d0root)/d0geant/cal/jetfil.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/spevnt.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/jetfil.f ;\
	mv jetfil.o $(SCRATCH)/d0geant/a14/jetfil.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(jetini.o):\
  $(d0root)/d0geant/cal/jetini.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izisae.link\
  $(d0library)/links/izisaq.link\
  $(d0library)/inc/spevnt.inc\
  $(d0library)/inc/gcunit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/jetini.f ;\
	mv jetini.o $(SCRATCH)/d0geant/a14/jetini.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(kind0.o):\
  $(d0root)/d0geant/d0/kind0.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gconst.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gclist.inc\
  $(d0library)/inc/zebio.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/d0/kind0.f ;\
	mv kind0.o $(SCRATCH)/d0geant/a14/kind0.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(l0_adpect301.o):\
  $(d0root)/d0geant/lv0/l0_adc_expect.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/lv0param.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/lv0/l0_adc_expect.f ;\
	mv l0_adc_expect.o $(SCRATCH)/d0geant/a14/l0_adpect301.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(l0_pack_adc.o):\
  $(d0root)/d0geant/lv0/l0_pack_adc.f\
  $(d0library)/inc/lv0param.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/lv0/l0_pack_adc.f ;\
	mv l0_pack_adc.o $(SCRATCH)/d0geant/a14/l0_pack_adc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(l0_paader388.o):\
  $(d0root)/d0geant/lv0/l0_pack_header.f\
  $(d0library)/inc/lv0param.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/lv0/l0_pack_header.f ;\
	mv l0_pack_header.o $(SCRATCH)/d0geant/a14/l0_paader388.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(l0_pailer526.o):\
  $(d0root)/d0geant/lv0/l0_pack_trailer.f\
  $(d0library)/inc/lv0param.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/lv0/l0_pack_trailer.f ;\
	mv l0_pack_trailer.o $(SCRATCH)/d0geant/a14/l0_pailer526.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(l0_partex441.o):\
  $(d0root)/d0geant/lv0/l0_pack_vertex.f\
  $(d0library)/inc/lv0param.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/lv0/l0_pack_vertex.f ;\
	mv l0_pack_vertex.o $(SCRATCH)/d0geant/a15/l0_partex441.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(l0_ruetup404.o):\
  $(d0root)/d0geant/lv0/l0_run1a_setup.f\
  $(d0library)/inc/lv0param.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/lv0/l0_run1a_setup.f ;\
	mv l0_run1a_setup.o $(SCRATCH)/d0geant/a15/l0_ruetup404.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(l0_ruetup405.o):\
  $(d0root)/d0geant/lv0/l0_run1b_setup.f\
  $(d0library)/inc/lv0param.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/lv0/l0_run1b_setup.f ;\
	mv l0_run1b_setup.o $(SCRATCH)/d0geant/a15/l0_ruetup405.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(lastfdcstp.o):\
  $(d0root)/d0geant/fdc/lastfdcstp.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izfgeh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/lastfdcstp.f ;\
	mv lastfdcstp.o $(SCRATCH)/d0geant/a15/lastfdcstp.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(lasttrdstp.o):\
  $(d0root)/d0geant/trd/lasttrdstp.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/iztach.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/lasttrdstp.f ;\
	mv lasttrdstp.o $(SCRATCH)/d0geant/a15/lasttrdstp.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(lddum.o):\
  $(d0root)/d0geant/dzero/lddum.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/lddum.f ;\
	mv lddum.o $(SCRATCH)/d0geant/a15/lddum.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(ldfpda.o):\
  $(d0root)/d0geant/fdc/ldfpda.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/fdloca.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/ldfpda.f ;\
	mv ldfpda.o $(SCRATCH)/d0geant/a15/ldfpda.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(ldfpsc.o):\
  $(d0root)/d0geant/fdc/ldfpsc.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/fdloca.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/ldfpsc.f ;\
	mv ldfpsc.o $(SCRATCH)/d0geant/a15/ldfpsc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(ldftda.o):\
  $(d0root)/d0geant/fdc/ldftda.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/fdloca.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/ldftda.f ;\
	mv ldftda.o $(SCRATCH)/d0geant/a15/ldftda.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(ldftsc.o):\
  $(d0root)/d0geant/fdc/ldftsc.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/fdloca.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/ldftsc.f ;\
	mv ldftsc.o $(SCRATCH)/d0geant/a15/ldftsc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(ldvsec.o):\
  $(d0root)/d0geant/vtx/ldvsec.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/vtxlnk.inc\
  $(d0library)/inc/vtloca.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/ldvsec.f ;\
	mv ldvsec.o $(SCRATCH)/d0geant/a15/ldvsec.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(ldvwda.o):\
  $(d0root)/d0geant/vtx/ldvwda.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/vtxlnk.inc\
  $(d0library)/inc/vtloca.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/ldvwda.f ;\
	mv ldvwda.o $(SCRATCH)/d0geant/a15/ldvwda.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(ldvzda.o):\
  $(d0root)/d0geant/vtx/ldvzda.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/vtxlnk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/ldvzda.f ;\
	mv ldvzda.o $(SCRATCH)/d0geant/a15/ldvzda.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(ldvzla.o):\
  $(d0root)/d0geant/vtx/ldvzla.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/vtxlnk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/ldvzla.f ;\
	mv ldvzla.o $(SCRATCH)/d0geant/a15/ldvzla.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(locgeo.o):\
  $(d0root)/d0geant/utl/locgeo.f\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gcvolu.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/utl/locgeo.f ;\
	mv locgeo.o $(SCRATCH)/d0geant/a15/locgeo.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(lstcal.o):\
  $(d0root)/d0geant/cal/lstcal.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/lstcal.f ;\
	mv lstcal.o $(SCRATCH)/d0geant/a15/lstcal.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(lstcdc.o):\
  $(d0root)/d0geant/cdc/lstcdc.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/lstcdc.f ;\
	mv lstcdc.o $(SCRATCH)/d0geant/a15/lstcdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(lstfdc.o):\
  $(d0root)/d0geant/fdc/lstfdc.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/lstfdc.f ;\
	mv lstfdc.o $(SCRATCH)/d0geant/a15/lstfdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(lstlv0.o):\
  $(d0root)/d0geant/lv0/lstlv0.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/lv0/lstlv0.f ;\
	mv lstlv0.o $(SCRATCH)/d0geant/a15/lstlv0.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(lstmuo.o):\
  $(d0root)/d0geant/muo/lstmuo.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/lstmuo.f ;\
	mv lstmuo.o $(SCRATCH)/d0geant/a15/lstmuo.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(lstsam.o):\
  $(d0root)/d0geant/samus/lstsam.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/lstsam.f ;\
	mv lstsam.o $(SCRATCH)/d0geant/a16/lstsam.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(lsttrd.o):\
  $(d0root)/d0geant/trd/lsttrd.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/lsttrd.f ;\
	mv lsttrd.o $(SCRATCH)/d0geant/a16/lsttrd.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(lstvtx.o):\
  $(d0root)/d0geant/vtx/lstvtx.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/lstvtx.f ;\
	mv lstvtx.o $(SCRATCH)/d0geant/a16/lstvtx.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(lutra1.o):\
  $(d0root)/d0geant/dzero/lutra1.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/lutra1.f ;\
	mv lutra1.o $(SCRATCH)/d0geant/a16/lutra1.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(mcalexit.o):\
  $(d0root)/d0geant/muo/mcalexit.f\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gckine.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/mcalexit.f ;\
	mv mcalexit.o $(SCRATCH)/d0geant/a16/mcalexit.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(mencal.o):\
  $(d0root)/d0geant/cal/mencal.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/dcalog.inc\
  $(d0library)/inc/d0com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/mencal.f ;\
	mv mencal.o $(SCRATCH)/d0geant/a16/mencal.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(mencdc.o):\
  $(d0root)/d0geant/cdc/mencdc.f\
  $(d0library)/inc/d0com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/mencdc.f ;\
	mv mencdc.o $(SCRATCH)/d0geant/a16/mencdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(mend0.o):\
  $(d0root)/d0geant/d0/mend0.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/d0/mend0.f ;\
	mv mend0.o $(SCRATCH)/d0geant/a16/mend0.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(menfdc.o):\
  $(d0root)/d0geant/fdc/menfdc.f\
  $(d0library)/inc/d0com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/menfdc.f ;\
	mv menfdc.o $(SCRATCH)/d0geant/a16/menfdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(menlv0.o):\
  $(d0root)/d0geant/lv0/menlv0.f\
  $(d0library)/inc/d0com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/lv0/menlv0.f ;\
	mv menlv0.o $(SCRATCH)/d0geant/a16/menlv0.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(menmuo.o):\
  $(d0root)/d0geant/muo/menmuo.f\
  $(d0library)/inc/d0com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/menmuo.f ;\
	mv menmuo.o $(SCRATCH)/d0geant/a16/menmuo.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(mensam.o):\
  $(d0root)/d0geant/samus/mensam.f\
  $(d0library)/inc/d0com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/mensam.f ;\
	mv mensam.o $(SCRATCH)/d0geant/a16/mensam.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(mentrd.o):\
  $(d0root)/d0geant/trd/mentrd.f\
  $(d0library)/inc/d0com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/mentrd.f ;\
	mv mentrd.o $(SCRATCH)/d0geant/a16/mentrd.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(menvtx.o):\
  $(d0root)/d0geant/vtx/menvtx.f\
  $(d0library)/inc/d0com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/menvtx.f ;\
	mv menvtx.o $(SCRATCH)/d0geant/a16/menvtx.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(mkdcdc.o):\
  $(d0root)/d0geant/cdc/mkdcdc.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdrft.link\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/cdcpar.inc\
  $(d0library)/inc/cdclnk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/mkdcdc.f ;\
	mv mkdcdc.o $(SCRATCH)/d0geant/a16/mkdcdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(mkfphi.o):\
  $(d0root)/d0geant/fdc/mkfphi.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/fdloca.inc\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/mkfphi.f ;\
	mv mkfphi.o $(SCRATCH)/d0geant/a16/mkfphi.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(mkfthe.o):\
  $(d0root)/d0geant/fdc/mkfthe.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/fdloca.inc\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/mkfthe.f ;\
	mv mkfthe.o $(SCRATCH)/d0geant/a16/mkfthe.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(mkvtxs.o):\
  $(d0root)/d0geant/vtx/mkvtxs.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/vtloca.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/mkvtxs.f ;\
	mv mkvtxs.o $(SCRATCH)/d0geant/a16/mkvtxs.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(mkvtxw.o):\
  $(d0root)/d0geant/vtx/mkvtxw.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/vtxlnk.inc\
  $(d0library)/inc/vtloca.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/mkvtxw.f ;\
	mv mkvtxw.o $(SCRATCH)/d0geant/a16/mkvtxw.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(mscrat.o):\
  $(d0root)/d0geant/muo/mscrat.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/mscrat.f ;\
	mv mscrat.o $(SCRATCH)/d0geant/a16/mscrat.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(msdelt.o):\
  $(d0root)/d0geant/muo/msdelt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/msdelt.f ;\
	mv msdelt.o $(SCRATCH)/d0geant/a17/msdelt.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(msdrft.o):\
  $(d0root)/d0geant/muo/msdrft.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/msdrft.f ;\
	mv msdrft.o $(SCRATCH)/d0geant/a17/msdrft.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(msgnum.o):\
  $(d0root)/d0geant/muo/msgnum.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/msgnum.f ;\
	mv msgnum.o $(SCRATCH)/d0geant/a17/msgnum.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(mshits.o):\
  $(d0root)/d0geant/muo/mshits.f\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gcvolu.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/mshits.f ;\
	mv mshits.o $(SCRATCH)/d0geant/a17/mshits.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(msindx.o):\
  $(d0root)/d0geant/muo/msindx.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/msindx.f ;\
	mv msindx.o $(SCRATCH)/d0geant/a17/msindx.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(msindx_v1.o):\
  $(d0root)/d0geant/muo/msindx_v1.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/msindx_v1.f ;\
	mv msindx_v1.o $(SCRATCH)/d0geant/a17/msindx_v1.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(msmud1.o):\
  $(d0root)/d0geant/muo/msmud1.f\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/mu_zebstp.inc\
  $(d0library)/links/izmud1.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/msmud1.f ;\
	mv msmud1.o $(SCRATCH)/d0geant/a17/msmud1.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(msmud1_v1.o):\
  $(d0root)/d0geant/muo/msmud1_v1.f\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izmud1.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/msmud1_v1.f ;\
	mv msmud1_v1.o $(SCRATCH)/d0geant/a17/msmud1_v1.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(msmudh.o):\
  $(d0root)/d0geant/muo/msmudh.f\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izmud1.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/msmudh.f ;\
	mv msmudh.o $(SCRATCH)/d0geant/a17/msmudh.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(mspad.o):\
  $(d0root)/d0geant/muo/mspad.f\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/links/izstpo.link\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izsmuo.link\
  $(d0library)/links/izmgeh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/mspad.f ;\
	mv mspad.o $(SCRATCH)/d0geant/a17/mspad.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(mspad_v1.o):\
  $(d0root)/d0geant/muo/mspad_v1.f\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/links/izstpo.link\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izsmuo.link\
  $(d0library)/links/izmgeh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/mspad_v1.f ;\
	mv mspad_v1.o $(SCRATCH)/d0geant/a17/mspad_v1.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(mspadr.o):\
  $(d0root)/d0geant/muo/mspadr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/mspadr.f ;\
	mv mspadr.o $(SCRATCH)/d0geant/a17/mspadr.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(mspads.o):\
  $(d0root)/d0geant/muo/mspads.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/mspads.f ;\
	mv mspads.o $(SCRATCH)/d0geant/a17/mspads.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(mstrak.o):\
  $(d0root)/d0geant/muo/mstrak.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcsets.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gcvolu.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/mstrak.f ;\
	mv mstrak.o $(SCRATCH)/d0geant/a17/mstrak.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(mstvc.o):\
  $(d0root)/d0geant/muo/mstvc.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/mstvc.f ;\
	mv mstvc.o $(SCRATCH)/d0geant/a17/mstvc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(mstvc_v1.o):\
  $(d0root)/d0geant/muo/mstvc_v1.f\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/mstvc_v1.f ;\
	mv mstvc_v1.o $(SCRATCH)/d0geant/a17/mstvc_v1.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(msvchk.o):\
  $(d0root)/d0geant/muo/msvchk.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izstpo.link\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izsmuo.link\
  $(d0library)/links/izmgeh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/msvchk.f ;\
	mv msvchk.o $(SCRATCH)/d0geant/a17/msvchk.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(mudrft.o):\
  $(d0root)/d0geant/muo/mudrft.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/mudrft.f ;\
	mv mudrft.o $(SCRATCH)/d0geant/a17/mudrft.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(mxcal.o):\
  $(d0root)/d0geant/cal/mxcal.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/dcalog.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/mxcal.f ;\
	mv mxcal.o $(SCRATCH)/d0geant/a17/mxcal.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(mxlv0.o):\
  $(d0root)/d0geant/lv0/mxlv0.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/lv0/mxlv0.f ;\
	mv mxlv0.o $(SCRATCH)/d0geant/a17/mxlv0.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(norran.o):\
  $(d0root)/d0geant/utl/norran.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/utl/norran.f ;\
	mv norran.o $(SCRATCH)/d0geant/a18/norran.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(nran.o):\
  $(d0root)/d0geant/dzero/nran.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/nran.f ;\
	mv nran.o $(SCRATCH)/d0geant/a18/nran.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(nxtzst.o):\
  $(d0root)/d0geant/vtx/nxtzst.f\
  $(d0library)/inc/vzdata.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/nxtzst.f ;\
	mv nxtzst.o $(SCRATCH)/d0geant/a18/nxtzst.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(pcolor.o):\
  $(d0root)/d0geant/dzero/pcolor.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/pcolor.f ;\
	mv pcolor.o $(SCRATCH)/d0geant/a18/pcolor.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(prgswt.o):\
  $(d0root)/d0geant/dzero/prgswt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/prgswt.f ;\
	mv prgswt.o $(SCRATCH)/d0geant/a18/prgswt.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(print_step.o):\
  $(d0root)/d0geant/dzero/print_step.f\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcking.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gctmed.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gcvolu.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/print_step.f ;\
	mv print_step.o $(SCRATCH)/d0geant/a18/print_step.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(prmmap.o):\
  $(d0root)/d0geant/muo/prmmap.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izmmap.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/prmmap.f ;\
	mv prmmap.o $(SCRATCH)/d0geant/a18/prmmap.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(prune_gskine.o):\
  $(d0root)/d0geant/dzero/prune_gskine.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/prune_gskine.f ;\
	mv prune_gskine.o $(SCRATCH)/d0geant/a18/prune_gskine.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(rdzeb.o):\
  $(d0root)/d0geant/zebio/rdzeb.f\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebio.inc\
  $(d0library)/inc/zebiog.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gclist.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/links/izgean.link\
  $(d0library)/inc/mioflg.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/rdzeb.f ;\
	mv rdzeb.o $(SCRATCH)/d0geant/a18/rdzeb.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(rotino.o):\
  $(d0root)/d0geant/zebio/rotino.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/rotino.f ;\
	mv rotino.o $(SCRATCH)/d0geant/a18/rotino.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(rrlam.o):\
  $(d0root)/d0geant/cal/rrlam.f\
  $(d0library)/inc/gconst.inc\
  $(d0library)/inc/absln.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/rrlam.f ;\
	mv rrlam.o $(SCRATCH)/d0geant/a18/rrlam.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(rwxspc.o):\
  $(d0root)/d0geant/trd/rwxspc.f\
  $(d0library)/inc/gconst.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/raddsc.inc\
  $(d0library)/inc/tabx.inc\
  $(d0library)/inc/xray.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/rwxspc.f ;\
	mv rwxspc.o $(SCRATCH)/d0geant/a18/rwxspc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(safill.o):\
  $(d0root)/d0geant/samus/safill.f\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/mu_zebstp.inc\
  $(d0library)/links/izmud1.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/safill.f ;\
	mv safill.o $(SCRATCH)/d0geant/a18/safill.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(saicmp.o):\
  $(d0root)/d0geant/samus/saicmp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/saicmp.f ;\
	mv saicmp.o $(SCRATCH)/d0geant/a18/saicmp.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(sam_bp.o):\
  $(d0root)/d0geant/samus/sam_bp.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/srcpr.inc\
  $(d0library)/links/izsbpg.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/sam_bp.f ;\
	mv sam_bp.o $(SCRATCH)/d0geant/a18/sam_bp.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(sam_ma.o):\
  $(d0root)/d0geant/samus/sam_ma.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/srcpr.inc\
  $(d0library)/links/izsmag.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/sam_ma.f ;\
	mv sam_ma.o $(SCRATCH)/d0geant/a18/sam_ma.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(samadc.o):\
  $(d0root)/d0geant/samus/samadc.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/samadc.f ;\
	mv samadc.o $(SCRATCH)/d0geant/a18/samadc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(samgt1.o):\
  $(d0root)/d0geant/samus/samgt1.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/samgt1.f ;\
	mv samgt1.o $(SCRATCH)/d0geant/a18/samgt1.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(samgt2.o):\
  $(d0root)/d0geant/samus/samgt2.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izgsat.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/samgt2.f ;\
	mv samgt2.o $(SCRATCH)/d0geant/a18/samgt2.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(saname.o):\
  $(d0root)/d0geant/samus/saname.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/saname.f ;\
	mv saname.o $(SCRATCH)/d0geant/a18/saname.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(saslb1.o):\
  $(d0root)/d0geant/samus/saslb1.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/saslb1.f ;\
	mv saslb1.o $(SCRATCH)/d0geant/a19/saslb1.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(saslb2.o):\
  $(d0root)/d0geant/samus/saslb2.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/saslb2.f ;\
	mv saslb2.o $(SCRATCH)/d0geant/a19/saslb2.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(sastat.o):\
  $(d0root)/d0geant/samus/sastat.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/srcpr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/sastat.f ;\
	mv sastat.o $(SCRATCH)/d0geant/a19/sastat.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(satube.o):\
  $(d0root)/d0geant/samus/satube.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/satube.f ;\
	mv satube.o $(SCRATCH)/d0geant/a19/satube.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(set_epars830.o):\
  $(d0root)/d0geant/cal/set_electron_pars.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/dcalog.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/set_electron_pars.f ;\
	mv set_electron_pars.o $(SCRATCH)/d0geant/a19/set_epars830.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(set_saram725.o):\
  $(d0root)/d0geant/cal/set_shower_param.f\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/dcalog.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/set_shower_param.f ;\
	mv set_shower_param.o $(SCRATCH)/d0geant/a19/set_saram725.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(setdet.o):\
  $(d0root)/d0geant/utl/setdet.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/utl/setdet.f ;\
	mv setdet.o $(SCRATCH)/d0geant/a19/setdet.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(setrot.o):\
  $(d0root)/d0geant/utl/setrot.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/utl/setrot.f ;\
	mv setrot.o $(SCRATCH)/d0geant/a19/setrot.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(setvew.o):\
  $(d0root)/d0geant/utl/setvew.f\
  $(d0library)/inc/srcpr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/utl/setvew.f ;\
	mv setvew.o $(SCRATCH)/d0geant/a19/setvew.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(shcopy.o):\
  $(d0root)/d0geant/caltower/shcopy.f\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/gcsets.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcking.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebshl.inc\
  $(d0library)/links/izshhd.link\
  $(d0library)/links/izshlb.link\
  $(d0library)/inc/shlcon.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/caltower/shcopy.f ;\
	mv shcopy.o $(SCRATCH)/d0geant/a19/shcopy.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(shinit.o):\
  $(d0root)/d0geant/caltower/shinit.f\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebshl.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/shlcon.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/caltower/shinit.f ;\
	mv shinit.o $(SCRATCH)/d0geant/a19/shinit.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(shlast.o):\
  $(d0root)/d0geant/caltower/shlast.f\
  $(d0library)/inc/shlcon.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/caltower/shlast.f ;\
	mv shlast.o $(SCRATCH)/d0geant/a19/shlast.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(shlb_cal_xyz.o):\
  $(d0root)/d0geant/zebio/shlb_cal_xyz.f\
  $(d0library)/inc/shldat.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/shlb_cal_xyz.f ;\
	mv shlb_cal_xyz.o $(SCRATCH)/d0geant/a19/shlb_cal_xyz.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(shlb__key983.o):\
  $(d0root)/d0geant/cal/shlb_elec_match_key.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/shlcon.inc\
  $(d0library)/inc/shldat.inc\
  $(d0library)/inc/shsort.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zlinkc.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/shlitr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/shlb_elec_match_key.f ;\
	mv shlb_elec_match_key.o $(SCRATCH)/d0geant/a19/shlb__key983.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(shlb_rack597.o):\
  $(d0root)/d0geant/zebio/shlb_move_track.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/shldat.inc\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/shlb_move_track.f ;\
	mv shlb_move_track.o $(SCRATCH)/d0geant/a19/shlb_rack597.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(simpul.o):\
  $(d0root)/d0geant/cdc/simpul.f\
  $(d0library)/inc/zpulpr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/simpul.f ;\
	mv simpul.o $(SCRATCH)/d0geant/a19/simpul.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(slopan.o):\
  $(d0root)/d0geant/utl/slopan.f\
  $(d0library)/inc/gconst.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/utl/slopan.f ;\
	mv slopan.o $(SCRATCH)/d0geant/a19/slopan.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(smrfac.o):\
  $(d0root)/d0geant/caltower/smrfac.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/caltower/smrfac.f ;\
	mv smrfac.o $(SCRATCH)/d0geant/a19/smrfac.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(snapshot.o):\
  $(d0root)/d0geant/dzero/snapshot.f\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcking.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gcnum.inc\
  $(d0library)/inc/gcstak.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gcunit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/snapshot.f ;\
	mv snapshot.o $(SCRATCH)/d0geant/a19/snapshot.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(splgar.o):\
  $(d0root)/d0geant/trd/splgar.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/xray.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/splgar.f ;\
	mv splgar.o $(SCRATCH)/d0geant/a19/splgar.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(stolv0.o):\
  $(d0root)/d0geant/lv0/stolv0.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/lv0/stolv0.f ;\
	mv stolv0.o $(SCRATCH)/d0geant/a20/stolv0.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(store_mcen.o):\
  $(d0root)/d0geant/dzero/store_mcen.f\
  $(d0library)/inc/cenmv.inc\
  $(d0library)/inc/dcalog.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcking.inc\
  $(d0library)/inc/gctrak.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/store_mcen.f ;\
	mv store_mcen.o $(SCRATCH)/d0geant/a20/store_mcen.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(stpcal.o):\
  $(d0root)/d0geant/cal/stpcal.f\
  $(d0library)/inc/gcsets.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcking.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/dcalog.inc\
  $(d0library)/inc/gcvolu.inc\
  $(d0library)/inc/caltrk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/stpcal.f ;\
	mv stpcal.o $(SCRATCH)/d0geant/a20/stpcal.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(stpcdc.o):\
  $(d0root)/d0geant/cdc/stpcdc.f\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gcsets.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/stpcdc.f ;\
	mv stpcdc.o $(SCRATCH)/d0geant/a20/stpcdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(stpd0.o):\
  $(d0root)/d0geant/d0/stpd0.f\
  $(d0library)/inc/cenmv.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcking.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gcvolu.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/d0/stpd0.f ;\
	mv stpd0.o $(SCRATCH)/d0geant/a20/stpd0.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(stpfdc.o):\
  $(d0root)/d0geant/fdc/stpfdc.f\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gcsets.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/stpfdc.f ;\
	mv stpfdc.o $(SCRATCH)/d0geant/a20/stpfdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(stplv0.o):\
  $(d0root)/d0geant/lv0/stplv0.f\
  $(d0library)/inc/lv0param.inc\
  $(d0library)/inc/gcsets.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gconst.inc\
  $(d0library)/inc/gcking.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcvolu.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/lv0/stplv0.f ;\
	mv stplv0.o $(SCRATCH)/d0geant/a20/stplv0.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(stpmuo.o):\
  $(d0root)/d0geant/muo/stpmuo.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcsets.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/stpmuo.f ;\
	mv stpmuo.o $(SCRATCH)/d0geant/a20/stpmuo.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(stpmuo_v1.o):\
  $(d0root)/d0geant/muo/stpmuo_v1.f\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcsets.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/stpmuo_v1.f ;\
	mv stpmuo_v1.o $(SCRATCH)/d0geant/a20/stpmuo_v1.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(stpsam.o):\
  $(d0root)/d0geant/samus/stpsam.f\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcsets.inc\
  $(d0library)/inc/gcvolu.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/stpsam.f ;\
	mv stpsam.o $(SCRATCH)/d0geant/a20/stpsam.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(stptrd.o):\
  $(d0root)/d0geant/trd/stptrd.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/enetrd.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/posit.inc\
  $(d0library)/inc/prnevt.inc\
  $(d0library)/inc/trinfo.inc\
  $(d0library)/inc/trdvol.inc\
  $(d0library)/inc/geomtr.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izisae.link\
  $(d0library)/links/izisv1.link\
  $(d0library)/links/izisp1.link\
  $(d0library)/inc/gcsets.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/stptrd.f ;\
	mv stptrd.o $(SCRATCH)/d0geant/a20/stptrd.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(stpvtx.o):\
  $(d0root)/d0geant/vtx/stpvtx.f\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gcsets.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/stpvtx.f ;\
	mv stpvtx.o $(SCRATCH)/d0geant/a20/stpvtx.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(stswch.o):\
  $(d0root)/d0geant/dzero/stswch.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/stswch.f ;\
	mv stswch.o $(SCRATCH)/d0geant/a20/stswch.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(stzdib.o):\
  $(d0root)/d0geant/utl/stzdib.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/utl/stzdib.f ;\
	mv stzdib.o $(SCRATCH)/d0geant/a20/stzdib.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(stzdiv.o):\
  $(d0root)/d0geant/utl/stzdiv.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/utl/stzdiv.f ;\
	mv stzdiv.o $(SCRATCH)/d0geant/a20/stzdiv.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(stzdv1.o):\
  $(d0root)/d0geant/utl/stzdv1.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/utl/stzdv1.f ;\
	mv stzdv1.o $(SCRATCH)/d0geant/a20/stzdv1.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(super_gswt.o):\
  $(d0root)/d0geant/dzero/super_gswt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/super_gswt.f ;\
	mv super_gswt.o $(SCRATCH)/d0geant/a20/super_gswt.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(svdlay.o):\
  $(d0root)/d0geant/fdc/svdlay.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/svdlay.f ;\
	mv svdlay.o $(SCRATCH)/d0geant/a20/svdlay.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(tevcal.o):\
  $(d0root)/d0geant/cal/tevcal.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/dcalog.inc\
  $(d0library)/inc/hcal.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/tevcal.f ;\
	mv tevcal.o $(SCRATCH)/d0geant/a20/tevcal.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(tevcdc.o):\
  $(d0root)/d0geant/cdc/tevcdc.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/tevcdc.f ;\
	mv tevcdc.o $(SCRATCH)/d0geant/a20/tevcdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(tevfdc.o):\
  $(d0root)/d0geant/fdc/tevfdc.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/tevfdc.f ;\
	mv tevfdc.o $(SCRATCH)/d0geant/a21/tevfdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(tevlv0.o):\
  $(d0root)/d0geant/lv0/tevlv0.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/lv0/tevlv0.f ;\
	mv tevlv0.o $(SCRATCH)/d0geant/a21/tevlv0.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(tevmuo.o):\
  $(d0root)/d0geant/muo/tevmuo.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/tevmuo.f ;\
	mv tevmuo.o $(SCRATCH)/d0geant/a21/tevmuo.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(tevsam.o):\
  $(d0root)/d0geant/samus/tevsam.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/tevsam.f ;\
	mv tevsam.o $(SCRATCH)/d0geant/a21/tevsam.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(tevtrd.o):\
  $(d0root)/d0geant/trd/tevtrd.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/tevtrd.f ;\
	mv tevtrd.o $(SCRATCH)/d0geant/a21/tevtrd.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(tevvtx.o):\
  $(d0root)/d0geant/vtx/tevvtx.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/tevvtx.f ;\
	mv tevvtx.o $(SCRATCH)/d0geant/a21/tevvtx.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(tlayer.o):\
  $(d0root)/d0geant/trd/tlayer.f\
  $(d0library)/inc/posit.inc\
  $(d0library)/inc/gcsets.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/tlayer.f ;\
	mv tlayer.o $(SCRATCH)/d0geant/a21/tlayer.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(trabsl.o):\
  $(d0root)/d0geant/trd/trabsl.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/trabsl.f ;\
	mv trabsl.o $(SCRATCH)/d0geant/a21/trabsl.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(trafra.o):\
  $(d0root)/d0geant/zebio/trafra.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/trafra.f ;\
	mv trafra.o $(SCRATCH)/d0geant/a21/trafra.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(trchan.o):\
  $(d0root)/d0geant/trd/trchan.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcunit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/trchan.f ;\
	mv trchan.o $(SCRATCH)/d0geant/a21/trchan.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(trdblk.o):\
  $(d0root)/d0geant/trd/trdblk.f\
  $(d0library)/inc/fadccn.inc\
  $(d0library)/inc/faddig.inc\
  $(d0library)/inc/info.inc\
  $(d0library)/inc/nortrd.inc\
  $(d0library)/inc/probde.inc\
  $(d0library)/inc/raddsc.inc\
  $(d0library)/inc/tecdsc.inc\
  $(d0library)/inc/windsc.inc\
  $(d0library)/inc/raddcc.inc\
  $(d0library)/inc/tecdcc.inc\
  $(d0library)/inc/windcc.inc\
  $(d0library)/inc/xray.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/trdblk.f ;\
	mv trdblk.o $(SCRATCH)/d0geant/a21/trdblk.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(trdfmy.o):\
  $(d0root)/d0geant/trd/trdfmy.f\
  $(d0library)/inc/gcunit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/trdfmy.f ;\
	mv trdfmy.o $(SCRATCH)/d0geant/a21/trdfmy.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(trdshp.o):\
  $(d0root)/d0geant/trd/trdshp.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/fadccn.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/nortrd.inc\
  $(d0library)/inc/worksp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/trdshp.f ;\
	mv trdshp.o $(SCRATCH)/d0geant/a21/trdshp.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(trdtim.o):\
  $(d0root)/d0geant/trd/trdtim.f\
  $(d0library)/inc/gcunit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/trdtim.f ;\
	mv trdtim.o $(SCRATCH)/d0geant/a21/trdtim.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(trdunp.o):\
  $(d0root)/d0geant/trd/trdunp.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/trdunp.f ;\
	mv trdunp.o $(SCRATCH)/d0geant/a21/trdunp.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(trdwde.o):\
  $(d0root)/d0geant/trd/trdwde.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/trdwde.f ;\
	mv trdwde.o $(SCRATCH)/d0geant/a21/trdwde.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(trdyld.o):\
  $(d0root)/d0geant/trd/trdyld.f\
  $(d0library)/inc/absor.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/raddsc.inc\
  $(d0library)/inc/raddcc.inc\
  $(d0library)/inc/tecdsc.inc\
  $(d0library)/inc/windsc.inc\
  $(d0library)/inc/xspect.inc\
  $(d0library)/inc/xray.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/trdyld.f ;\
	mv trdyld.o $(SCRATCH)/d0geant/a21/trdyld.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(trgpmc.o):\
  $(d0root)/d0geant/zebio/trgpmc.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/fadccn.inc\
  $(d0library)/inc/geomtc.inc\
  $(d0library)/inc/geomtr.inc\
  $(d0library)/inc/pedtrd.inc\
  $(d0library)/inc/nortrd.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/trgpmc.f ;\
	mv trgpmc.o $(SCRATCH)/d0geant/a21/trgpmc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(trhigh.o):\
  $(d0root)/d0geant/trd/trhigh.f\
  $(d0library)/inc/gconst.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcunit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/trhigh.f ;\
	mv trhigh.o $(SCRATCH)/d0geant/a21/trhigh.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(trkcal.o):\
  $(d0root)/d0geant/cal/trkcal.f\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/dcalog.inc\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gcstak.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/trkcal.f ;\
	mv trkcal.o $(SCRATCH)/d0geant/a21/trkcal.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(trkcdc.o):\
  $(d0root)/d0geant/cdc/trkcdc.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/trkcdc.f ;\
	mv trkcdc.o $(SCRATCH)/d0geant/a22/trkcdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(trkfdc.o):\
  $(d0root)/d0geant/fdc/trkfdc.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/fdc/trkfdc.f ;\
	mv trkfdc.o $(SCRATCH)/d0geant/a22/trkfdc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(trklv0.o):\
  $(d0root)/d0geant/lv0/trklv0.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/lv0/trklv0.f ;\
	mv trklv0.o $(SCRATCH)/d0geant/a22/trklv0.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(trkmuo.o):\
  $(d0root)/d0geant/muo/trkmuo.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/trkmuo.f ;\
	mv trkmuo.o $(SCRATCH)/d0geant/a22/trkmuo.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(trksam.o):\
  $(d0root)/d0geant/samus/trksam.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/samus/trksam.f ;\
	mv trksam.o $(SCRATCH)/d0geant/a22/trksam.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(trktrd.o):\
  $(d0root)/d0geant/trd/trktrd.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/trktrd.f ;\
	mv trktrd.o $(SCRATCH)/d0geant/a22/trktrd.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(trkvtx.o):\
  $(d0root)/d0geant/vtx/trkvtx.f\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/trkvtx.f ;\
	mv trkvtx.o $(SCRATCH)/d0geant/a22/trkvtx.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(trnumb.o):\
  $(d0root)/d0geant/trd/trnumb.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/trnumb.f ;\
	mv trnumb.o $(SCRATCH)/d0geant/a22/trnumb.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(trtabl.o):\
  $(d0root)/d0geant/trd/trtabl.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/tabx.inc\
  $(d0library)/inc/gconst.inc\
  $(d0library)/inc/worksp.inc\
  $(d0library)/inc/xray.inc\
  $(d0library)/inc/xspect.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/raddsc.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/trtabl.f ;\
	mv trtabl.o $(SCRATCH)/d0geant/a22/trtabl.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(ucindx.o):\
  $(d0root)/d0geant/caltower/ucindx.f\
  $(d0library)/inc/gconst.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/caltower/ucindx.f ;\
	mv ucindx.o $(SCRATCH)/d0geant/a22/ucindx.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(uginit.o):\
  $(d0root)/d0geant/dzero/uginit.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gclist.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gcphys.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/uginit.f ;\
	mv uginit.o $(SCRATCH)/d0geant/a22/uginit.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(uglast.o):\
  $(d0root)/d0geant/dzero/uglast.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gclist.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/uglast.f ;\
	mv uglast.o $(SCRATCH)/d0geant/a22/uglast.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(use_mulib.o):\
  $(d0root)/d0geant/muo/use_mulib.f\
  $(d0library)/inc/mulcon.inc\
  $(d0library)/inc/muldat.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/use_mulib.f ;\
	mv use_mulib.o $(SCRATCH)/d0geant/a22/use_mulib.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(use_mrary642.o):\
  $(d0root)/d0geant/muo/use_muonlibrary.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/mulcon.inc\
  $(d0library)/inc/muldat.inc\
  $(d0library)/inc/shsort.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zlinkc.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/links/izisp1.link\
  $(d0library)/inc/shlitr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/muo/use_muonlibrary.f ;\
	mv use_muonlibrary.o $(SCRATCH)/d0geant/a22/use_mrary642.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(use_shlb.o):\
  $(d0root)/d0geant/zebio/use_shlb.f\
  $(d0library)/params/cal_offline.params\
  $(d0library)/inc/shlcon.inc\
  $(d0library)/inc/shldat.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/use_shlb.f ;\
	mv use_shlb.o $(SCRATCH)/d0geant/a22/use_shlb.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(use_srary859.o):\
  $(d0root)/d0geant/zebio/use_showerlibrary.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/shlcon.inc\
  $(d0library)/inc/shldat.inc\
  $(d0library)/inc/shsort.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zlinkc.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/dcalog.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gcking.inc\
  $(d0library)/params/isajet_codes.def\
  $(d0library)/links/izisp1.link\
  $(d0library)/links/izisv2.link\
  $(d0library)/links/izisp2.link\
  $(d0library)/inc/shlitr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/use_showerlibrary.f ;\
	mv use_showerlibrary.o $(SCRATCH)/d0geant/a22/use_srary859.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(vdzero.o):\
  $(d0root)/d0geant/dzero/vdzero.f\
  $(d0library)/inc/gcflag.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/dzero/vdzero.f ;\
	mv vdzero.o $(SCRATCH)/d0geant/a22/vdzero.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(vlanda.o):\
  $(d0root)/d0geant/vtx/vlanda.f\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gckine.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/vlanda.f ;\
	mv vlanda.o $(SCRATCH)/d0geant/a22/vlanda.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(volord.o):\
  $(d0root)/d0geant/cal/volord.f\
  $(d0library)/inc/srcpr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cal/volord.f ;\
	mv volord.o $(SCRATCH)/d0geant/a22/volord.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(volpos.o):\
  $(d0root)/d0geant/utl/volpos.f\
  $(d0library)/inc/srcpr.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/utl/volpos.f ;\
	mv volpos.o $(SCRATCH)/d0geant/a22/volpos.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(volpoz.o):\
  $(d0root)/d0geant/utl/volpoz.f\
  $(d0library)/inc/srcpr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/utl/volpoz.f ;\
	mv volpoz.o $(SCRATCH)/d0geant/a23/volpoz.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(vtxpul.o):\
  $(d0root)/d0geant/vtx/vtxpul.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/vtxpsh.inc\
  $(d0library)/inc/vtloca.inc\
  $(d0library)/inc/vtxlnk.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/vtxpul.f ;\
	mv vtxpul.o $(SCRATCH)/d0geant/a23/vtxpul.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(vtxwrt.o):\
  $(d0root)/d0geant/vtx/vtxwrt.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/vtxlnk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/vtxwrt.f ;\
	mv vtxwrt.o $(SCRATCH)/d0geant/a23/vtxwrt.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(wrzeb.o):\
  $(d0root)/d0geant/zebio/wrzeb.f\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebio.inc\
  $(d0library)/inc/zebiog.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/mioflg.inc\
  $(d0library)/inc/getcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/wrzeb.f ;\
	mv wrzeb.o $(SCRATCH)/d0geant/a23/wrzeb.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(xinter.o):\
  $(d0root)/d0geant/trd/xinter.f\
  $(d0library)/inc/grutil.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/xinter.f ;\
	mv xinter.o $(SCRATCH)/d0geant/a23/xinter.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(xlum.o):\
  $(d0root)/d0geant/trd/xlum.f\
  $(d0library)/inc/absor.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gconst.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/xlum.f ;\
	mv xlum.o $(SCRATCH)/d0geant/a23/xlum.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(xrad.o):\
  $(d0root)/d0geant/trd/xrad.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/xspect.inc\
  $(d0library)/inc/tabx.inc\
  $(d0library)/inc/xray.inc\
  $(d0library)/inc/d0log.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/trd/xrad.f ;\
	mv xrad.o $(SCRATCH)/d0geant/a23/xrad.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(zadped.o):\
  $(d0root)/d0geant/vtx/zadped.f\
  $(d0library)/inc/gcflag.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/zadped.f ;\
	mv zadped.o $(SCRATCH)/d0geant/a23/zadped.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(zcnvrt.o):\
  $(d0root)/d0geant/cdc/zcnvrt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/zcnvrt.f ;\
	mv zcnvrt.o $(SCRATCH)/d0geant/a23/zcnvrt.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(zebfin.o):\
  $(d0root)/d0geant/zebio/zebfin.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebio.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/zebfin.f ;\
	mv zebfin.o $(SCRATCH)/d0geant/a23/zebfin.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(zebini.o):\
  $(d0root)/d0geant/zebio/zebini.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/zebini.f ;\
	mv zebini.o $(SCRATCH)/d0geant/a23/zebini.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(zebout.o):\
  $(d0root)/d0geant/zebio/zebout.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/zebio/zebout.f ;\
	mv zebout.o $(SCRATCH)/d0geant/a23/zebout.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(zersup.o):\
  $(d0root)/d0geant/cdc/zersup.f\
  $(d0library)/inc/zzerop.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/cdc/zersup.f ;\
	mv zersup.o $(SCRATCH)/d0geant/a23/zersup.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(zinduc.o):\
  $(d0root)/d0geant/vtx/zinduc.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/zinduc.f ;\
	mv zinduc.o $(SCRATCH)/d0geant/a23/zinduc.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(zsinit.o):\
  $(d0root)/d0geant/vtx/zsinit.f\
  $(d0library)/inc/vzhits.inc\
  $(d0library)/inc/vzdata.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/zsinit.f ;\
	mv zsinit.o $(SCRATCH)/d0geant/a23/zsinit.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(zstrht.o):\
  $(d0root)/d0geant/vtx/zstrht.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/vzhits.inc\
  $(d0library)/inc/vtloca.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/zstrht.f ;\
	mv zstrht.o $(SCRATCH)/d0geant/a23/zstrht.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(zstrip.o):\
  $(d0root)/d0geant/vtx/zstrip.f\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/vzdata.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/vtloca.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/zstrip.f ;\
	mv zstrip.o $(SCRATCH)/d0geant/a23/zstrip.o ;\
	)
$(d0root)/d0geant/deb_d0geant.a(zstrno.o):\
  $(d0root)/d0geant/vtx/zstrno.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0geant/vtx/zstrno.f ;\
	mv zstrno.o $(SCRATCH)/d0geant/a23/zstrno.o ;\
	)
deb_d0geant_farm.a : $(d0root)/d0geant/deb_d0geant_farm.a
	@ echo deb_d0geant_farm.a is up to date
$(d0root)/d0geant/deb_d0geant_farm.a:: $(SCRATCH)/d0geant_farm/nothing.nl
$(d0root)/d0geant/deb_d0geant_farm.a::
	rmdirp $(SCRATCH)/d0geant_farm
$(SCRATCH)/d0geant_farm/nothing.nl:
	rmdirp $(SCRATCH)/d0geant_farm
deb_d0geant_uffarm.a : $(d0root)/d0geant/deb_d0geant_uffarm.a
	@ echo deb_d0geant_uffarm.a is up to date
$(d0root)/d0geant/deb_d0geant_uffarm.a:: $(SCRATCH)/d0geant_uffarm/nothing.nl
$(d0root)/d0geant/deb_d0geant_uffarm.a::
	rmdirp $(SCRATCH)/d0geant_uffarm
$(SCRATCH)/d0geant_uffarm/nothing.nl:
	rmdirp $(SCRATCH)/d0geant_uffarm
deb_d0geantdi.a : $(d0root)/d0geant/deb_d0geantdi.a
	@ echo deb_d0geantdi.a is up to date
$(d0root)/d0geant/deb_d0geantdi.a:: $(SCRATCH)/d0geantdi/nothing.nl
$(d0root)/d0geant/deb_d0geantdi.a::
	rmdirp $(SCRATCH)/d0geantdi
$(SCRATCH)/d0geantdi/nothing.nl:
	rmdirp $(SCRATCH)/d0geantdi
pre:\
  $(d0root)/d0geant/cal/absorb.f\
  $(d0root)/d0geant/cal/anlcal.f\
  $(d0root)/d0geant/cdc/anlcdc.f\
  $(d0root)/d0geant/fdc/anlfdc.f\
  $(d0root)/d0geant/lv0/anllv0.f\
  $(d0root)/d0geant/muo/anlmuo.f\
  $(d0root)/d0geant/samus/anlsam.f\
  $(d0root)/d0geant/trd/anltrd.f\
  $(d0root)/d0geant/vtx/anlvtx.f\
  $(d0root)/d0geant/trd/aphasa.f\
  $(d0root)/d0geant/utl/bkd0rg.f\
  $(d0root)/d0geant/cal/bkgcah.f\
  $(d0root)/d0geant/dzero/bkgswt.f\
  $(d0root)/d0geant/zebio/bkisp2.f\
  $(d0root)/d0geant/zebio/bkisv2.f\
  $(d0root)/d0geant/vtx/blcdd1.f\
  $(d0root)/d0geant/cdc/blcdd2.f\
  $(d0root)/d0geant/fdc/blcdd3.f\
  $(d0root)/d0geant/cdc/bldcda.f\
  $(d0root)/d0geant/cdc/bldsec.f\
  $(d0root)/d0geant/vtx/blvsec.f\
  $(d0root)/d0geant/vtx/blvwda.f\
  $(d0root)/d0geant/vtx/blvzda.f\
  $(d0root)/d0geant/vtx/blvzla.f\
  $(d0root)/d0geant/cal/bokcal.f\
  $(d0root)/d0geant/cdc/bokcdc.f\
  $(d0root)/d0geant/fdc/bokfdc.f\
  $(d0root)/d0geant/lv0/boklv0.f\
  $(d0root)/d0geant/muo/bokmuo.f\
  $(d0root)/d0geant/samus/boksam.f\
  $(d0root)/d0geant/trd/boktrd.f\
  $(d0root)/d0geant/vtx/bokvtx.f\
  $(d0root)/d0geant/caltower/cacmpr.f\
  $(d0root)/d0geant/caltower/calbld.f\
  $(d0root)/d0geant/utl/cd_head_flg.f\
  $(d0root)/d0geant/zebio/cd_hstr_flg.f\
  $(d0root)/d0geant/cdc/cdatdl.f\
  $(d0root)/d0geant/cdc/cdatsw.f\
  $(d0root)/d0geant/cdc/cdcwrt.f\
  $(d0root)/d0geant/vtx/cdd1fl.f\
  $(d0root)/d0geant/cdc/cdd2fl.f\
  $(d0root)/d0geant/fdc/cdd3fl.f\
  $(d0root)/d0geant/trd/cdd4fl.f\
  $(d0root)/d0geant/cdc/cdelay.f\
  $(d0root)/d0geant/cdc/cdhits.f\
  $(d0root)/d0geant/cdc/cdresp.f\
  $(d0root)/d0geant/cdc/cdrift.f\
  $(d0root)/d0geant/vtx/chgdiv.f\
  $(d0root)/d0geant/zebio/chkbrr.f\
  $(d0root)/d0geant/cdc/clanda.f\
  $(d0root)/d0geant/cal/clncal.f\
  $(d0root)/d0geant/cdc/clncdc.f\
  $(d0root)/d0geant/fdc/clnfdc.f\
  $(d0root)/d0geant/lv0/clnlv0.f\
  $(d0root)/d0geant/muo/clnmuo.f\
  $(d0root)/d0geant/samus/clnsam.f\
  $(d0root)/d0geant/trd/clntrd.f\
  $(d0root)/d0geant/vtx/clnvtx.f\
  $(d0root)/d0geant/trd/clsmen.f\
  $(d0root)/d0geant/trd/clucle.f\
  $(d0root)/d0geant/trd/cludel.f\
  $(d0root)/d0geant/trd/cludig.f\
  $(d0root)/d0geant/trd/clumix.f\
  $(d0root)/d0geant/trd/clumrg.f\
  $(d0root)/d0geant/trd/clupos.f\
  $(d0root)/d0geant/trd/cluprn.f\
  $(d0root)/d0geant/trd/clurad.f\
  $(d0root)/d0geant/trd/clusmr.f\
  $(d0root)/d0geant/trd/clusor.f\
  $(d0root)/d0geant/trd/clustf.f\
  $(d0root)/d0geant/zebio/cpd0rg.f\
  $(d0root)/d0geant/dzero/d0main.f\
  $(d0root)/d0geant/dzero/d0xint.f\
  $(d0root)/d0geant/cdc/datapk.f\
  $(d0root)/d0geant/caltower/dchit.f\
  $(d0root)/d0geant/caltower/dehit.f\
  $(d0root)/d0geant/fdc/deldig.f\
  $(d0root)/d0geant/cal/detcal.f\
  $(d0root)/d0geant/cdc/detcdc.f\
  $(d0root)/d0geant/cal/detded.f\
  $(d0root)/d0geant/cal/deteca.f\
  $(d0root)/d0geant/fdc/detfdc.f\
  $(d0root)/d0geant/lv0/detlv0.f\
  $(d0root)/d0geant/muo/detmuo.f\
  $(d0root)/d0geant/muo/detmuo_v1.f\
  $(d0root)/d0geant/samus/detsam.f\
  $(d0root)/d0geant/cal/detscn.f\
  $(d0root)/d0geant/trd/dettrd.f\
  $(d0root)/d0geant/cal/detuca.f\
  $(d0root)/d0geant/vtx/detvtx.f\
  $(d0root)/d0geant/dzero/dffini.f\
  $(d0root)/d0geant/caltower/dhstor.f\
  $(d0root)/d0geant/cal/digcal.f\
  $(d0root)/d0geant/cdc/digcdc.f\
  $(d0root)/d0geant/cal/digded.f\
  $(d0root)/d0geant/cal/digeca.f\
  $(d0root)/d0geant/fdc/digfdc.f\
  $(d0root)/d0geant/lv0/diglv0.f\
  $(d0root)/d0geant/muo/digmuo.f\
  $(d0root)/d0geant/muo/digmuo_v1.f\
  $(d0root)/d0geant/samus/digsam.f\
  $(d0root)/d0geant/cal/digscn.f\
  $(d0root)/d0geant/trd/digtrd.f\
  $(d0root)/d0geant/cal/diguca.f\
  $(d0root)/d0geant/vtx/digvtx.f\
  $(d0root)/d0geant/utl/dodigi.f\
  $(d0root)/d0geant/caltower/dschit.f\
  $(d0root)/d0geant/caltower/dshit.f\
  $(d0root)/d0geant/caltower/duhit.f\
  $(d0root)/d0geant/cdc/dvecda.f\
  $(d0root)/d0geant/trd/dwdegr.f\
  $(d0root)/d0geant/dzero/dzemat.f\
  $(d0root)/d0geant/dzero/dzero.f\
  $(d0root)/d0geant/caltower/ecindx.f\
  $(d0root)/d0geant/trd/edegen.f\
  $(d0root)/d0geant/zebio/endzeb.f\
  $(d0root)/d0geant/cal/enicdh.f\
  $(d0root)/d0geant/cal/enjets.f\
  $(d0root)/d0geant/cal/enjpts.f\
  $(d0root)/d0geant/trd/entrd.f\
  $(d0root)/d0geant/trd/exrgen.f\
  $(d0root)/d0geant/trd/extrap.f\
  $(d0root)/d0geant/cdc/fadcdl.f\
  $(d0root)/d0geant/cdc/fadcfr.f\
  $(d0root)/d0geant/cdc/fadcsw.f\
  $(d0root)/d0geant/vtx/fadcvs.f\
  $(d0root)/d0geant/vtx/fadcvz.f\
  $(d0root)/d0geant/dzero/fajxyz.f\
  $(d0root)/d0geant/fdc/fdcwrt.f\
  $(d0root)/d0geant/cal/ffical.f\
  $(d0root)/d0geant/cdc/fficdc.f\
  $(d0root)/d0geant/d0/ffid0.f\
  $(d0root)/d0geant/fdc/ffifdc.f\
  $(d0root)/d0geant/lv0/ffilv0.f\
  $(d0root)/d0geant/muo/ffimuo.f\
  $(d0root)/d0geant/samus/ffisam.f\
  $(d0root)/d0geant/trd/ffitrd.f\
  $(d0root)/d0geant/vtx/ffivtx.f\
  $(d0root)/d0geant/vtx/ficdd1.f\
  $(d0root)/d0geant/cdc/ficdd2.f\
  $(d0root)/d0geant/fdc/ficdd3.f\
  $(d0root)/d0geant/cdc/fidcda.f\
  $(d0root)/d0geant/cdc/fidsec.f\
  $(d0root)/d0geant/vtx/fivzda.f\
  $(d0root)/d0geant/dzero/fld0rg.f\
  $(d0root)/d0geant/caltower/flgcah.f\
  $(d0root)/d0geant/dzero/flgval.f\
  $(d0root)/d0geant/cal/flicdh.f\
  $(d0root)/d0geant/cal/fljets.f\
  $(d0root)/d0geant/cal/fljpts.f\
  $(d0root)/d0geant/fdc/fpdata.f\
  $(d0root)/d0geant/fdc/fphits.f\
  $(d0root)/d0geant/fdc/ftdata.f\
  $(d0root)/d0geant/fdc/ftdlda.f\
  $(d0root)/d0geant/fdc/fthits.f\
  $(d0root)/d0geant/dzero/gdcole.f\
  $(d0root)/d0geant/zebio/geaisa.f\
  $(d0root)/d0geant/cal/geecel.f\
  $(d0root)/d0geant/cal/geecip.f\
  $(d0root)/d0geant/cal/geecmh.f\
  $(d0root)/d0geant/cal/geecoh.f\
  $(d0root)/d0geant/cal/geocal.f\
  $(d0root)/d0geant/cdc/geocdc.f\
  $(d0root)/d0geant/d0/geod0.f\
  $(d0root)/d0geant/cal/geoeca.f\
  $(d0root)/d0geant/cal/geoecr.f\
  $(d0root)/d0geant/fdc/geofdc.f\
  $(d0root)/d0geant/lv0/geolv0.f\
  $(d0root)/d0geant/muo/geomuo.f\
  $(d0root)/d0geant/dzero/geomvl.f\
  $(d0root)/d0geant/samus/geosam.f\
  $(d0root)/d0geant/trd/geotrd.f\
  $(d0root)/d0geant/cal/geouca.f\
  $(d0root)/d0geant/cal/geoucr.f\
  $(d0root)/d0geant/vtx/geovtx.f\
  $(d0root)/d0geant/cal/gescin.f\
  $(d0root)/d0geant/vtx/get_pulse.f\
  $(d0root)/d0geant/caltower/get_type_interact.f\
  $(d0root)/d0geant/caltower/getbin.f\
  $(d0root)/d0geant/caltower/getcel.f\
  $(d0root)/d0geant/utl/getdet.f\
  $(d0root)/d0geant/cal/geucel.f\
  $(d0root)/d0geant/cal/geucfh.f\
  $(d0root)/d0geant/cal/geuclk.f\
  $(d0root)/d0geant/cal/geumsg.f\
  $(d0root)/d0geant/dzero/gftver.f\
  $(d0root)/d0geant/dzero/gfver1.f\
  $(d0root)/d0geant/vtx/gget.f\
  $(d0root)/d0geant/dzero/ginvol.f\
  $(d0root)/d0geant/dzero/gitran.f\
  $(d0root)/d0geant/dzero/gltrac.f\
  $(d0root)/d0geant/dzero/gnsout.f\
  $(d0root)/d0geant/dzero/goking.f\
  $(d0root)/d0geant/dzero/gsmate.f\
  $(d0root)/d0geant/dzero/gsmixt.f\
  $(d0root)/d0geant/dzero/gspart.f\
  $(d0root)/d0geant/dzero/gsrotm.f\
  $(d0root)/d0geant/cdc/gtelke.f\
  $(d0root)/d0geant/utl/gtimer.f\
  $(d0root)/d0geant/dzero/gtmedi.f\
  $(d0root)/d0geant/dzero/gtnino.f\
  $(d0root)/d0geant/cdc/gtprim.f\
  $(d0root)/d0geant/dzero/gtrnsf.f\
  $(d0root)/d0geant/cdc/gtscnd.f\
  $(d0root)/d0geant/dzero/gtswch.f\
  $(d0root)/d0geant/dzero/gudigi.f\
  $(d0root)/d0geant/muo/gufld.f\
  $(d0root)/d0geant/muo/gufld_v1.f\
  $(d0root)/d0geant/dzero/guhadr.f\
  $(d0root)/d0geant/cal/guical.f\
  $(d0root)/d0geant/cdc/guicdc.f\
  $(d0root)/d0geant/fdc/guifdc.f\
  $(d0root)/d0geant/dzero/guiget.f\
  $(d0root)/d0geant/lv0/guilv0.f\
  $(d0root)/d0geant/muo/guimuo.f\
  $(d0root)/d0geant/samus/guisam.f\
  $(d0root)/d0geant/trd/guitrd.f\
  $(d0root)/d0geant/vtx/guivtx.f\
  $(d0root)/d0geant/dzero/gukine.f\
  $(d0root)/d0geant/dzero/guout.f\
  $(d0root)/d0geant/dzero/guphad.f\
  $(d0root)/d0geant/dzero/gupxyz.f\
  $(d0root)/d0geant/dzero/gustep.f\
  $(d0root)/d0geant/dzero/gutrak.f\
  $(d0root)/d0geant/dzero/gutrev.f\
  $(d0root)/d0geant/dzero/gz_clean.f\
  $(d0root)/d0geant/utl/gzd0rg.f\
  $(d0root)/d0geant/utl/gzd0rgz.f\
  $(d0root)/d0geant/dzero/gzgswt.f\
  $(d0root)/d0geant/dzero/gzgswtz.f\
  $(d0root)/d0geant/cal/hcfill.f\
  $(d0root)/d0geant/cal/hlpcal.f\
  $(d0root)/d0geant/cdc/hlpcdc.f\
  $(d0root)/d0geant/d0/hlpd0.f\
  $(d0root)/d0geant/fdc/hlpfdc.f\
  $(d0root)/d0geant/lv0/hlplv0.f\
  $(d0root)/d0geant/muo/hlpmuo.f\
  $(d0root)/d0geant/samus/hlpsam.f\
  $(d0root)/d0geant/trd/hlptrd.f\
  $(d0root)/d0geant/vtx/hlpvtx.f\
  $(d0root)/d0geant/cal/inical.f\
  $(d0root)/d0geant/cdc/inicdc.f\
  $(d0root)/d0geant/d0/inid0.f\
  $(d0root)/d0geant/fdc/inifdc.f\
  $(d0root)/d0geant/lv0/inilv0.f\
  $(d0root)/d0geant/muo/inimuo.f\
  $(d0root)/d0geant/samus/inisam.f\
  $(d0root)/d0geant/trd/initab.f\
  $(d0root)/d0geant/trd/initrd.f\
  $(d0root)/d0geant/vtx/inivtx.f\
  $(d0root)/d0geant/cal/intcal.f\
  $(d0root)/d0geant/cdc/intcdc.f\
  $(d0root)/d0geant/d0/intd0.f\
  $(d0root)/d0geant/trd/interp.f\
  $(d0root)/d0geant/fdc/intfdc.f\
  $(d0root)/d0geant/lv0/intlv0.f\
  $(d0root)/d0geant/muo/intmuo.f\
  $(d0root)/d0geant/samus/intsam.f\
  $(d0root)/d0geant/trd/inttrd.f\
  $(d0root)/d0geant/vtx/intvtx.f\
  $(d0root)/d0geant/zebio/isagea.f\
  $(d0root)/d0geant/zebio/isajet_geant_track.f\
  $(d0root)/d0geant/zebio/isajet_geant_vtx.f\
  $(d0root)/d0geant/zebio/isdrop.f\
  $(d0root)/d0geant/zebio/iskine.f\
  $(d0root)/d0geant/zebio/isout.f\
  $(d0root)/d0geant/cal/isp1_link.f\
  $(d0root)/d0geant/zebio/israw.f\
  $(d0root)/d0geant/zebio/isread.f\
  $(d0root)/d0geant/zebio/isref.f\
  $(d0root)/d0geant/zebio/isubfn.f\
  $(d0root)/d0geant/zebio/iszeb.f\
  $(d0root)/d0geant/cal/jetend.f\
  $(d0root)/d0geant/cal/jetfil.f\
  $(d0root)/d0geant/cal/jetini.f\
  $(d0root)/d0geant/d0/kind0.f\
  $(d0root)/d0geant/lv0/l0_adc_expect.f\
  $(d0root)/d0geant/lv0/l0_pack_adc.f\
  $(d0root)/d0geant/lv0/l0_pack_header.f\
  $(d0root)/d0geant/lv0/l0_pack_trailer.f\
  $(d0root)/d0geant/lv0/l0_pack_vertex.f\
  $(d0root)/d0geant/lv0/l0_run1a_setup.f\
  $(d0root)/d0geant/lv0/l0_run1b_setup.f\
  $(d0root)/d0geant/fdc/lastfdcstp.f\
  $(d0root)/d0geant/trd/lasttrdstp.f\
  $(d0root)/d0geant/dzero/lddum.f\
  $(d0root)/d0geant/fdc/ldfpda.f\
  $(d0root)/d0geant/fdc/ldfpsc.f\
  $(d0root)/d0geant/fdc/ldftda.f\
  $(d0root)/d0geant/fdc/ldftsc.f\
  $(d0root)/d0geant/vtx/ldvsec.f\
  $(d0root)/d0geant/vtx/ldvwda.f\
  $(d0root)/d0geant/vtx/ldvzda.f\
  $(d0root)/d0geant/vtx/ldvzla.f\
  $(d0root)/d0geant/utl/locgeo.f\
  $(d0root)/d0geant/cal/lstcal.f\
  $(d0root)/d0geant/cdc/lstcdc.f\
  $(d0root)/d0geant/fdc/lstfdc.f\
  $(d0root)/d0geant/lv0/lstlv0.f\
  $(d0root)/d0geant/muo/lstmuo.f\
  $(d0root)/d0geant/samus/lstsam.f\
  $(d0root)/d0geant/trd/lsttrd.f\
  $(d0root)/d0geant/vtx/lstvtx.f\
  $(d0root)/d0geant/dzero/lutra1.f\
  $(d0root)/d0geant/muo/mcalexit.f\
  $(d0root)/d0geant/cal/mencal.f\
  $(d0root)/d0geant/cdc/mencdc.f\
  $(d0root)/d0geant/d0/mend0.f\
  $(d0root)/d0geant/fdc/menfdc.f\
  $(d0root)/d0geant/lv0/menlv0.f\
  $(d0root)/d0geant/muo/menmuo.f\
  $(d0root)/d0geant/samus/mensam.f\
  $(d0root)/d0geant/trd/mentrd.f\
  $(d0root)/d0geant/vtx/menvtx.f\
  $(d0root)/d0geant/cdc/mkdcdc.f\
  $(d0root)/d0geant/fdc/mkfphi.f\
  $(d0root)/d0geant/fdc/mkfthe.f\
  $(d0root)/d0geant/vtx/mkvtxs.f\
  $(d0root)/d0geant/vtx/mkvtxw.f\
  $(d0root)/d0geant/muo/mscrat.f\
  $(d0root)/d0geant/muo/msdelt.f\
  $(d0root)/d0geant/muo/msdrft.f\
  $(d0root)/d0geant/muo/msgnum.f\
  $(d0root)/d0geant/muo/mshits.f\
  $(d0root)/d0geant/muo/msindx.f\
  $(d0root)/d0geant/muo/msindx_v1.f\
  $(d0root)/d0geant/muo/msmud1.f\
  $(d0root)/d0geant/muo/msmud1_v1.f\
  $(d0root)/d0geant/muo/msmudh.f\
  $(d0root)/d0geant/muo/mspad.f\
  $(d0root)/d0geant/muo/mspad_v1.f\
  $(d0root)/d0geant/muo/mspadr.f\
  $(d0root)/d0geant/muo/mspads.f\
  $(d0root)/d0geant/muo/mstrak.f\
  $(d0root)/d0geant/muo/mstvc.f\
  $(d0root)/d0geant/muo/mstvc_v1.f\
  $(d0root)/d0geant/muo/msvchk.f\
  $(d0root)/d0geant/muo/mudrft.f\
  $(d0root)/d0geant/cal/mxcal.f\
  $(d0root)/d0geant/lv0/mxlv0.f\
  $(d0root)/d0geant/utl/norran.f\
  $(d0root)/d0geant/dzero/nran.f\
  $(d0root)/d0geant/vtx/nxtzst.f\
  $(d0root)/d0geant/dzero/pcolor.f\
  $(d0root)/d0geant/dzero/prgswt.f\
  $(d0root)/d0geant/dzero/print_step.f\
  $(d0root)/d0geant/muo/prmmap.f\
  $(d0root)/d0geant/dzero/prune_gskine.f\
  $(d0root)/d0geant/zebio/rdzeb.f\
  $(d0root)/d0geant/zebio/rotino.f\
  $(d0root)/d0geant/cal/rrlam.f\
  $(d0root)/d0geant/trd/rwxspc.f\
  $(d0root)/d0geant/samus/safill.f\
  $(d0root)/d0geant/samus/saicmp.f\
  $(d0root)/d0geant/samus/sam_bp.f\
  $(d0root)/d0geant/samus/sam_ma.f\
  $(d0root)/d0geant/samus/samadc.f\
  $(d0root)/d0geant/samus/samgt1.f\
  $(d0root)/d0geant/samus/samgt2.f\
  $(d0root)/d0geant/samus/saname.f\
  $(d0root)/d0geant/samus/saslb1.f\
  $(d0root)/d0geant/samus/saslb2.f\
  $(d0root)/d0geant/samus/sastat.f\
  $(d0root)/d0geant/samus/satube.f\
  $(d0root)/d0geant/cal/set_electron_pars.f\
  $(d0root)/d0geant/cal/set_shower_param.f\
  $(d0root)/d0geant/utl/setdet.f\
  $(d0root)/d0geant/utl/setrot.f\
  $(d0root)/d0geant/utl/setvew.f\
  $(d0root)/d0geant/caltower/shcopy.f\
  $(d0root)/d0geant/caltower/shinit.f\
  $(d0root)/d0geant/caltower/shlast.f\
  $(d0root)/d0geant/zebio/shlb_cal_xyz.f\
  $(d0root)/d0geant/cal/shlb_elec_match_key.f\
  $(d0root)/d0geant/zebio/shlb_move_track.f\
  $(d0root)/d0geant/cdc/simpul.f\
  $(d0root)/d0geant/utl/slopan.f\
  $(d0root)/d0geant/caltower/smrfac.f\
  $(d0root)/d0geant/dzero/snapshot.f\
  $(d0root)/d0geant/trd/splgar.f\
  $(d0root)/d0geant/lv0/stolv0.f\
  $(d0root)/d0geant/dzero/store_mcen.f\
  $(d0root)/d0geant/cal/stpcal.f\
  $(d0root)/d0geant/cdc/stpcdc.f\
  $(d0root)/d0geant/d0/stpd0.f\
  $(d0root)/d0geant/fdc/stpfdc.f\
  $(d0root)/d0geant/lv0/stplv0.f\
  $(d0root)/d0geant/muo/stpmuo.f\
  $(d0root)/d0geant/muo/stpmuo_v1.f\
  $(d0root)/d0geant/samus/stpsam.f\
  $(d0root)/d0geant/trd/stptrd.f\
  $(d0root)/d0geant/vtx/stpvtx.f\
  $(d0root)/d0geant/dzero/stswch.f\
  $(d0root)/d0geant/utl/stzdib.f\
  $(d0root)/d0geant/utl/stzdiv.f\
  $(d0root)/d0geant/utl/stzdv1.f\
  $(d0root)/d0geant/dzero/super_gswt.f\
  $(d0root)/d0geant/fdc/svdlay.f\
  $(d0root)/d0geant/cal/tevcal.f\
  $(d0root)/d0geant/cdc/tevcdc.f\
  $(d0root)/d0geant/fdc/tevfdc.f\
  $(d0root)/d0geant/lv0/tevlv0.f\
  $(d0root)/d0geant/muo/tevmuo.f\
  $(d0root)/d0geant/samus/tevsam.f\
  $(d0root)/d0geant/trd/tevtrd.f\
  $(d0root)/d0geant/vtx/tevvtx.f\
  $(d0root)/d0geant/trd/tlayer.f\
  $(d0root)/d0geant/trd/trabsl.f\
  $(d0root)/d0geant/zebio/trafra.f\
  $(d0root)/d0geant/trd/trchan.f\
  $(d0root)/d0geant/trd/trdblk.f\
  $(d0root)/d0geant/trd/trdfmy.f\
  $(d0root)/d0geant/trd/trdshp.f\
  $(d0root)/d0geant/trd/trdtim.f\
  $(d0root)/d0geant/trd/trdunp.f\
  $(d0root)/d0geant/trd/trdwde.f\
  $(d0root)/d0geant/trd/trdyld.f\
  $(d0root)/d0geant/zebio/trgpmc.f\
  $(d0root)/d0geant/trd/trhigh.f\
  $(d0root)/d0geant/cal/trkcal.f\
  $(d0root)/d0geant/cdc/trkcdc.f\
  $(d0root)/d0geant/fdc/trkfdc.f\
  $(d0root)/d0geant/lv0/trklv0.f\
  $(d0root)/d0geant/muo/trkmuo.f\
  $(d0root)/d0geant/samus/trksam.f\
  $(d0root)/d0geant/trd/trktrd.f\
  $(d0root)/d0geant/vtx/trkvtx.f\
  $(d0root)/d0geant/trd/trnumb.f\
  $(d0root)/d0geant/trd/trtabl.f\
  $(d0root)/d0geant/caltower/ucindx.f\
  $(d0root)/d0geant/dzero/uginit.f\
  $(d0root)/d0geant/dzero/uglast.f\
  $(d0root)/d0geant/muo/use_mulib.f\
  $(d0root)/d0geant/muo/use_muonlibrary.f\
  $(d0root)/d0geant/zebio/use_shlb.f\
  $(d0root)/d0geant/zebio/use_showerlibrary.f\
  $(d0root)/d0geant/dzero/vdzero.f\
  $(d0root)/d0geant/vtx/vlanda.f\
  $(d0root)/d0geant/cal/volord.f\
  $(d0root)/d0geant/utl/volpos.f\
  $(d0root)/d0geant/utl/volpoz.f\
  $(d0root)/d0geant/vtx/vtxpul.f\
  $(d0root)/d0geant/vtx/vtxwrt.f\
  $(d0root)/d0geant/zebio/wrzeb.f\
  $(d0root)/d0geant/trd/xinter.f\
  $(d0root)/d0geant/trd/xlum.f\
  $(d0root)/d0geant/trd/xrad.f\
  $(d0root)/d0geant/vtx/zadped.f\
  $(d0root)/d0geant/cdc/zcnvrt.f\
  $(d0root)/d0geant/zebio/zebfin.f\
  $(d0root)/d0geant/zebio/zebini.f\
  $(d0root)/d0geant/zebio/zebout.f\
  $(d0root)/d0geant/cdc/zersup.f\
  $(d0root)/d0geant/vtx/zinduc.f\
  $(d0root)/d0geant/vtx/zsinit.f\
  $(d0root)/d0geant/vtx/zstrht.f\
  $(d0root)/d0geant/vtx/zstrip.f\
  $(d0root)/d0geant/vtx/zstrno.f
$(d0root)/d0geant/cal/absorb.f:\
  $(d0root)/d0geant/cal/absorb.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gconst.inc\
  $(d0library)/inc/gctmed.inc\
  $(d0library)/inc/absln.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/absorb.for | vmstounix > $(d0root)/d0geant/cal/absorb.f
$(d0root)/d0geant/cal/anlcal.f:\
  $(d0root)/d0geant/cal/anlcal.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/anlcal.for | vmstounix > $(d0root)/d0geant/cal/anlcal.f
$(d0root)/d0geant/cdc/anlcdc.f:\
  $(d0root)/d0geant/cdc/anlcdc.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/anlcdc.for | vmstounix > $(d0root)/d0geant/cdc/anlcdc.f
$(d0root)/d0geant/fdc/anlfdc.f:\
  $(d0root)/d0geant/fdc/anlfdc.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/anlfdc.for | vmstounix > $(d0root)/d0geant/fdc/anlfdc.f
$(d0root)/d0geant/lv0/anllv0.f:\
  $(d0root)/d0geant/lv0/anllv0.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/lv0/anllv0.for | vmstounix > $(d0root)/d0geant/lv0/anllv0.f
$(d0root)/d0geant/muo/anlmuo.f:\
  $(d0root)/d0geant/muo/anlmuo.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/anlmuo.for | vmstounix > $(d0root)/d0geant/muo/anlmuo.f
$(d0root)/d0geant/samus/anlsam.f:\
  $(d0root)/d0geant/samus/anlsam.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/anlsam.for | vmstounix > $(d0root)/d0geant/samus/anlsam.f
$(d0root)/d0geant/trd/anltrd.f:\
  $(d0root)/d0geant/trd/anltrd.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/trhitw.inc\
  $(d0library)/inc/trinfo.inc\
  $(d0library)/inc/worksp.inc\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/anltrd.for | vmstounix > $(d0root)/d0geant/trd/anltrd.f
$(d0root)/d0geant/vtx/anlvtx.f:\
  $(d0root)/d0geant/vtx/anlvtx.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/anlvtx.for | vmstounix > $(d0root)/d0geant/vtx/anlvtx.f
$(d0root)/d0geant/trd/aphasa.f:\
  $(d0root)/d0geant/trd/aphasa.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/aphasa.for | vmstounix > $(d0root)/d0geant/trd/aphasa.f
$(d0root)/d0geant/utl/bkd0rg.f:\
  $(d0root)/d0geant/utl/bkd0rg.for\
  $(d0library)/links/izd0rg.link\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gcmail.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/utl/bkd0rg.for | vmstounix > $(d0root)/d0geant/utl/bkd0rg.f
$(d0root)/d0geant/cal/bkgcah.f:\
  $(d0root)/d0geant/cal/bkgcah.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izgcah.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/bkgcah.for | vmstounix > $(d0root)/d0geant/cal/bkgcah.f
$(d0root)/d0geant/dzero/bkgswt.f:\
  $(d0root)/d0geant/dzero/bkgswt.for\
  $(d0library)/links/izgswt.link\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gcmail.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/bkgswt.for | vmstounix > $(d0root)/d0geant/dzero/bkgswt.f
$(d0root)/d0geant/zebio/bkisp2.f:\
  $(d0root)/d0geant/zebio/bkisp2.for\
  $(d0library)/links/izisp2.link\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/bkisp2.for | vmstounix > $(d0root)/d0geant/zebio/bkisp2.f
$(d0root)/d0geant/zebio/bkisv2.f:\
  $(d0root)/d0geant/zebio/bkisv2.for\
  $(d0library)/links/izisae.link\
  $(d0library)/links/izisv2.link\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/bkisv2.for | vmstounix > $(d0root)/d0geant/zebio/bkisv2.f
$(d0root)/d0geant/vtx/blcdd1.f:\
  $(d0root)/d0geant/vtx/blcdd1.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/links/izvrft.link\
  $(d0library)/inc/vtxlnk.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/vtxpsh.inc\
  $(d0library)/inc/vtloca.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/blcdd1.for | vmstounix > $(d0root)/d0geant/vtx/blcdd1.f
$(d0root)/d0geant/cdc/blcdd2.f:\
  $(d0root)/d0geant/cdc/blcdd2.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/links/izdrft.link\
  $(d0library)/inc/cdclnk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/blcdd2.for | vmstounix > $(d0root)/d0geant/cdc/blcdd2.f
$(d0root)/d0geant/fdc/blcdd3.f:\
  $(d0root)/d0geant/fdc/blcdd3.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/fdclnk.inc\
  $(d0library)/links/izfdrt.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/blcdd3.for | vmstounix > $(d0root)/d0geant/fdc/blcdd3.f
$(d0root)/d0geant/cdc/bldcda.f:\
  $(d0root)/d0geant/cdc/bldcda.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/bldcda.for | vmstounix > $(d0root)/d0geant/cdc/bldcda.f
$(d0root)/d0geant/cdc/bldsec.f:\
  $(d0root)/d0geant/cdc/bldsec.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/bldsec.for | vmstounix > $(d0root)/d0geant/cdc/bldsec.f
$(d0root)/d0geant/vtx/blvsec.f:\
  $(d0root)/d0geant/vtx/blvsec.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/vtloca.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/blvsec.for | vmstounix > $(d0root)/d0geant/vtx/blvsec.f
$(d0root)/d0geant/vtx/blvwda.f:\
  $(d0root)/d0geant/vtx/blvwda.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/vtloca.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/blvwda.for | vmstounix > $(d0root)/d0geant/vtx/blvwda.f
$(d0root)/d0geant/vtx/blvzda.f:\
  $(d0root)/d0geant/vtx/blvzda.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/vzdata.inc\
  $(d0library)/inc/vtloca.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/blvzda.for | vmstounix > $(d0root)/d0geant/vtx/blvzda.f
$(d0root)/d0geant/vtx/blvzla.f:\
  $(d0root)/d0geant/vtx/blvzla.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/vzhits.inc\
  $(d0library)/inc/vtloca.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/blvzla.for | vmstounix > $(d0root)/d0geant/vtx/blvzla.f
$(d0root)/d0geant/cal/bokcal.f:\
  $(d0root)/d0geant/cal/bokcal.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/bokcal.for | vmstounix > $(d0root)/d0geant/cal/bokcal.f
$(d0root)/d0geant/cdc/bokcdc.f:\
  $(d0root)/d0geant/cdc/bokcdc.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/bokcdc.for | vmstounix > $(d0root)/d0geant/cdc/bokcdc.f
$(d0root)/d0geant/fdc/bokfdc.f:\
  $(d0root)/d0geant/fdc/bokfdc.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/bokfdc.for | vmstounix > $(d0root)/d0geant/fdc/bokfdc.f
$(d0root)/d0geant/lv0/boklv0.f:\
  $(d0root)/d0geant/lv0/boklv0.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/lv0/boklv0.for | vmstounix > $(d0root)/d0geant/lv0/boklv0.f
$(d0root)/d0geant/muo/bokmuo.f:\
  $(d0root)/d0geant/muo/bokmuo.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/bokmuo.for | vmstounix > $(d0root)/d0geant/muo/bokmuo.f
$(d0root)/d0geant/samus/boksam.f:\
  $(d0root)/d0geant/samus/boksam.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/boksam.for | vmstounix > $(d0root)/d0geant/samus/boksam.f
$(d0root)/d0geant/trd/boktrd.f:\
  $(d0root)/d0geant/trd/boktrd.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/trinfo.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/boktrd.for | vmstounix > $(d0root)/d0geant/trd/boktrd.f
$(d0root)/d0geant/vtx/bokvtx.f:\
  $(d0root)/d0geant/vtx/bokvtx.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/bokvtx.for | vmstounix > $(d0root)/d0geant/vtx/bokvtx.f
$(d0root)/d0geant/caltower/cacmpr.f:\
  $(d0root)/d0geant/caltower/cacmpr.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/caltower/cacmpr.for | vmstounix > $(d0root)/d0geant/caltower/cacmpr.f
$(d0root)/d0geant/caltower/calbld.f:\
  $(d0root)/d0geant/caltower/calbld.for\
  $(d0library)/inc/caltrk.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gconst.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/hcal.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/caltower/calbld.for | vmstounix > $(d0root)/d0geant/caltower/calbld.f
$(d0root)/d0geant/utl/cd_head_flg.f:\
  $(d0root)/d0geant/utl/cd_head_flg.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/params/byte_order.params
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/utl/cd_head_flg.for | vmstounix > $(d0root)/d0geant/utl/cd_head_flg.f
$(d0root)/d0geant/zebio/cd_hstr_flg.f:\
  $(d0root)/d0geant/zebio/cd_hstr_flg.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/params/byte_order.params
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/cd_hstr_flg.for | vmstounix > $(d0root)/d0geant/zebio/cd_hstr_flg.f
$(d0root)/d0geant/cdc/cdatdl.f:\
  $(d0root)/d0geant/cdc/cdatdl.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/links/izdalh.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/cdatdl.for | vmstounix > $(d0root)/d0geant/cdc/cdatdl.f
$(d0root)/d0geant/cdc/cdatsw.f:\
  $(d0root)/d0geant/cdc/cdatsw.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdalh.link\
  $(d0library)/inc/gcunit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/cdatsw.for | vmstounix > $(d0root)/d0geant/cdc/cdatsw.f
$(d0root)/d0geant/cdc/cdcwrt.f:\
  $(d0root)/d0geant/cdc/cdcwrt.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/cdclnk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/cdcwrt.for | vmstounix > $(d0root)/d0geant/cdc/cdcwrt.f
$(d0root)/d0geant/vtx/cdd1fl.f:\
  $(d0root)/d0geant/vtx/cdd1fl.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/links/izcdd1.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/cdd1fl.for | vmstounix > $(d0root)/d0geant/vtx/cdd1fl.f
$(d0root)/d0geant/cdc/cdd2fl.f:\
  $(d0root)/d0geant/cdc/cdd2fl.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/links/izcdd2.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/cdd2fl.for | vmstounix > $(d0root)/d0geant/cdc/cdd2fl.f
$(d0root)/d0geant/fdc/cdd3fl.f:\
  $(d0root)/d0geant/fdc/cdd3fl.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/links/izcdd3.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/cdd3fl.for | vmstounix > $(d0root)/d0geant/fdc/cdd3fl.f
$(d0root)/d0geant/trd/cdd4fl.f:\
  $(d0root)/d0geant/trd/cdd4fl.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/links/izcdd4.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/cdd4fl.for | vmstounix > $(d0root)/d0geant/trd/cdd4fl.f
$(d0root)/d0geant/cdc/cdelay.f:\
  $(d0root)/d0geant/cdc/cdelay.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdrft.link\
  $(d0library)/inc/gcunit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/cdelay.for | vmstounix > $(d0root)/d0geant/cdc/cdelay.f
$(d0root)/d0geant/cdc/cdhits.f:\
  $(d0root)/d0geant/cdc/cdhits.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/links/izdalh.link\
  $(d0library)/links/izdrft.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/cdhits.for | vmstounix > $(d0root)/d0geant/cdc/cdhits.f
$(d0root)/d0geant/cdc/cdresp.f:\
  $(d0root)/d0geant/cdc/cdresp.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/cdresp.for | vmstounix > $(d0root)/d0geant/cdc/cdresp.f
$(d0root)/d0geant/cdc/cdrift.f:\
  $(d0root)/d0geant/cdc/cdrift.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/cdrift.for | vmstounix > $(d0root)/d0geant/cdc/cdrift.f
$(d0root)/d0geant/vtx/chgdiv.f:\
  $(d0root)/d0geant/vtx/chgdiv.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/chgdiv.for | vmstounix > $(d0root)/d0geant/vtx/chgdiv.f
$(d0root)/d0geant/zebio/chkbrr.f:\
  $(d0root)/d0geant/zebio/chkbrr.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izd0rgz.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/chkbrr.for | vmstounix > $(d0root)/d0geant/zebio/chkbrr.f
$(d0root)/d0geant/cdc/clanda.f:\
  $(d0root)/d0geant/cdc/clanda.for\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gckine.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/clanda.for | vmstounix > $(d0root)/d0geant/cdc/clanda.f
$(d0root)/d0geant/cal/clncal.f:\
  $(d0root)/d0geant/cal/clncal.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/dcalog.inc\
  $(d0library)/inc/drop.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/clncal.for | vmstounix > $(d0root)/d0geant/cal/clncal.f
$(d0root)/d0geant/cdc/clncdc.f:\
  $(d0root)/d0geant/cdc/clncdc.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/clncdc.for | vmstounix > $(d0root)/d0geant/cdc/clncdc.f
$(d0root)/d0geant/fdc/clnfdc.f:\
  $(d0root)/d0geant/fdc/clnfdc.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/clnfdc.for | vmstounix > $(d0root)/d0geant/fdc/clnfdc.f
$(d0root)/d0geant/lv0/clnlv0.f:\
  $(d0root)/d0geant/lv0/clnlv0.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/lv0/clnlv0.for | vmstounix > $(d0root)/d0geant/lv0/clnlv0.f
$(d0root)/d0geant/muo/clnmuo.f:\
  $(d0root)/d0geant/muo/clnmuo.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/clnmuo.for | vmstounix > $(d0root)/d0geant/muo/clnmuo.f
$(d0root)/d0geant/samus/clnsam.f:\
  $(d0root)/d0geant/samus/clnsam.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/clnsam.for | vmstounix > $(d0root)/d0geant/samus/clnsam.f
$(d0root)/d0geant/trd/clntrd.f:\
  $(d0root)/d0geant/trd/clntrd.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/clntrd.for | vmstounix > $(d0root)/d0geant/trd/clntrd.f
$(d0root)/d0geant/vtx/clnvtx.f:\
  $(d0root)/d0geant/vtx/clnvtx.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/clnvtx.for | vmstounix > $(d0root)/d0geant/vtx/clnvtx.f
$(d0root)/d0geant/trd/clsmen.f:\
  $(d0root)/d0geant/trd/clsmen.for\
  $(d0library)/inc/nortrd.inc\
  $(d0library)/inc/prnevt.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/clsmen.for | vmstounix > $(d0root)/d0geant/trd/clsmen.f
$(d0root)/d0geant/trd/clucle.f:\
  $(d0root)/d0geant/trd/clucle.for\
  $(d0library)/inc/clusm.inc\
  $(d0library)/inc/clustr.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/clucle.for | vmstounix > $(d0root)/d0geant/trd/clucle.f
$(d0root)/d0geant/trd/cludel.f:\
  $(d0root)/d0geant/trd/cludel.for\
  $(d0library)/inc/clucon.inc\
  $(d0library)/inc/clugen.inc\
  $(d0library)/inc/clusm.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/enetrd.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/info.inc\
  $(d0library)/inc/probde.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/cludel.for | vmstounix > $(d0root)/d0geant/trd/cludel.f
$(d0root)/d0geant/trd/cludig.f:\
  $(d0root)/d0geant/trd/cludig.for\
  $(d0library)/inc/clusm.inc\
  $(d0library)/inc/clustr.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/enetrd.inc\
  $(d0library)/inc/faddig.inc\
  $(d0library)/inc/fadccn.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/geomtc.inc\
  $(d0library)/inc/geomtr.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/nortrd.inc\
  $(d0library)/inc/posit.inc\
  $(d0library)/inc/prnevt.inc\
  $(d0library)/inc/trinfo.inc\
  $(d0library)/inc/worksp.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gcflag.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/cludig.for | vmstounix > $(d0root)/d0geant/trd/cludig.f
$(d0root)/d0geant/trd/clumix.f:\
  $(d0root)/d0geant/trd/clumix.for\
  $(d0library)/inc/clusm.inc\
  $(d0library)/inc/worksp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/clumix.for | vmstounix > $(d0root)/d0geant/trd/clumix.f
$(d0root)/d0geant/trd/clumrg.f:\
  $(d0root)/d0geant/trd/clumrg.for\
  $(d0library)/inc/clusm.inc\
  $(d0library)/inc/clustr.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/info.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/clumrg.for | vmstounix > $(d0root)/d0geant/trd/clumrg.f
$(d0root)/d0geant/trd/clupos.f:\
  $(d0root)/d0geant/trd/clupos.for\
  $(d0library)/inc/clusm.inc\
  $(d0library)/inc/clustr.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gconst.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/geomtc.inc\
  $(d0library)/inc/geomtr.inc\
  $(d0library)/inc/info.inc\
  $(d0library)/inc/posit.inc\
  $(d0library)/inc/worksp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/clupos.for | vmstounix > $(d0root)/d0geant/trd/clupos.f
$(d0root)/d0geant/trd/cluprn.f:\
  $(d0root)/d0geant/trd/cluprn.for\
  $(d0library)/inc/clusm.inc\
  $(d0library)/inc/clustr.inc\
  $(d0library)/inc/gcunit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/cluprn.for | vmstounix > $(d0root)/d0geant/trd/cluprn.f
$(d0root)/d0geant/trd/clurad.f:\
  $(d0root)/d0geant/trd/clurad.for\
  $(d0library)/inc/absor.inc\
  $(d0library)/inc/clugen.inc\
  $(d0library)/inc/clusm.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/enetrd.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/posit.inc\
  $(d0library)/inc/trinfo.inc\
  $(d0library)/inc/xray.inc\
  $(d0library)/inc/xspect.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/clurad.for | vmstounix > $(d0root)/d0geant/trd/clurad.f
$(d0root)/d0geant/trd/clusmr.f:\
  $(d0root)/d0geant/trd/clusmr.for\
  $(d0library)/inc/clusm.inc\
  $(d0library)/inc/clustr.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/enetrd.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/info.inc\
  $(d0library)/inc/tecdsc.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/clusmr.for | vmstounix > $(d0root)/d0geant/trd/clusmr.f
$(d0root)/d0geant/trd/clusor.f:\
  $(d0root)/d0geant/trd/clusor.for\
  $(d0library)/inc/clusm.inc\
  $(d0library)/inc/clustr.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/worksp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/clusor.for | vmstounix > $(d0root)/d0geant/trd/clusor.f
$(d0root)/d0geant/trd/clustf.f:\
  $(d0root)/d0geant/trd/clustf.for\
  $(d0library)/inc/clurec.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/tgrav.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/clustf.for | vmstounix > $(d0root)/d0geant/trd/clustf.f
$(d0root)/d0geant/zebio/cpd0rg.f:\
  $(d0root)/d0geant/zebio/cpd0rg.for\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/links/izd0rg.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/cpd0rg.for | vmstounix > $(d0root)/d0geant/zebio/cpd0rg.f
$(d0root)/d0geant/dzero/d0main.f:\
  $(d0root)/d0geant/dzero/d0main.for\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gctime.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/params/nblank.def\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gctime.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/params/nblank.def\
  $(d0library)/inc/gvect.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/params/nblank.def
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/d0main.for | vmstounix > $(d0root)/d0geant/dzero/d0main.f
$(d0root)/d0geant/dzero/d0xint.f:\
  $(d0root)/d0geant/dzero/d0xint.for\
  $(d0library)/inc/gctime.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gcflag.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/d0xint.for | vmstounix > $(d0root)/d0geant/dzero/d0xint.f
$(d0root)/d0geant/cdc/datapk.f:\
  $(d0root)/d0geant/cdc/datapk.for\
  $(d0library)/inc/gcunit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/datapk.for | vmstounix > $(d0root)/d0geant/cdc/datapk.f
$(d0root)/d0geant/caltower/dchit.f:\
  $(d0root)/d0geant/caltower/dchit.for\
  $(d0library)/inc/gconst.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/caltower/dchit.for | vmstounix > $(d0root)/d0geant/caltower/dchit.f
$(d0root)/d0geant/caltower/dehit.f:\
  $(d0root)/d0geant/caltower/dehit.for\
  $(d0library)/inc/gconst.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/caltower/dehit.for | vmstounix > $(d0root)/d0geant/caltower/dehit.f
$(d0root)/d0geant/fdc/deldig.f:\
  $(d0root)/d0geant/fdc/deldig.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/deldig.for | vmstounix > $(d0root)/d0geant/fdc/deldig.f
$(d0root)/d0geant/cal/detcal.f:\
  $(d0root)/d0geant/cal/detcal.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/detcal.for | vmstounix > $(d0root)/d0geant/cal/detcal.f
$(d0root)/d0geant/cdc/detcdc.f:\
  $(d0root)/d0geant/cdc/detcdc.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/detcdc.for | vmstounix > $(d0root)/d0geant/cdc/detcdc.f
$(d0root)/d0geant/cal/detded.f:\
  $(d0root)/d0geant/cal/detded.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/detded.for | vmstounix > $(d0root)/d0geant/cal/detded.f
$(d0root)/d0geant/cal/deteca.f:\
  $(d0root)/d0geant/cal/deteca.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/deteca.for | vmstounix > $(d0root)/d0geant/cal/deteca.f
$(d0root)/d0geant/fdc/detfdc.f:\
  $(d0root)/d0geant/fdc/detfdc.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/detfdc.for | vmstounix > $(d0root)/d0geant/fdc/detfdc.f
$(d0root)/d0geant/lv0/detlv0.f:\
  $(d0root)/d0geant/lv0/detlv0.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/lv0/detlv0.for | vmstounix > $(d0root)/d0geant/lv0/detlv0.f
$(d0root)/d0geant/muo/detmuo.f:\
  $(d0root)/d0geant/muo/detmuo.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/detmuo.for | vmstounix > $(d0root)/d0geant/muo/detmuo.f
$(d0root)/d0geant/muo/detmuo_v1.f:\
  $(d0root)/d0geant/muo/detmuo_v1.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/detmuo_v1.for | vmstounix > $(d0root)/d0geant/muo/detmuo_v1.f
$(d0root)/d0geant/samus/detsam.f:\
  $(d0root)/d0geant/samus/detsam.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/detsam.for | vmstounix > $(d0root)/d0geant/samus/detsam.f
$(d0root)/d0geant/cal/detscn.f:\
  $(d0root)/d0geant/cal/detscn.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/detscn.for | vmstounix > $(d0root)/d0geant/cal/detscn.f
$(d0root)/d0geant/trd/dettrd.f:\
  $(d0root)/d0geant/trd/dettrd.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/dettrd.for | vmstounix > $(d0root)/d0geant/trd/dettrd.f
$(d0root)/d0geant/cal/detuca.f:\
  $(d0root)/d0geant/cal/detuca.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/detuca.for | vmstounix > $(d0root)/d0geant/cal/detuca.f
$(d0root)/d0geant/vtx/detvtx.f:\
  $(d0root)/d0geant/vtx/detvtx.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/detvtx.for | vmstounix > $(d0root)/d0geant/vtx/detvtx.f
$(d0root)/d0geant/dzero/dffini.f:\
  $(d0root)/d0geant/dzero/dffini.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/zebio.inc\
  $(d0library)/inc/zebiog.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/dffini.for | vmstounix > $(d0root)/d0geant/dzero/dffini.f
$(d0root)/d0geant/caltower/dhstor.f:\
  $(d0root)/d0geant/caltower/dhstor.for\
  $(d0library)/inc/caltrk.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/caltower/dhstor.for | vmstounix > $(d0root)/d0geant/caltower/dhstor.f
$(d0root)/d0geant/cal/digcal.f:\
  $(d0root)/d0geant/cal/digcal.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/dcalog.inc\
  $(d0library)/inc/hcal.inc\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/digcal.for | vmstounix > $(d0root)/d0geant/cal/digcal.f
$(d0root)/d0geant/cdc/digcdc.f:\
  $(d0root)/d0geant/cdc/digcdc.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izdrft.link\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/cdclnk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/digcdc.for | vmstounix > $(d0root)/d0geant/cdc/digcdc.f
$(d0root)/d0geant/cal/digded.f:\
  $(d0root)/d0geant/cal/digded.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/hcal.inc\
  $(d0library)/inc/gcunit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/digded.for | vmstounix > $(d0root)/d0geant/cal/digded.f
$(d0root)/d0geant/cal/digeca.f:\
  $(d0root)/d0geant/cal/digeca.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/hcal.inc\
  $(d0library)/inc/gcunit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/digeca.for | vmstounix > $(d0root)/d0geant/cal/digeca.f
$(d0root)/d0geant/fdc/digfdc.f:\
  $(d0root)/d0geant/fdc/digfdc.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/fdclnk.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/digfdc.for | vmstounix > $(d0root)/d0geant/fdc/digfdc.f
$(d0root)/d0geant/lv0/diglv0.f:\
  $(d0root)/d0geant/lv0/diglv0.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/lv0param.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcunit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/lv0/diglv0.for | vmstounix > $(d0root)/d0geant/lv0/diglv0.f
$(d0root)/d0geant/muo/digmuo.f:\
  $(d0root)/d0geant/muo/digmuo.for\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/digmuo.for | vmstounix > $(d0root)/d0geant/muo/digmuo.f
$(d0root)/d0geant/muo/digmuo_v1.f:\
  $(d0root)/d0geant/muo/digmuo_v1.for\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/digmuo_v1.for | vmstounix > $(d0root)/d0geant/muo/digmuo_v1.f
$(d0root)/d0geant/samus/digsam.f:\
  $(d0root)/d0geant/samus/digsam.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/digsam.for | vmstounix > $(d0root)/d0geant/samus/digsam.f
$(d0root)/d0geant/cal/digscn.f:\
  $(d0root)/d0geant/cal/digscn.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/hcal.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/digscn.for | vmstounix > $(d0root)/d0geant/cal/digscn.f
$(d0root)/d0geant/trd/digtrd.f:\
  $(d0root)/d0geant/trd/digtrd.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/nortrd.inc\
  $(d0library)/inc/fadccn.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/geomtc.inc\
  $(d0library)/inc/geomtr.inc\
  $(d0library)/inc/trinfo.inc\
  $(d0library)/inc/tecdsc.inc\
  $(d0library)/inc/gtrhln.inc\
  $(d0library)/inc/worksp.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pedtrd.inc\
  $(d0library)/links/izcdd4.link\
  $(d0library)/links/izgean.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/digtrd.for | vmstounix > $(d0root)/d0geant/trd/digtrd.f
$(d0root)/d0geant/cal/diguca.f:\
  $(d0root)/d0geant/cal/diguca.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/hcal.inc\
  $(d0library)/inc/gcunit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/diguca.for | vmstounix > $(d0root)/d0geant/cal/diguca.f
$(d0root)/d0geant/vtx/digvtx.f:\
  $(d0root)/d0geant/vtx/digvtx.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/vtxlnk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/digvtx.for | vmstounix > $(d0root)/d0geant/vtx/digvtx.f
$(d0root)/d0geant/utl/dodigi.f:\
  $(d0root)/d0geant/utl/dodigi.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcsets.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/utl/dodigi.for | vmstounix > $(d0root)/d0geant/utl/dodigi.f
$(d0root)/d0geant/caltower/dschit.f:\
  $(d0root)/d0geant/caltower/dschit.for\
  $(d0library)/inc/gcsets.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/caltower/dschit.for | vmstounix > $(d0root)/d0geant/caltower/dschit.f
$(d0root)/d0geant/caltower/dshit.f:\
  $(d0root)/d0geant/caltower/dshit.for\
  $(d0library)/inc/gcsets.inc\
  $(d0library)/inc/gcunit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/caltower/dshit.for | vmstounix > $(d0root)/d0geant/caltower/dshit.f
$(d0root)/d0geant/caltower/duhit.f:\
  $(d0root)/d0geant/caltower/duhit.for\
  $(d0library)/inc/gconst.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/caltower/duhit.for | vmstounix > $(d0root)/d0geant/caltower/duhit.f
$(d0root)/d0geant/cdc/dvecda.f:\
  $(d0root)/d0geant/cdc/dvecda.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/dvecda.for | vmstounix > $(d0root)/d0geant/cdc/dvecda.f
$(d0root)/d0geant/trd/dwdegr.f:\
  $(d0root)/d0geant/trd/dwdegr.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/grutil.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/dwdegr.for | vmstounix > $(d0root)/d0geant/trd/dwdegr.f
$(d0root)/d0geant/dzero/dzemat.f:\
  $(d0root)/d0geant/dzero/dzemat.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/dzemat.for | vmstounix > $(d0root)/d0geant/dzero/dzemat.f
$(d0root)/d0geant/dzero/dzero.f:\
  $(d0root)/d0geant/dzero/dzero.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/dzero.for | vmstounix > $(d0root)/d0geant/dzero/dzero.f
$(d0root)/d0geant/caltower/ecindx.f:\
  $(d0root)/d0geant/caltower/ecindx.for\
  $(d0library)/inc/gconst.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/caltower/ecindx.for | vmstounix > $(d0root)/d0geant/caltower/ecindx.f
$(d0root)/d0geant/trd/edegen.f:\
  $(d0root)/d0geant/trd/edegen.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/disdel.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/edegen.for | vmstounix > $(d0root)/d0geant/trd/edegen.f
$(d0root)/d0geant/zebio/endzeb.f:\
  $(d0root)/d0geant/zebio/endzeb.for\
  $(d0library)/inc/zebio.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/endzeb.for | vmstounix > $(d0root)/d0geant/zebio/endzeb.f
$(d0root)/d0geant/cal/enicdh.f:\
  $(d0root)/d0geant/cal/enicdh.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcunit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/enicdh.for | vmstounix > $(d0root)/d0geant/cal/enicdh.f
$(d0root)/d0geant/cal/enjets.f:\
  $(d0root)/d0geant/cal/enjets.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gconst.inc\
  $(d0library)/inc/gcunit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/enjets.for | vmstounix > $(d0root)/d0geant/cal/enjets.f
$(d0root)/d0geant/cal/enjpts.f:\
  $(d0root)/d0geant/cal/enjpts.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izjpts.link\
  $(d0library)/params/byte_order.params\
  $(d0library)/inc/gcunit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/enjpts.for | vmstounix > $(d0root)/d0geant/cal/enjpts.f
$(d0root)/d0geant/trd/entrd.f:\
  $(d0root)/d0geant/trd/entrd.for\
  $(d0library)/inc/clugen.inc\
  $(d0library)/inc/clusm.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/enetrd.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/posit.inc\
  $(d0library)/inc/prnevt.inc\
  $(d0library)/inc/trinfo.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/entrd.for | vmstounix > $(d0root)/d0geant/trd/entrd.f
$(d0root)/d0geant/trd/exrgen.f:\
  $(d0root)/d0geant/trd/exrgen.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/tabx.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/exrgen.for | vmstounix > $(d0root)/d0geant/trd/exrgen.f
$(d0root)/d0geant/trd/extrap.f:\
  $(d0root)/d0geant/trd/extrap.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/extrap.for | vmstounix > $(d0root)/d0geant/trd/extrap.f
$(d0root)/d0geant/cdc/fadcdl.f:\
  $(d0root)/d0geant/cdc/fadcdl.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/fadcdl.for | vmstounix > $(d0root)/d0geant/cdc/fadcdl.f
$(d0root)/d0geant/cdc/fadcfr.f:\
  $(d0root)/d0geant/cdc/fadcfr.for\
  $(d0library)/inc/zpulpr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/fadcfr.for | vmstounix > $(d0root)/d0geant/cdc/fadcfr.f
$(d0root)/d0geant/cdc/fadcsw.f:\
  $(d0root)/d0geant/cdc/fadcsw.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/fadcsw.for | vmstounix > $(d0root)/d0geant/cdc/fadcsw.f
$(d0root)/d0geant/vtx/fadcvs.f:\
  $(d0root)/d0geant/vtx/fadcvs.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/fadcvs.for | vmstounix > $(d0root)/d0geant/vtx/fadcvs.f
$(d0root)/d0geant/vtx/fadcvz.f:\
  $(d0root)/d0geant/vtx/fadcvz.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/fadcvz.for | vmstounix > $(d0root)/d0geant/vtx/fadcvz.f
$(d0root)/d0geant/dzero/fajxyz.f:\
  $(d0root)/d0geant/dzero/fajxyz.for\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gcnum.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/cenmv.inc\
  $(d0library)/inc/gditrp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/fajxyz.for | vmstounix > $(d0root)/d0geant/dzero/fajxyz.f
$(d0root)/d0geant/fdc/fdcwrt.f:\
  $(d0root)/d0geant/fdc/fdcwrt.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/fdclnk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/fdcwrt.for | vmstounix > $(d0root)/d0geant/fdc/fdcwrt.f
$(d0root)/d0geant/cal/ffical.f:\
  $(d0root)/d0geant/cal/ffical.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/dcalog.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/ffical.for | vmstounix > $(d0root)/d0geant/cal/ffical.f
$(d0root)/d0geant/cdc/fficdc.f:\
  $(d0root)/d0geant/cdc/fficdc.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/fficdc.for | vmstounix > $(d0root)/d0geant/cdc/fficdc.f
$(d0root)/d0geant/d0/ffid0.f:\
  $(d0root)/d0geant/d0/ffid0.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/d0/ffid0.for | vmstounix > $(d0root)/d0geant/d0/ffid0.f
$(d0root)/d0geant/fdc/ffifdc.f:\
  $(d0root)/d0geant/fdc/ffifdc.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/ffifdc.for | vmstounix > $(d0root)/d0geant/fdc/ffifdc.f
$(d0root)/d0geant/lv0/ffilv0.f:\
  $(d0root)/d0geant/lv0/ffilv0.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/lv0/ffilv0.for | vmstounix > $(d0root)/d0geant/lv0/ffilv0.f
$(d0root)/d0geant/muo/ffimuo.f:\
  $(d0root)/d0geant/muo/ffimuo.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/ffimuo.for | vmstounix > $(d0root)/d0geant/muo/ffimuo.f
$(d0root)/d0geant/samus/ffisam.f:\
  $(d0root)/d0geant/samus/ffisam.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/ffisam.for | vmstounix > $(d0root)/d0geant/samus/ffisam.f
$(d0root)/d0geant/trd/ffitrd.f:\
  $(d0root)/d0geant/trd/ffitrd.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/ffitrd.for | vmstounix > $(d0root)/d0geant/trd/ffitrd.f
$(d0root)/d0geant/vtx/ffivtx.f:\
  $(d0root)/d0geant/vtx/ffivtx.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/ffivtx.for | vmstounix > $(d0root)/d0geant/vtx/ffivtx.f
$(d0root)/d0geant/vtx/ficdd1.f:\
  $(d0root)/d0geant/vtx/ficdd1.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/vtxpsh.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/ficdd1.for | vmstounix > $(d0root)/d0geant/vtx/ficdd1.f
$(d0root)/d0geant/cdc/ficdd2.f:\
  $(d0root)/d0geant/cdc/ficdd2.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/links/izdtmw.link\
  $(d0library)/links/izdtmd.link\
  $(d0library)/links/izdpdl.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/ficdd2.for | vmstounix > $(d0root)/d0geant/cdc/ficdd2.f
$(d0root)/d0geant/fdc/ficdd3.f:\
  $(d0root)/d0geant/fdc/ficdd3.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/fdclnk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/ficdd3.for | vmstounix > $(d0root)/d0geant/fdc/ficdd3.f
$(d0root)/d0geant/cdc/fidcda.f:\
  $(d0root)/d0geant/cdc/fidcda.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/cdclnk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/fidcda.for | vmstounix > $(d0root)/d0geant/cdc/fidcda.f
$(d0root)/d0geant/cdc/fidsec.f:\
  $(d0root)/d0geant/cdc/fidsec.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/cdclnk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/fidsec.for | vmstounix > $(d0root)/d0geant/cdc/fidsec.f
$(d0root)/d0geant/vtx/fivzda.f:\
  $(d0root)/d0geant/vtx/fivzda.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/vzdata.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/fivzda.for | vmstounix > $(d0root)/d0geant/vtx/fivzda.f
$(d0root)/d0geant/dzero/fld0rg.f:\
  $(d0root)/d0geant/dzero/fld0rg.for\
  $(d0library)/inc/gcbank.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/fld0rg.for | vmstounix > $(d0root)/d0geant/dzero/fld0rg.f
$(d0root)/d0geant/caltower/flgcah.f:\
  $(d0root)/d0geant/caltower/flgcah.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/caltower/flgcah.for | vmstounix > $(d0root)/d0geant/caltower/flgcah.f
$(d0root)/d0geant/dzero/flgval.f:\
  $(d0root)/d0geant/dzero/flgval.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/flgval.for | vmstounix > $(d0root)/d0geant/dzero/flgval.f
$(d0root)/d0geant/cal/flicdh.f:\
  $(d0root)/d0geant/cal/flicdh.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gcsets.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcmate.inc\
  $(d0library)/params/byte_order.params
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/flicdh.for | vmstounix > $(d0root)/d0geant/cal/flicdh.f
$(d0root)/d0geant/cal/fljets.f:\
  $(d0root)/d0geant/cal/fljets.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gcunit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/fljets.for | vmstounix > $(d0root)/d0geant/cal/fljets.f
$(d0root)/d0geant/cal/fljpts.f:\
  $(d0root)/d0geant/cal/fljpts.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izjpts.link\
  $(d0library)/params/byte_order.params\
  $(d0library)/inc/gcunit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/fljpts.for | vmstounix > $(d0root)/d0geant/cal/fljpts.f
$(d0root)/d0geant/fdc/fpdata.f:\
  $(d0root)/d0geant/fdc/fpdata.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/fdloca.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/fpdata.for | vmstounix > $(d0root)/d0geant/fdc/fpdata.f
$(d0root)/d0geant/fdc/fphits.f:\
  $(d0root)/d0geant/fdc/fphits.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/fdloca.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/fphits.for | vmstounix > $(d0root)/d0geant/fdc/fphits.f
$(d0root)/d0geant/fdc/ftdata.f:\
  $(d0root)/d0geant/fdc/ftdata.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/fdloca.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/ftdata.for | vmstounix > $(d0root)/d0geant/fdc/ftdata.f
$(d0root)/d0geant/fdc/ftdlda.f:\
  $(d0root)/d0geant/fdc/ftdlda.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/fdloca.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/ftdlda.for | vmstounix > $(d0root)/d0geant/fdc/ftdlda.f
$(d0root)/d0geant/fdc/fthits.f:\
  $(d0root)/d0geant/fdc/fthits.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/fdloca.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/fthits.for | vmstounix > $(d0root)/d0geant/fdc/fthits.f
$(d0root)/d0geant/dzero/gdcole.f:\
  $(d0root)/d0geant/dzero/gdcole.for\
  $(d0library)/inc/gcunit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/gdcole.for | vmstounix > $(d0root)/d0geant/dzero/gdcole.f
$(d0root)/d0geant/zebio/geaisa.f:\
  $(d0root)/d0geant/zebio/geaisa.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/geaisa.for | vmstounix > $(d0root)/d0geant/zebio/geaisa.f
$(d0root)/d0geant/cal/geecel.f:\
  $(d0root)/d0geant/cal/geecel.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/geecel.for | vmstounix > $(d0root)/d0geant/cal/geecel.f
$(d0root)/d0geant/cal/geecip.f:\
  $(d0root)/d0geant/cal/geecip.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/geecip.for | vmstounix > $(d0root)/d0geant/cal/geecip.f
$(d0root)/d0geant/cal/geecmh.f:\
  $(d0root)/d0geant/cal/geecmh.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/geecmh.for | vmstounix > $(d0root)/d0geant/cal/geecmh.f
$(d0root)/d0geant/cal/geecoh.f:\
  $(d0root)/d0geant/cal/geecoh.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/geecoh.for | vmstounix > $(d0root)/d0geant/cal/geecoh.f
$(d0root)/d0geant/cal/geocal.f:\
  $(d0root)/d0geant/cal/geocal.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/geocal.for | vmstounix > $(d0root)/d0geant/cal/geocal.f
$(d0root)/d0geant/cdc/geocdc.f:\
  $(d0root)/d0geant/cdc/geocdc.for\
  $(d0library)/inc/cenmv.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/links/izdwal.link\
  $(d0library)/links/izdmat.link\
  $(d0library)/links/izdrft.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/geocdc.for | vmstounix > $(d0root)/d0geant/cdc/geocdc.f
$(d0root)/d0geant/d0/geod0.f:\
  $(d0root)/d0geant/d0/geod0.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/d0/geod0.for | vmstounix > $(d0root)/d0geant/d0/geod0.f
$(d0root)/d0geant/cal/geoeca.f:\
  $(d0root)/d0geant/cal/geoeca.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/geoeca.for | vmstounix > $(d0root)/d0geant/cal/geoeca.f
$(d0root)/d0geant/cal/geoecr.f:\
  $(d0root)/d0geant/cal/geoecr.for\
  $(d0library)/inc/cryvln.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/srcpr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/geoecr.for | vmstounix > $(d0root)/d0geant/cal/geoecr.f
$(d0root)/d0geant/fdc/geofdc.f:\
  $(d0root)/d0geant/fdc/geofdc.for\
  $(d0library)/inc/cenmv.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/links/izfwal.link\
  $(d0library)/links/izfwta.link\
  $(d0library)/links/izfwtb.link\
  $(d0library)/links/izfwph.link\
  $(d0library)/links/izfmat.link\
  $(d0library)/links/izfdrt.link\
  $(d0library)/links/izfdta.link\
  $(d0library)/links/izfdtb.link\
  $(d0library)/links/izfdph.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/geofdc.for | vmstounix > $(d0root)/d0geant/fdc/geofdc.f
$(d0root)/d0geant/lv0/geolv0.f:\
  $(d0root)/d0geant/lv0/geolv0.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/lv0/geolv0.for | vmstounix > $(d0root)/d0geant/lv0/geolv0.f
$(d0root)/d0geant/muo/geomuo.f:\
  $(d0root)/d0geant/muo/geomuo.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/geomuo.for | vmstounix > $(d0root)/d0geant/muo/geomuo.f
$(d0root)/d0geant/dzero/geomvl.f:\
  $(d0root)/d0geant/dzero/geomvl.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/srcpr.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/geomvl.for | vmstounix > $(d0root)/d0geant/dzero/geomvl.f
$(d0root)/d0geant/samus/geosam.f:\
  $(d0root)/d0geant/samus/geosam.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/geosam.for | vmstounix > $(d0root)/d0geant/samus/geosam.f
$(d0root)/d0geant/trd/geotrd.f:\
  $(d0root)/d0geant/trd/geotrd.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/trdvol.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/links/iztach.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/geotrd.for | vmstounix > $(d0root)/d0geant/trd/geotrd.f
$(d0root)/d0geant/cal/geouca.f:\
  $(d0root)/d0geant/cal/geouca.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/geouca.for | vmstounix > $(d0root)/d0geant/cal/geouca.f
$(d0root)/d0geant/cal/geoucr.f:\
  $(d0root)/d0geant/cal/geoucr.for\
  $(d0library)/inc/cryvln.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/srcpr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/geoucr.for | vmstounix > $(d0root)/d0geant/cal/geoucr.f
$(d0root)/d0geant/vtx/geovtx.f:\
  $(d0root)/d0geant/vtx/geovtx.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cenmv.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/links/izvwal.link\
  $(d0library)/links/izvmat.link\
  $(d0library)/links/izvrft.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/geovtx.for | vmstounix > $(d0root)/d0geant/vtx/geovtx.f
$(d0root)/d0geant/cal/gescin.f:\
  $(d0root)/d0geant/cal/gescin.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/gescin.for | vmstounix > $(d0root)/d0geant/cal/gescin.f
$(d0root)/d0geant/vtx/get_pulse.f:\
  $(d0root)/d0geant/vtx/get_pulse.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/get_pulse.for | vmstounix > $(d0root)/d0geant/vtx/get_pulse.f
$(d0root)/d0geant/caltower/get_type_interact.f:\
  $(d0root)/d0geant/caltower/get_type_interact.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/caltower/get_type_interact.for | vmstounix > $(d0root)/d0geant/caltower/get_type_interact.f
$(d0root)/d0geant/caltower/getbin.f:\
  $(d0root)/d0geant/caltower/getbin.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/caltower/getbin.for | vmstounix > $(d0root)/d0geant/caltower/getbin.f
$(d0root)/d0geant/caltower/getcel.f:\
  $(d0root)/d0geant/caltower/getcel.for\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/caltower/getcel.for | vmstounix > $(d0root)/d0geant/caltower/getcel.f
$(d0root)/d0geant/utl/getdet.f:\
  $(d0root)/d0geant/utl/getdet.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/utl/getdet.for | vmstounix > $(d0root)/d0geant/utl/getdet.f
$(d0root)/d0geant/cal/geucel.f:\
  $(d0root)/d0geant/cal/geucel.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/geucel.for | vmstounix > $(d0root)/d0geant/cal/geucel.f
$(d0root)/d0geant/cal/geucfh.f:\
  $(d0root)/d0geant/cal/geucfh.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/geucfh.for | vmstounix > $(d0root)/d0geant/cal/geucfh.f
$(d0root)/d0geant/cal/geuclk.f:\
  $(d0root)/d0geant/cal/geuclk.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/geuclk.for | vmstounix > $(d0root)/d0geant/cal/geuclk.f
$(d0root)/d0geant/cal/geumsg.f:\
  $(d0root)/d0geant/cal/geumsg.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/geumsg.for | vmstounix > $(d0root)/d0geant/cal/geumsg.f
$(d0root)/d0geant/dzero/gftver.f:\
  $(d0root)/d0geant/dzero/gftver.for\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gcmail.inc\
  $(d0library)/inc/gcnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/gftver.for | vmstounix > $(d0root)/d0geant/dzero/gftver.f
$(d0root)/d0geant/dzero/gfver1.f:\
  $(d0root)/d0geant/dzero/gfver1.for\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gcmail.inc\
  $(d0library)/inc/gcnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/gfver1.for | vmstounix > $(d0root)/d0geant/dzero/gfver1.f
$(d0root)/d0geant/vtx/gget.f:\
  $(d0root)/d0geant/vtx/gget.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/gget.for | vmstounix > $(d0root)/d0geant/vtx/gget.f
$(d0root)/d0geant/dzero/ginvol.f:\
  $(d0root)/d0geant/dzero/ginvol.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/ginvol.for | vmstounix > $(d0root)/d0geant/dzero/ginvol.f
$(d0root)/d0geant/dzero/gitran.f:\
  $(d0root)/d0geant/dzero/gitran.for\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/gitran.for | vmstounix > $(d0root)/d0geant/dzero/gitran.f
$(d0root)/d0geant/dzero/gltrac.f:\
  $(d0root)/d0geant/dzero/gltrac.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/gltrac.for | vmstounix > $(d0root)/d0geant/dzero/gltrac.f
$(d0root)/d0geant/dzero/gnsout.f:\
  $(d0root)/d0geant/dzero/gnsout.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/gnsout.for | vmstounix > $(d0root)/d0geant/dzero/gnsout.f
$(d0root)/d0geant/dzero/goking.f:\
  $(d0root)/d0geant/dzero/goking.for\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcking.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/dcalog.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/goking.for | vmstounix > $(d0root)/d0geant/dzero/goking.f
$(d0root)/d0geant/dzero/gsmate.f:\
  $(d0root)/d0geant/dzero/gsmate.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/gsmate.for | vmstounix > $(d0root)/d0geant/dzero/gsmate.f
$(d0root)/d0geant/dzero/gsmixt.f:\
  $(d0root)/d0geant/dzero/gsmixt.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/gsmixt.for | vmstounix > $(d0root)/d0geant/dzero/gsmixt.f
$(d0root)/d0geant/dzero/gspart.f:\
  $(d0root)/d0geant/dzero/gspart.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/gspart.for | vmstounix > $(d0root)/d0geant/dzero/gspart.f
$(d0root)/d0geant/dzero/gsrotm.f:\
  $(d0root)/d0geant/dzero/gsrotm.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/gsrotm.for | vmstounix > $(d0root)/d0geant/dzero/gsrotm.f
$(d0root)/d0geant/cdc/gtelke.f:\
  $(d0root)/d0geant/cdc/gtelke.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/gtelke.for | vmstounix > $(d0root)/d0geant/cdc/gtelke.f
$(d0root)/d0geant/utl/gtimer.f:\
  $(d0root)/d0geant/utl/gtimer.for\
  $(d0library)/inc/times.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/utl/gtimer.for | vmstounix > $(d0root)/d0geant/utl/gtimer.f
$(d0root)/d0geant/dzero/gtmedi.f:\
  $(d0root)/d0geant/dzero/gtmedi.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/gtmedi.for | vmstounix > $(d0root)/d0geant/dzero/gtmedi.f
$(d0root)/d0geant/dzero/gtnino.f:\
  $(d0root)/d0geant/dzero/gtnino.for\
  $(d0library)/inc/gconst.inc\
  $(d0library)/inc/gcsets.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gctmed.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcmate.inc\
  $(d0library)/inc/gccuts.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/dcalog.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/gtnino.for | vmstounix > $(d0root)/d0geant/dzero/gtnino.f
$(d0root)/d0geant/cdc/gtprim.f:\
  $(d0root)/d0geant/cdc/gtprim.for\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gckine.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/gtprim.for | vmstounix > $(d0root)/d0geant/cdc/gtprim.f
$(d0root)/d0geant/dzero/gtrnsf.f:\
  $(d0root)/d0geant/dzero/gtrnsf.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/gtrnsf.for | vmstounix > $(d0root)/d0geant/dzero/gtrnsf.f
$(d0root)/d0geant/cdc/gtscnd.f:\
  $(d0root)/d0geant/cdc/gtscnd.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/gtscnd.for | vmstounix > $(d0root)/d0geant/cdc/gtscnd.f
$(d0root)/d0geant/dzero/gtswch.f:\
  $(d0root)/d0geant/dzero/gtswch.for\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcmail.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/gtswch.for | vmstounix > $(d0root)/d0geant/dzero/gtswch.f
$(d0root)/d0geant/dzero/gudigi.f:\
  $(d0root)/d0geant/dzero/gudigi.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/gudigi.for | vmstounix > $(d0root)/d0geant/dzero/gudigi.f
$(d0root)/d0geant/muo/gufld.f:\
  $(d0root)/d0geant/muo/gufld.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcvolu.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/gufld.for | vmstounix > $(d0root)/d0geant/muo/gufld.f
$(d0root)/d0geant/muo/gufld_v1.f:\
  $(d0root)/d0geant/muo/gufld_v1.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/gufld_v1.for | vmstounix > $(d0root)/d0geant/muo/gufld_v1.f
$(d0root)/d0geant/dzero/guhadr.f:\
  $(d0root)/d0geant/dzero/guhadr.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/guhadr.for | vmstounix > $(d0root)/d0geant/dzero/guhadr.f
$(d0root)/d0geant/cal/guical.f:\
  $(d0root)/d0geant/cal/guical.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/guical.for | vmstounix > $(d0root)/d0geant/cal/guical.f
$(d0root)/d0geant/cdc/guicdc.f:\
  $(d0root)/d0geant/cdc/guicdc.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/guicdc.for | vmstounix > $(d0root)/d0geant/cdc/guicdc.f
$(d0root)/d0geant/fdc/guifdc.f:\
  $(d0root)/d0geant/fdc/guifdc.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/guifdc.for | vmstounix > $(d0root)/d0geant/fdc/guifdc.f
$(d0root)/d0geant/dzero/guiget.f:\
  $(d0root)/d0geant/dzero/guiget.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/guiget.for | vmstounix > $(d0root)/d0geant/dzero/guiget.f
$(d0root)/d0geant/lv0/guilv0.f:\
  $(d0root)/d0geant/lv0/guilv0.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/lv0/guilv0.for | vmstounix > $(d0root)/d0geant/lv0/guilv0.f
$(d0root)/d0geant/muo/guimuo.f:\
  $(d0root)/d0geant/muo/guimuo.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/guimuo.for | vmstounix > $(d0root)/d0geant/muo/guimuo.f
$(d0root)/d0geant/samus/guisam.f:\
  $(d0root)/d0geant/samus/guisam.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/guisam.for | vmstounix > $(d0root)/d0geant/samus/guisam.f
$(d0root)/d0geant/trd/guitrd.f:\
  $(d0root)/d0geant/trd/guitrd.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/guitrd.for | vmstounix > $(d0root)/d0geant/trd/guitrd.f
$(d0root)/d0geant/vtx/guivtx.f:\
  $(d0root)/d0geant/vtx/guivtx.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/guivtx.for | vmstounix > $(d0root)/d0geant/vtx/guivtx.f
$(d0root)/d0geant/dzero/gukine.f:\
  $(d0root)/d0geant/dzero/gukine.for\
  $(d0library)/inc/gcflag.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/gukine.for | vmstounix > $(d0root)/d0geant/dzero/gukine.f
$(d0root)/d0geant/dzero/guout.f:\
  $(d0root)/d0geant/dzero/guout.for\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gclist.inc\
  $(d0library)/inc/gcnum.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcunit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/guout.for | vmstounix > $(d0root)/d0geant/dzero/guout.f
$(d0root)/d0geant/dzero/guphad.f:\
  $(d0root)/d0geant/dzero/guphad.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/guphad.for | vmstounix > $(d0root)/d0geant/dzero/guphad.f
$(d0root)/d0geant/dzero/gupxyz.f:\
  $(d0root)/d0geant/dzero/gupxyz.for\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcvolu.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gcnum.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gcmate.inc\
  $(d0library)/inc/gcmail.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/gupxyz.for | vmstounix > $(d0root)/d0geant/dzero/gupxyz.f
$(d0root)/d0geant/dzero/gustep.f:\
  $(d0root)/d0geant/dzero/gustep.for\
  $(d0library)/inc/caltrk.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/dcalog.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcking.inc\
  $(d0library)/inc/gctrak.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/gustep.for | vmstounix > $(d0root)/d0geant/dzero/gustep.f
$(d0root)/d0geant/dzero/gutrak.f:\
  $(d0root)/d0geant/dzero/gutrak.for\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/dcalog.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/gutrak.for | vmstounix > $(d0root)/d0geant/dzero/gutrak.f
$(d0root)/d0geant/dzero/gutrev.f:\
  $(d0root)/d0geant/dzero/gutrev.for\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gclist.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebio.inc\
  $(d0library)/inc/zebiog.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/links/izisae.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/gutrev.for | vmstounix > $(d0root)/d0geant/dzero/gutrev.f
$(d0root)/d0geant/dzero/gz_clean.f:\
  $(d0root)/d0geant/dzero/gz_clean.for\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gcnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/gz_clean.for | vmstounix > $(d0root)/d0geant/dzero/gz_clean.f
$(d0root)/d0geant/utl/gzd0rg.f:\
  $(d0root)/d0geant/utl/gzd0rg.for\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/links/izd0rg.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/utl/gzd0rg.for | vmstounix > $(d0root)/d0geant/utl/gzd0rg.f
$(d0root)/d0geant/utl/gzd0rgz.f:\
  $(d0root)/d0geant/utl/gzd0rgz.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izd0rgz.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/utl/gzd0rgz.for | vmstounix > $(d0root)/d0geant/utl/gzd0rgz.f
$(d0root)/d0geant/dzero/gzgswt.f:\
  $(d0root)/d0geant/dzero/gzgswt.for\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/links/izgswt.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/gzgswt.for | vmstounix > $(d0root)/d0geant/dzero/gzgswt.f
$(d0root)/d0geant/dzero/gzgswtz.f:\
  $(d0root)/d0geant/dzero/gzgswtz.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izgswt.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/gzgswtz.for | vmstounix > $(d0root)/d0geant/dzero/gzgswtz.f
$(d0root)/d0geant/cal/hcfill.f:\
  $(d0root)/d0geant/cal/hcfill.for\
  $(d0library)/inc/hcal.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/hcfill.for | vmstounix > $(d0root)/d0geant/cal/hcfill.f
$(d0root)/d0geant/cal/hlpcal.f:\
  $(d0root)/d0geant/cal/hlpcal.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/hlpcal.for | vmstounix > $(d0root)/d0geant/cal/hlpcal.f
$(d0root)/d0geant/cdc/hlpcdc.f:\
  $(d0root)/d0geant/cdc/hlpcdc.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/hlpcdc.for | vmstounix > $(d0root)/d0geant/cdc/hlpcdc.f
$(d0root)/d0geant/d0/hlpd0.f:\
  $(d0root)/d0geant/d0/hlpd0.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/d0/hlpd0.for | vmstounix > $(d0root)/d0geant/d0/hlpd0.f
$(d0root)/d0geant/fdc/hlpfdc.f:\
  $(d0root)/d0geant/fdc/hlpfdc.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/hlpfdc.for | vmstounix > $(d0root)/d0geant/fdc/hlpfdc.f
$(d0root)/d0geant/lv0/hlplv0.f:\
  $(d0root)/d0geant/lv0/hlplv0.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/lv0/hlplv0.for | vmstounix > $(d0root)/d0geant/lv0/hlplv0.f
$(d0root)/d0geant/muo/hlpmuo.f:\
  $(d0root)/d0geant/muo/hlpmuo.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/hlpmuo.for | vmstounix > $(d0root)/d0geant/muo/hlpmuo.f
$(d0root)/d0geant/samus/hlpsam.f:\
  $(d0root)/d0geant/samus/hlpsam.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/hlpsam.for | vmstounix > $(d0root)/d0geant/samus/hlpsam.f
$(d0root)/d0geant/trd/hlptrd.f:\
  $(d0root)/d0geant/trd/hlptrd.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/hlptrd.for | vmstounix > $(d0root)/d0geant/trd/hlptrd.f
$(d0root)/d0geant/vtx/hlpvtx.f:\
  $(d0root)/d0geant/vtx/hlpvtx.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/hlpvtx.for | vmstounix > $(d0root)/d0geant/vtx/hlpvtx.f
$(d0root)/d0geant/cal/inical.f:\
  $(d0root)/d0geant/cal/inical.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gclist.inc\
  $(d0library)/inc/caltrk.inc\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/inical.for | vmstounix > $(d0root)/d0geant/cal/inical.f
$(d0root)/d0geant/cdc/inicdc.f:\
  $(d0root)/d0geant/cdc/inicdc.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/cdcpar.inc\
  $(d0library)/inc/zpulpr.inc\
  $(d0library)/inc/zzerop.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/inicdc.for | vmstounix > $(d0root)/d0geant/cdc/inicdc.f
$(d0root)/d0geant/d0/inid0.f:\
  $(d0root)/d0geant/d0/inid0.for\
  $(d0library)/inc/gcunit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/d0/inid0.for | vmstounix > $(d0root)/d0geant/d0/inid0.f
$(d0root)/d0geant/fdc/inifdc.f:\
  $(d0root)/d0geant/fdc/inifdc.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/zpulpr.inc\
  $(d0library)/inc/zzerop.inc\
  $(d0library)/links/izfgeh.link\
  $(d0library)/links/izfalh.link\
  $(d0library)/links/izftmh.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/inifdc.for | vmstounix > $(d0root)/d0geant/fdc/inifdc.f
$(d0root)/d0geant/lv0/inilv0.f:\
  $(d0root)/d0geant/lv0/inilv0.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/lv0/inilv0.for | vmstounix > $(d0root)/d0geant/lv0/inilv0.f
$(d0root)/d0geant/muo/inimuo.f:\
  $(d0root)/d0geant/muo/inimuo.for\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/inimuo.for | vmstounix > $(d0root)/d0geant/muo/inimuo.f
$(d0root)/d0geant/samus/inisam.f:\
  $(d0root)/d0geant/samus/inisam.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/inisam.for | vmstounix > $(d0root)/d0geant/samus/inisam.f
$(d0root)/d0geant/trd/initab.f:\
  $(d0root)/d0geant/trd/initab.for\
  $(d0library)/inc/absor.inc\
  $(d0library)/inc/tecdsc.inc\
  $(d0library)/inc/raddcc.inc\
  $(d0library)/inc/tecdcc.inc\
  $(d0library)/inc/windcc.inc\
  $(d0library)/inc/xray.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/initab.for | vmstounix > $(d0root)/d0geant/trd/initab.f
$(d0root)/d0geant/trd/initrd.f:\
  $(d0root)/d0geant/trd/initrd.for\
  $(d0library)/inc/disdel.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/fadccn.inc\
  $(d0library)/inc/faddig.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/geomtr.inc\
  $(d0library)/inc/nortrd.inc\
  $(d0library)/inc/tabx.inc\
  $(d0library)/inc/trinfo.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/initrd.for | vmstounix > $(d0root)/d0geant/trd/initrd.f
$(d0root)/d0geant/vtx/inivtx.f:\
  $(d0root)/d0geant/vtx/inivtx.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zpulpr.inc\
  $(d0library)/inc/zzerop.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/inivtx.for | vmstounix > $(d0root)/d0geant/vtx/inivtx.f
$(d0root)/d0geant/cal/intcal.f:\
  $(d0root)/d0geant/cal/intcal.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/d0com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/intcal.for | vmstounix > $(d0root)/d0geant/cal/intcal.f
$(d0root)/d0geant/cdc/intcdc.f:\
  $(d0root)/d0geant/cdc/intcdc.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/d0com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/intcdc.for | vmstounix > $(d0root)/d0geant/cdc/intcdc.f
$(d0root)/d0geant/d0/intd0.f:\
  $(d0root)/d0geant/d0/intd0.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/d0/intd0.for | vmstounix > $(d0root)/d0geant/d0/intd0.f
$(d0root)/d0geant/trd/interp.f:\
  $(d0root)/d0geant/trd/interp.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/interp.for | vmstounix > $(d0root)/d0geant/trd/interp.f
$(d0root)/d0geant/fdc/intfdc.f:\
  $(d0root)/d0geant/fdc/intfdc.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/d0com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/intfdc.for | vmstounix > $(d0root)/d0geant/fdc/intfdc.f
$(d0root)/d0geant/lv0/intlv0.f:\
  $(d0root)/d0geant/lv0/intlv0.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/d0com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/lv0/intlv0.for | vmstounix > $(d0root)/d0geant/lv0/intlv0.f
$(d0root)/d0geant/muo/intmuo.f:\
  $(d0root)/d0geant/muo/intmuo.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/d0com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/intmuo.for | vmstounix > $(d0root)/d0geant/muo/intmuo.f
$(d0root)/d0geant/samus/intsam.f:\
  $(d0root)/d0geant/samus/intsam.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/intsam.for | vmstounix > $(d0root)/d0geant/samus/intsam.f
$(d0root)/d0geant/trd/inttrd.f:\
  $(d0root)/d0geant/trd/inttrd.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/d0com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/inttrd.for | vmstounix > $(d0root)/d0geant/trd/inttrd.f
$(d0root)/d0geant/vtx/intvtx.f:\
  $(d0root)/d0geant/vtx/intvtx.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/d0com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/intvtx.for | vmstounix > $(d0root)/d0geant/vtx/intvtx.f
$(d0root)/d0geant/zebio/isagea.f:\
  $(d0root)/d0geant/zebio/isagea.for\
  $(d0library)/inc/gcunit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/isagea.for | vmstounix > $(d0root)/d0geant/zebio/isagea.f
$(d0root)/d0geant/zebio/isajet_geant_track.f:\
  $(d0root)/d0geant/zebio/isajet_geant_track.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebio.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/isajet_geant_track.for | vmstounix > $(d0root)/d0geant/zebio/isajet_geant_track.f
$(d0root)/d0geant/zebio/isajet_geant_vtx.f:\
  $(d0root)/d0geant/zebio/isajet_geant_vtx.for\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/isajet_geant_vtx.for | vmstounix > $(d0root)/d0geant/zebio/isajet_geant_vtx.f
$(d0root)/d0geant/zebio/isdrop.f:\
  $(d0root)/d0geant/zebio/isdrop.for\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/isdrop.for | vmstounix > $(d0root)/d0geant/zebio/isdrop.f
$(d0root)/d0geant/zebio/iskine.f:\
  $(d0root)/d0geant/zebio/iskine.for\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcnum.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebio.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/links/izisae.link\
  $(d0library)/links/izisv1.link\
  $(d0library)/links/izisp1.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/iskine.for | vmstounix > $(d0root)/d0geant/zebio/iskine.f
$(d0root)/d0geant/zebio/isout.f:\
  $(d0root)/d0geant/zebio/isout.for\
  $(d0library)/inc/gcnum.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebio.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/links/izisae.link\
  $(d0library)/links/izisv1.link\
  $(d0library)/links/izisp1.link\
  $(d0library)/links/izisv2.link\
  $(d0library)/links/izisp2.link\
  $(d0library)/links/izisp3.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/isout.for | vmstounix > $(d0root)/d0geant/zebio/isout.f
$(d0root)/d0geant/cal/isp1_link.f:\
  $(d0root)/d0geant/cal/isp1_link.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izisae.link\
  $(d0library)/links/izisv1.link\
  $(d0library)/links/izisp1.link\
  $(d0library)/inc/gcnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/isp1_link.for | vmstounix > $(d0root)/d0geant/cal/isp1_link.f
$(d0root)/d0geant/zebio/israw.f:\
  $(d0root)/d0geant/zebio/israw.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/israw.for | vmstounix > $(d0root)/d0geant/zebio/israw.f
$(d0root)/d0geant/zebio/isread.f:\
  $(d0root)/d0geant/zebio/isread.for\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gcnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/isread.for | vmstounix > $(d0root)/d0geant/zebio/isread.f
$(d0root)/d0geant/zebio/isref.f:\
  $(d0root)/d0geant/zebio/isref.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izisae.link\
  $(d0library)/links/izisv1.link\
  $(d0library)/links/izisv2.link\
  $(d0library)/links/izisp1.link\
  $(d0library)/links/izisp2.link\
  $(d0library)/links/izisp3.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/isref.for | vmstounix > $(d0root)/d0geant/zebio/isref.f
$(d0root)/d0geant/zebio/isubfn.f:\
  $(d0root)/d0geant/zebio/isubfn.for\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gcunit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/isubfn.for | vmstounix > $(d0root)/d0geant/zebio/isubfn.f
$(d0root)/d0geant/zebio/iszeb.f:\
  $(d0root)/d0geant/zebio/iszeb.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebio.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/iszeb.for | vmstounix > $(d0root)/d0geant/zebio/iszeb.f
$(d0root)/d0geant/cal/jetend.f:\
  $(d0root)/d0geant/cal/jetend.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/jetend.for | vmstounix > $(d0root)/d0geant/cal/jetend.f
$(d0root)/d0geant/cal/jetfil.f:\
  $(d0root)/d0geant/cal/jetfil.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/spevnt.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/jetfil.for | vmstounix > $(d0root)/d0geant/cal/jetfil.f
$(d0root)/d0geant/cal/jetini.f:\
  $(d0root)/d0geant/cal/jetini.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izisae.link\
  $(d0library)/links/izisaq.link\
  $(d0library)/inc/spevnt.inc\
  $(d0library)/inc/gcunit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/jetini.for | vmstounix > $(d0root)/d0geant/cal/jetini.f
$(d0root)/d0geant/d0/kind0.f:\
  $(d0root)/d0geant/d0/kind0.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gconst.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gclist.inc\
  $(d0library)/inc/zebio.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/d0/kind0.for | vmstounix > $(d0root)/d0geant/d0/kind0.f
$(d0root)/d0geant/lv0/l0_adc_expect.f:\
  $(d0root)/d0geant/lv0/l0_adc_expect.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/lv0param.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/lv0/l0_adc_expect.for | vmstounix > $(d0root)/d0geant/lv0/l0_adc_expect.f
$(d0root)/d0geant/lv0/l0_pack_adc.f:\
  $(d0root)/d0geant/lv0/l0_pack_adc.for\
  $(d0library)/inc/lv0param.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/lv0/l0_pack_adc.for | vmstounix > $(d0root)/d0geant/lv0/l0_pack_adc.f
$(d0root)/d0geant/lv0/l0_pack_header.f:\
  $(d0root)/d0geant/lv0/l0_pack_header.for\
  $(d0library)/inc/lv0param.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/lv0/l0_pack_header.for | vmstounix > $(d0root)/d0geant/lv0/l0_pack_header.f
$(d0root)/d0geant/lv0/l0_pack_trailer.f:\
  $(d0root)/d0geant/lv0/l0_pack_trailer.for\
  $(d0library)/inc/lv0param.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/lv0/l0_pack_trailer.for | vmstounix > $(d0root)/d0geant/lv0/l0_pack_trailer.f
$(d0root)/d0geant/lv0/l0_pack_vertex.f:\
  $(d0root)/d0geant/lv0/l0_pack_vertex.for\
  $(d0library)/inc/lv0param.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/lv0/l0_pack_vertex.for | vmstounix > $(d0root)/d0geant/lv0/l0_pack_vertex.f
$(d0root)/d0geant/lv0/l0_run1a_setup.f:\
  $(d0root)/d0geant/lv0/l0_run1a_setup.for\
  $(d0library)/inc/lv0param.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/lv0/l0_run1a_setup.for | vmstounix > $(d0root)/d0geant/lv0/l0_run1a_setup.f
$(d0root)/d0geant/lv0/l0_run1b_setup.f:\
  $(d0root)/d0geant/lv0/l0_run1b_setup.for\
  $(d0library)/inc/lv0param.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/lv0/l0_run1b_setup.for | vmstounix > $(d0root)/d0geant/lv0/l0_run1b_setup.f
$(d0root)/d0geant/fdc/lastfdcstp.f:\
  $(d0root)/d0geant/fdc/lastfdcstp.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izfgeh.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/lastfdcstp.for | vmstounix > $(d0root)/d0geant/fdc/lastfdcstp.f
$(d0root)/d0geant/trd/lasttrdstp.f:\
  $(d0root)/d0geant/trd/lasttrdstp.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/iztach.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/lasttrdstp.for | vmstounix > $(d0root)/d0geant/trd/lasttrdstp.f
$(d0root)/d0geant/dzero/lddum.f:\
  $(d0root)/d0geant/dzero/lddum.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/lddum.for | vmstounix > $(d0root)/d0geant/dzero/lddum.f
$(d0root)/d0geant/fdc/ldfpda.f:\
  $(d0root)/d0geant/fdc/ldfpda.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/fdloca.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/ldfpda.for | vmstounix > $(d0root)/d0geant/fdc/ldfpda.f
$(d0root)/d0geant/fdc/ldfpsc.f:\
  $(d0root)/d0geant/fdc/ldfpsc.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/fdloca.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/ldfpsc.for | vmstounix > $(d0root)/d0geant/fdc/ldfpsc.f
$(d0root)/d0geant/fdc/ldftda.f:\
  $(d0root)/d0geant/fdc/ldftda.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/fdloca.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/ldftda.for | vmstounix > $(d0root)/d0geant/fdc/ldftda.f
$(d0root)/d0geant/fdc/ldftsc.f:\
  $(d0root)/d0geant/fdc/ldftsc.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/fdloca.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/ldftsc.for | vmstounix > $(d0root)/d0geant/fdc/ldftsc.f
$(d0root)/d0geant/vtx/ldvsec.f:\
  $(d0root)/d0geant/vtx/ldvsec.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/vtxlnk.inc\
  $(d0library)/inc/vtloca.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/ldvsec.for | vmstounix > $(d0root)/d0geant/vtx/ldvsec.f
$(d0root)/d0geant/vtx/ldvwda.f:\
  $(d0root)/d0geant/vtx/ldvwda.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/vtxlnk.inc\
  $(d0library)/inc/vtloca.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/ldvwda.for | vmstounix > $(d0root)/d0geant/vtx/ldvwda.f
$(d0root)/d0geant/vtx/ldvzda.f:\
  $(d0root)/d0geant/vtx/ldvzda.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/vtxlnk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/ldvzda.for | vmstounix > $(d0root)/d0geant/vtx/ldvzda.f
$(d0root)/d0geant/vtx/ldvzla.f:\
  $(d0root)/d0geant/vtx/ldvzla.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/vtxlnk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/ldvzla.for | vmstounix > $(d0root)/d0geant/vtx/ldvzla.f
$(d0root)/d0geant/utl/locgeo.f:\
  $(d0root)/d0geant/utl/locgeo.for\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gcvolu.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/utl/locgeo.for | vmstounix > $(d0root)/d0geant/utl/locgeo.f
$(d0root)/d0geant/cal/lstcal.f:\
  $(d0root)/d0geant/cal/lstcal.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/lstcal.for | vmstounix > $(d0root)/d0geant/cal/lstcal.f
$(d0root)/d0geant/cdc/lstcdc.f:\
  $(d0root)/d0geant/cdc/lstcdc.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/lstcdc.for | vmstounix > $(d0root)/d0geant/cdc/lstcdc.f
$(d0root)/d0geant/fdc/lstfdc.f:\
  $(d0root)/d0geant/fdc/lstfdc.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/lstfdc.for | vmstounix > $(d0root)/d0geant/fdc/lstfdc.f
$(d0root)/d0geant/lv0/lstlv0.f:\
  $(d0root)/d0geant/lv0/lstlv0.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/lv0/lstlv0.for | vmstounix > $(d0root)/d0geant/lv0/lstlv0.f
$(d0root)/d0geant/muo/lstmuo.f:\
  $(d0root)/d0geant/muo/lstmuo.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/lstmuo.for | vmstounix > $(d0root)/d0geant/muo/lstmuo.f
$(d0root)/d0geant/samus/lstsam.f:\
  $(d0root)/d0geant/samus/lstsam.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/lstsam.for | vmstounix > $(d0root)/d0geant/samus/lstsam.f
$(d0root)/d0geant/trd/lsttrd.f:\
  $(d0root)/d0geant/trd/lsttrd.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/lsttrd.for | vmstounix > $(d0root)/d0geant/trd/lsttrd.f
$(d0root)/d0geant/vtx/lstvtx.f:\
  $(d0root)/d0geant/vtx/lstvtx.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/lstvtx.for | vmstounix > $(d0root)/d0geant/vtx/lstvtx.f
$(d0root)/d0geant/dzero/lutra1.f:\
  $(d0root)/d0geant/dzero/lutra1.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/lutra1.for | vmstounix > $(d0root)/d0geant/dzero/lutra1.f
$(d0root)/d0geant/muo/mcalexit.f:\
  $(d0root)/d0geant/muo/mcalexit.for\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gckine.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/mcalexit.for | vmstounix > $(d0root)/d0geant/muo/mcalexit.f
$(d0root)/d0geant/cal/mencal.f:\
  $(d0root)/d0geant/cal/mencal.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/dcalog.inc\
  $(d0library)/inc/d0com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/mencal.for | vmstounix > $(d0root)/d0geant/cal/mencal.f
$(d0root)/d0geant/cdc/mencdc.f:\
  $(d0root)/d0geant/cdc/mencdc.for\
  $(d0library)/inc/d0com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/mencdc.for | vmstounix > $(d0root)/d0geant/cdc/mencdc.f
$(d0root)/d0geant/d0/mend0.f:\
  $(d0root)/d0geant/d0/mend0.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/d0/mend0.for | vmstounix > $(d0root)/d0geant/d0/mend0.f
$(d0root)/d0geant/fdc/menfdc.f:\
  $(d0root)/d0geant/fdc/menfdc.for\
  $(d0library)/inc/d0com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/menfdc.for | vmstounix > $(d0root)/d0geant/fdc/menfdc.f
$(d0root)/d0geant/lv0/menlv0.f:\
  $(d0root)/d0geant/lv0/menlv0.for\
  $(d0library)/inc/d0com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/lv0/menlv0.for | vmstounix > $(d0root)/d0geant/lv0/menlv0.f
$(d0root)/d0geant/muo/menmuo.f:\
  $(d0root)/d0geant/muo/menmuo.for\
  $(d0library)/inc/d0com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/menmuo.for | vmstounix > $(d0root)/d0geant/muo/menmuo.f
$(d0root)/d0geant/samus/mensam.f:\
  $(d0root)/d0geant/samus/mensam.for\
  $(d0library)/inc/d0com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/mensam.for | vmstounix > $(d0root)/d0geant/samus/mensam.f
$(d0root)/d0geant/trd/mentrd.f:\
  $(d0root)/d0geant/trd/mentrd.for\
  $(d0library)/inc/d0com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/mentrd.for | vmstounix > $(d0root)/d0geant/trd/mentrd.f
$(d0root)/d0geant/vtx/menvtx.f:\
  $(d0root)/d0geant/vtx/menvtx.for\
  $(d0library)/inc/d0com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/menvtx.for | vmstounix > $(d0root)/d0geant/vtx/menvtx.f
$(d0root)/d0geant/cdc/mkdcdc.f:\
  $(d0root)/d0geant/cdc/mkdcdc.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdrft.link\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/cdcpar.inc\
  $(d0library)/inc/cdclnk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/mkdcdc.for | vmstounix > $(d0root)/d0geant/cdc/mkdcdc.f
$(d0root)/d0geant/fdc/mkfphi.f:\
  $(d0root)/d0geant/fdc/mkfphi.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/fdloca.inc\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/mkfphi.for | vmstounix > $(d0root)/d0geant/fdc/mkfphi.f
$(d0root)/d0geant/fdc/mkfthe.f:\
  $(d0root)/d0geant/fdc/mkfthe.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/fdloca.inc\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/mkfthe.for | vmstounix > $(d0root)/d0geant/fdc/mkfthe.f
$(d0root)/d0geant/vtx/mkvtxs.f:\
  $(d0root)/d0geant/vtx/mkvtxs.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/vtloca.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/mkvtxs.for | vmstounix > $(d0root)/d0geant/vtx/mkvtxs.f
$(d0root)/d0geant/vtx/mkvtxw.f:\
  $(d0root)/d0geant/vtx/mkvtxw.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/vtxlnk.inc\
  $(d0library)/inc/vtloca.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/mkvtxw.for | vmstounix > $(d0root)/d0geant/vtx/mkvtxw.f
$(d0root)/d0geant/muo/mscrat.f:\
  $(d0root)/d0geant/muo/mscrat.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/mscrat.for | vmstounix > $(d0root)/d0geant/muo/mscrat.f
$(d0root)/d0geant/muo/msdelt.f:\
  $(d0root)/d0geant/muo/msdelt.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/msdelt.for | vmstounix > $(d0root)/d0geant/muo/msdelt.f
$(d0root)/d0geant/muo/msdrft.f:\
  $(d0root)/d0geant/muo/msdrft.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/msdrft.for | vmstounix > $(d0root)/d0geant/muo/msdrft.f
$(d0root)/d0geant/muo/msgnum.f:\
  $(d0root)/d0geant/muo/msgnum.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/msgnum.for | vmstounix > $(d0root)/d0geant/muo/msgnum.f
$(d0root)/d0geant/muo/mshits.f:\
  $(d0root)/d0geant/muo/mshits.for\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gcvolu.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/mshits.for | vmstounix > $(d0root)/d0geant/muo/mshits.f
$(d0root)/d0geant/muo/msindx.f:\
  $(d0root)/d0geant/muo/msindx.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/msindx.for | vmstounix > $(d0root)/d0geant/muo/msindx.f
$(d0root)/d0geant/muo/msindx_v1.f:\
  $(d0root)/d0geant/muo/msindx_v1.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/msindx_v1.for | vmstounix > $(d0root)/d0geant/muo/msindx_v1.f
$(d0root)/d0geant/muo/msmud1.f:\
  $(d0root)/d0geant/muo/msmud1.for\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/mu_zebstp.inc\
  $(d0library)/links/izmud1.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/msmud1.for | vmstounix > $(d0root)/d0geant/muo/msmud1.f
$(d0root)/d0geant/muo/msmud1_v1.f:\
  $(d0root)/d0geant/muo/msmud1_v1.for\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izmud1.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/msmud1_v1.for | vmstounix > $(d0root)/d0geant/muo/msmud1_v1.f
$(d0root)/d0geant/muo/msmudh.f:\
  $(d0root)/d0geant/muo/msmudh.for\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izmud1.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/msmudh.for | vmstounix > $(d0root)/d0geant/muo/msmudh.f
$(d0root)/d0geant/muo/mspad.f:\
  $(d0root)/d0geant/muo/mspad.for\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/links/izstpo.link\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izsmuo.link\
  $(d0library)/links/izmgeh.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/mspad.for | vmstounix > $(d0root)/d0geant/muo/mspad.f
$(d0root)/d0geant/muo/mspad_v1.f:\
  $(d0root)/d0geant/muo/mspad_v1.for\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/links/izstpo.link\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izsmuo.link\
  $(d0library)/links/izmgeh.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/mspad_v1.for | vmstounix > $(d0root)/d0geant/muo/mspad_v1.f
$(d0root)/d0geant/muo/mspadr.f:\
  $(d0root)/d0geant/muo/mspadr.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/mspadr.for | vmstounix > $(d0root)/d0geant/muo/mspadr.f
$(d0root)/d0geant/muo/mspads.f:\
  $(d0root)/d0geant/muo/mspads.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/mspads.for | vmstounix > $(d0root)/d0geant/muo/mspads.f
$(d0root)/d0geant/muo/mstrak.f:\
  $(d0root)/d0geant/muo/mstrak.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcsets.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gcvolu.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/mstrak.for | vmstounix > $(d0root)/d0geant/muo/mstrak.f
$(d0root)/d0geant/muo/mstvc.f:\
  $(d0root)/d0geant/muo/mstvc.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/mstvc.for | vmstounix > $(d0root)/d0geant/muo/mstvc.f
$(d0root)/d0geant/muo/mstvc_v1.f:\
  $(d0root)/d0geant/muo/mstvc_v1.for\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/mstvc_v1.for | vmstounix > $(d0root)/d0geant/muo/mstvc_v1.f
$(d0root)/d0geant/muo/msvchk.f:\
  $(d0root)/d0geant/muo/msvchk.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izstpo.link\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izsmuo.link\
  $(d0library)/links/izmgeh.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/msvchk.for | vmstounix > $(d0root)/d0geant/muo/msvchk.f
$(d0root)/d0geant/muo/mudrft.f:\
  $(d0root)/d0geant/muo/mudrft.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/mudrft.for | vmstounix > $(d0root)/d0geant/muo/mudrft.f
$(d0root)/d0geant/cal/mxcal.f:\
  $(d0root)/d0geant/cal/mxcal.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/dcalog.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/mxcal.for | vmstounix > $(d0root)/d0geant/cal/mxcal.f
$(d0root)/d0geant/lv0/mxlv0.f:\
  $(d0root)/d0geant/lv0/mxlv0.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/lv0/mxlv0.for | vmstounix > $(d0root)/d0geant/lv0/mxlv0.f
$(d0root)/d0geant/utl/norran.f:\
  $(d0root)/d0geant/utl/norran.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/utl/norran.for | vmstounix > $(d0root)/d0geant/utl/norran.f
$(d0root)/d0geant/dzero/nran.f:\
  $(d0root)/d0geant/dzero/nran.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/nran.for | vmstounix > $(d0root)/d0geant/dzero/nran.f
$(d0root)/d0geant/vtx/nxtzst.f:\
  $(d0root)/d0geant/vtx/nxtzst.for\
  $(d0library)/inc/vzdata.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/nxtzst.for | vmstounix > $(d0root)/d0geant/vtx/nxtzst.f
$(d0root)/d0geant/dzero/pcolor.f:\
  $(d0root)/d0geant/dzero/pcolor.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/pcolor.for | vmstounix > $(d0root)/d0geant/dzero/pcolor.f
$(d0root)/d0geant/dzero/prgswt.f:\
  $(d0root)/d0geant/dzero/prgswt.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/prgswt.for | vmstounix > $(d0root)/d0geant/dzero/prgswt.f
$(d0root)/d0geant/dzero/print_step.f:\
  $(d0root)/d0geant/dzero/print_step.for\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcking.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gctmed.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gcvolu.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/print_step.for | vmstounix > $(d0root)/d0geant/dzero/print_step.f
$(d0root)/d0geant/muo/prmmap.f:\
  $(d0root)/d0geant/muo/prmmap.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izmmap.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/prmmap.for | vmstounix > $(d0root)/d0geant/muo/prmmap.f
$(d0root)/d0geant/dzero/prune_gskine.f:\
  $(d0root)/d0geant/dzero/prune_gskine.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/prune_gskine.for | vmstounix > $(d0root)/d0geant/dzero/prune_gskine.f
$(d0root)/d0geant/zebio/rdzeb.f:\
  $(d0root)/d0geant/zebio/rdzeb.for\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebio.inc\
  $(d0library)/inc/zebiog.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gclist.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/links/izgean.link\
  $(d0library)/inc/mioflg.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/rdzeb.for | vmstounix > $(d0root)/d0geant/zebio/rdzeb.f
$(d0root)/d0geant/zebio/rotino.f:\
  $(d0root)/d0geant/zebio/rotino.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/rotino.for | vmstounix > $(d0root)/d0geant/zebio/rotino.f
$(d0root)/d0geant/cal/rrlam.f:\
  $(d0root)/d0geant/cal/rrlam.for\
  $(d0library)/inc/gconst.inc\
  $(d0library)/inc/absln.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/rrlam.for | vmstounix > $(d0root)/d0geant/cal/rrlam.f
$(d0root)/d0geant/trd/rwxspc.f:\
  $(d0root)/d0geant/trd/rwxspc.for\
  $(d0library)/inc/gconst.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/raddsc.inc\
  $(d0library)/inc/tabx.inc\
  $(d0library)/inc/xray.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/rwxspc.for | vmstounix > $(d0root)/d0geant/trd/rwxspc.f
$(d0root)/d0geant/samus/safill.f:\
  $(d0root)/d0geant/samus/safill.for\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/mu_zebstp.inc\
  $(d0library)/links/izmud1.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/safill.for | vmstounix > $(d0root)/d0geant/samus/safill.f
$(d0root)/d0geant/samus/saicmp.f:\
  $(d0root)/d0geant/samus/saicmp.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/saicmp.for | vmstounix > $(d0root)/d0geant/samus/saicmp.f
$(d0root)/d0geant/samus/sam_bp.f:\
  $(d0root)/d0geant/samus/sam_bp.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/srcpr.inc\
  $(d0library)/links/izsbpg.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/sam_bp.for | vmstounix > $(d0root)/d0geant/samus/sam_bp.f
$(d0root)/d0geant/samus/sam_ma.f:\
  $(d0root)/d0geant/samus/sam_ma.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/srcpr.inc\
  $(d0library)/links/izsmag.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/sam_ma.for | vmstounix > $(d0root)/d0geant/samus/sam_ma.f
$(d0root)/d0geant/samus/samadc.f:\
  $(d0root)/d0geant/samus/samadc.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/samadc.for | vmstounix > $(d0root)/d0geant/samus/samadc.f
$(d0root)/d0geant/samus/samgt1.f:\
  $(d0root)/d0geant/samus/samgt1.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/samgt1.for | vmstounix > $(d0root)/d0geant/samus/samgt1.f
$(d0root)/d0geant/samus/samgt2.f:\
  $(d0root)/d0geant/samus/samgt2.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izgsat.link
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/samgt2.for | vmstounix > $(d0root)/d0geant/samus/samgt2.f
$(d0root)/d0geant/samus/saname.f:\
  $(d0root)/d0geant/samus/saname.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/saname.for | vmstounix > $(d0root)/d0geant/samus/saname.f
$(d0root)/d0geant/samus/saslb1.f:\
  $(d0root)/d0geant/samus/saslb1.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/saslb1.for | vmstounix > $(d0root)/d0geant/samus/saslb1.f
$(d0root)/d0geant/samus/saslb2.f:\
  $(d0root)/d0geant/samus/saslb2.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/saslb2.for | vmstounix > $(d0root)/d0geant/samus/saslb2.f
$(d0root)/d0geant/samus/sastat.f:\
  $(d0root)/d0geant/samus/sastat.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/srcpr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/sastat.for | vmstounix > $(d0root)/d0geant/samus/sastat.f
$(d0root)/d0geant/samus/satube.f:\
  $(d0root)/d0geant/samus/satube.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/satube.for | vmstounix > $(d0root)/d0geant/samus/satube.f
$(d0root)/d0geant/cal/set_electron_pars.f:\
  $(d0root)/d0geant/cal/set_electron_pars.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/dcalog.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/set_electron_pars.for | vmstounix > $(d0root)/d0geant/cal/set_electron_pars.f
$(d0root)/d0geant/cal/set_shower_param.f:\
  $(d0root)/d0geant/cal/set_shower_param.for\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/dcalog.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/set_shower_param.for | vmstounix > $(d0root)/d0geant/cal/set_shower_param.f
$(d0root)/d0geant/utl/setdet.f:\
  $(d0root)/d0geant/utl/setdet.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/utl/setdet.for | vmstounix > $(d0root)/d0geant/utl/setdet.f
$(d0root)/d0geant/utl/setrot.f:\
  $(d0root)/d0geant/utl/setrot.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/utl/setrot.for | vmstounix > $(d0root)/d0geant/utl/setrot.f
$(d0root)/d0geant/utl/setvew.f:\
  $(d0root)/d0geant/utl/setvew.for\
  $(d0library)/inc/srcpr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/utl/setvew.for | vmstounix > $(d0root)/d0geant/utl/setvew.f
$(d0root)/d0geant/caltower/shcopy.f:\
  $(d0root)/d0geant/caltower/shcopy.for\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/gcsets.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcking.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebshl.inc\
  $(d0library)/links/izshhd.link\
  $(d0library)/links/izshlb.link\
  $(d0library)/inc/shlcon.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/caltower/shcopy.for | vmstounix > $(d0root)/d0geant/caltower/shcopy.f
$(d0root)/d0geant/caltower/shinit.f:\
  $(d0root)/d0geant/caltower/shinit.for\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebshl.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/shlcon.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/caltower/shinit.for | vmstounix > $(d0root)/d0geant/caltower/shinit.f
$(d0root)/d0geant/caltower/shlast.f:\
  $(d0root)/d0geant/caltower/shlast.for\
  $(d0library)/inc/shlcon.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/caltower/shlast.for | vmstounix > $(d0root)/d0geant/caltower/shlast.f
$(d0root)/d0geant/zebio/shlb_cal_xyz.f:\
  $(d0root)/d0geant/zebio/shlb_cal_xyz.for\
  $(d0library)/inc/shldat.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/shlb_cal_xyz.for | vmstounix > $(d0root)/d0geant/zebio/shlb_cal_xyz.f
$(d0root)/d0geant/cal/shlb_elec_match_key.f:\
  $(d0root)/d0geant/cal/shlb_elec_match_key.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/shlcon.inc\
  $(d0library)/inc/shldat.inc\
  $(d0library)/inc/shsort.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zlinkc.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/shlitr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/shlb_elec_match_key.for | vmstounix > $(d0root)/d0geant/cal/shlb_elec_match_key.f
$(d0root)/d0geant/zebio/shlb_move_track.f:\
  $(d0root)/d0geant/zebio/shlb_move_track.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/shldat.inc\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/shlb_move_track.for | vmstounix > $(d0root)/d0geant/zebio/shlb_move_track.f
$(d0root)/d0geant/cdc/simpul.f:\
  $(d0root)/d0geant/cdc/simpul.for\
  $(d0library)/inc/zpulpr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/simpul.for | vmstounix > $(d0root)/d0geant/cdc/simpul.f
$(d0root)/d0geant/utl/slopan.f:\
  $(d0root)/d0geant/utl/slopan.for\
  $(d0library)/inc/gconst.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/utl/slopan.for | vmstounix > $(d0root)/d0geant/utl/slopan.f
$(d0root)/d0geant/caltower/smrfac.f:\
  $(d0root)/d0geant/caltower/smrfac.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/caltower/smrfac.for | vmstounix > $(d0root)/d0geant/caltower/smrfac.f
$(d0root)/d0geant/dzero/snapshot.f:\
  $(d0root)/d0geant/dzero/snapshot.for\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcking.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gcnum.inc\
  $(d0library)/inc/gcstak.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gcunit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/snapshot.for | vmstounix > $(d0root)/d0geant/dzero/snapshot.f
$(d0root)/d0geant/trd/splgar.f:\
  $(d0root)/d0geant/trd/splgar.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/xray.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/splgar.for | vmstounix > $(d0root)/d0geant/trd/splgar.f
$(d0root)/d0geant/lv0/stolv0.f:\
  $(d0root)/d0geant/lv0/stolv0.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/lv0/stolv0.for | vmstounix > $(d0root)/d0geant/lv0/stolv0.f
$(d0root)/d0geant/dzero/store_mcen.f:\
  $(d0root)/d0geant/dzero/store_mcen.for\
  $(d0library)/inc/cenmv.inc\
  $(d0library)/inc/dcalog.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcking.inc\
  $(d0library)/inc/gctrak.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/store_mcen.for | vmstounix > $(d0root)/d0geant/dzero/store_mcen.f
$(d0root)/d0geant/cal/stpcal.f:\
  $(d0root)/d0geant/cal/stpcal.for\
  $(d0library)/inc/gcsets.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcking.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/dcalog.inc\
  $(d0library)/inc/gcvolu.inc\
  $(d0library)/inc/caltrk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/stpcal.for | vmstounix > $(d0root)/d0geant/cal/stpcal.f
$(d0root)/d0geant/cdc/stpcdc.f:\
  $(d0root)/d0geant/cdc/stpcdc.for\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gcsets.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/stpcdc.for | vmstounix > $(d0root)/d0geant/cdc/stpcdc.f
$(d0root)/d0geant/d0/stpd0.f:\
  $(d0root)/d0geant/d0/stpd0.for\
  $(d0library)/inc/cenmv.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcking.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gcvolu.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/d0/stpd0.for | vmstounix > $(d0root)/d0geant/d0/stpd0.f
$(d0root)/d0geant/fdc/stpfdc.f:\
  $(d0root)/d0geant/fdc/stpfdc.for\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gcsets.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/stpfdc.for | vmstounix > $(d0root)/d0geant/fdc/stpfdc.f
$(d0root)/d0geant/lv0/stplv0.f:\
  $(d0root)/d0geant/lv0/stplv0.for\
  $(d0library)/inc/lv0param.inc\
  $(d0library)/inc/gcsets.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gconst.inc\
  $(d0library)/inc/gcking.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcvolu.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/lv0/stplv0.for | vmstounix > $(d0root)/d0geant/lv0/stplv0.f
$(d0root)/d0geant/muo/stpmuo.f:\
  $(d0root)/d0geant/muo/stpmuo.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcsets.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/stpmuo.for | vmstounix > $(d0root)/d0geant/muo/stpmuo.f
$(d0root)/d0geant/muo/stpmuo_v1.f:\
  $(d0root)/d0geant/muo/stpmuo_v1.for\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcsets.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/stpmuo_v1.for | vmstounix > $(d0root)/d0geant/muo/stpmuo_v1.f
$(d0root)/d0geant/samus/stpsam.f:\
  $(d0root)/d0geant/samus/stpsam.for\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gcsets.inc\
  $(d0library)/inc/gcvolu.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/stpsam.for | vmstounix > $(d0root)/d0geant/samus/stpsam.f
$(d0root)/d0geant/trd/stptrd.f:\
  $(d0root)/d0geant/trd/stptrd.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/enetrd.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/posit.inc\
  $(d0library)/inc/prnevt.inc\
  $(d0library)/inc/trinfo.inc\
  $(d0library)/inc/trdvol.inc\
  $(d0library)/inc/geomtr.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izisae.link\
  $(d0library)/links/izisv1.link\
  $(d0library)/links/izisp1.link\
  $(d0library)/inc/gcsets.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/stptrd.for | vmstounix > $(d0root)/d0geant/trd/stptrd.f
$(d0root)/d0geant/vtx/stpvtx.f:\
  $(d0root)/d0geant/vtx/stpvtx.for\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gcsets.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/stpvtx.for | vmstounix > $(d0root)/d0geant/vtx/stpvtx.f
$(d0root)/d0geant/dzero/stswch.f:\
  $(d0root)/d0geant/dzero/stswch.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/stswch.for | vmstounix > $(d0root)/d0geant/dzero/stswch.f
$(d0root)/d0geant/utl/stzdib.f:\
  $(d0root)/d0geant/utl/stzdib.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/utl/stzdib.for | vmstounix > $(d0root)/d0geant/utl/stzdib.f
$(d0root)/d0geant/utl/stzdiv.f:\
  $(d0root)/d0geant/utl/stzdiv.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/utl/stzdiv.for | vmstounix > $(d0root)/d0geant/utl/stzdiv.f
$(d0root)/d0geant/utl/stzdv1.f:\
  $(d0root)/d0geant/utl/stzdv1.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/utl/stzdv1.for | vmstounix > $(d0root)/d0geant/utl/stzdv1.f
$(d0root)/d0geant/dzero/super_gswt.f:\
  $(d0root)/d0geant/dzero/super_gswt.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/super_gswt.for | vmstounix > $(d0root)/d0geant/dzero/super_gswt.f
$(d0root)/d0geant/fdc/svdlay.f:\
  $(d0root)/d0geant/fdc/svdlay.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/svdlay.for | vmstounix > $(d0root)/d0geant/fdc/svdlay.f
$(d0root)/d0geant/cal/tevcal.f:\
  $(d0root)/d0geant/cal/tevcal.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/dcalog.inc\
  $(d0library)/inc/hcal.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/tevcal.for | vmstounix > $(d0root)/d0geant/cal/tevcal.f
$(d0root)/d0geant/cdc/tevcdc.f:\
  $(d0root)/d0geant/cdc/tevcdc.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/tevcdc.for | vmstounix > $(d0root)/d0geant/cdc/tevcdc.f
$(d0root)/d0geant/fdc/tevfdc.f:\
  $(d0root)/d0geant/fdc/tevfdc.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/tevfdc.for | vmstounix > $(d0root)/d0geant/fdc/tevfdc.f
$(d0root)/d0geant/lv0/tevlv0.f:\
  $(d0root)/d0geant/lv0/tevlv0.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/lv0/tevlv0.for | vmstounix > $(d0root)/d0geant/lv0/tevlv0.f
$(d0root)/d0geant/muo/tevmuo.f:\
  $(d0root)/d0geant/muo/tevmuo.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/tevmuo.for | vmstounix > $(d0root)/d0geant/muo/tevmuo.f
$(d0root)/d0geant/samus/tevsam.f:\
  $(d0root)/d0geant/samus/tevsam.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/tevsam.for | vmstounix > $(d0root)/d0geant/samus/tevsam.f
$(d0root)/d0geant/trd/tevtrd.f:\
  $(d0root)/d0geant/trd/tevtrd.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/tevtrd.for | vmstounix > $(d0root)/d0geant/trd/tevtrd.f
$(d0root)/d0geant/vtx/tevvtx.f:\
  $(d0root)/d0geant/vtx/tevvtx.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/tevvtx.for | vmstounix > $(d0root)/d0geant/vtx/tevvtx.f
$(d0root)/d0geant/trd/tlayer.f:\
  $(d0root)/d0geant/trd/tlayer.for\
  $(d0library)/inc/posit.inc\
  $(d0library)/inc/gcsets.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/tlayer.for | vmstounix > $(d0root)/d0geant/trd/tlayer.f
$(d0root)/d0geant/trd/trabsl.f:\
  $(d0root)/d0geant/trd/trabsl.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/trabsl.for | vmstounix > $(d0root)/d0geant/trd/trabsl.f
$(d0root)/d0geant/zebio/trafra.f:\
  $(d0root)/d0geant/zebio/trafra.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/trafra.for | vmstounix > $(d0root)/d0geant/zebio/trafra.f
$(d0root)/d0geant/trd/trchan.f:\
  $(d0root)/d0geant/trd/trchan.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcunit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/trchan.for | vmstounix > $(d0root)/d0geant/trd/trchan.f
$(d0root)/d0geant/trd/trdblk.f:\
  $(d0root)/d0geant/trd/trdblk.for\
  $(d0library)/inc/fadccn.inc\
  $(d0library)/inc/faddig.inc\
  $(d0library)/inc/info.inc\
  $(d0library)/inc/nortrd.inc\
  $(d0library)/inc/probde.inc\
  $(d0library)/inc/raddsc.inc\
  $(d0library)/inc/tecdsc.inc\
  $(d0library)/inc/windsc.inc\
  $(d0library)/inc/raddcc.inc\
  $(d0library)/inc/tecdcc.inc\
  $(d0library)/inc/windcc.inc\
  $(d0library)/inc/xray.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/trdblk.for | vmstounix > $(d0root)/d0geant/trd/trdblk.f
$(d0root)/d0geant/trd/trdfmy.f:\
  $(d0root)/d0geant/trd/trdfmy.for\
  $(d0library)/inc/gcunit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/trdfmy.for | vmstounix > $(d0root)/d0geant/trd/trdfmy.f
$(d0root)/d0geant/trd/trdshp.f:\
  $(d0root)/d0geant/trd/trdshp.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/fadccn.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/nortrd.inc\
  $(d0library)/inc/worksp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/trdshp.for | vmstounix > $(d0root)/d0geant/trd/trdshp.f
$(d0root)/d0geant/trd/trdtim.f:\
  $(d0root)/d0geant/trd/trdtim.for\
  $(d0library)/inc/gcunit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/trdtim.for | vmstounix > $(d0root)/d0geant/trd/trdtim.f
$(d0root)/d0geant/trd/trdunp.f:\
  $(d0root)/d0geant/trd/trdunp.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/trdunp.for | vmstounix > $(d0root)/d0geant/trd/trdunp.f
$(d0root)/d0geant/trd/trdwde.f:\
  $(d0root)/d0geant/trd/trdwde.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/trdwde.for | vmstounix > $(d0root)/d0geant/trd/trdwde.f
$(d0root)/d0geant/trd/trdyld.f:\
  $(d0root)/d0geant/trd/trdyld.for\
  $(d0library)/inc/absor.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/raddsc.inc\
  $(d0library)/inc/raddcc.inc\
  $(d0library)/inc/tecdsc.inc\
  $(d0library)/inc/windsc.inc\
  $(d0library)/inc/xspect.inc\
  $(d0library)/inc/xray.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/trdyld.for | vmstounix > $(d0root)/d0geant/trd/trdyld.f
$(d0root)/d0geant/zebio/trgpmc.f:\
  $(d0root)/d0geant/zebio/trgpmc.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/fadccn.inc\
  $(d0library)/inc/geomtc.inc\
  $(d0library)/inc/geomtr.inc\
  $(d0library)/inc/pedtrd.inc\
  $(d0library)/inc/nortrd.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/trgpmc.for | vmstounix > $(d0root)/d0geant/zebio/trgpmc.f
$(d0root)/d0geant/trd/trhigh.f:\
  $(d0root)/d0geant/trd/trhigh.for\
  $(d0library)/inc/gconst.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcunit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/trhigh.for | vmstounix > $(d0root)/d0geant/trd/trhigh.f
$(d0root)/d0geant/cal/trkcal.f:\
  $(d0root)/d0geant/cal/trkcal.for\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/dcalog.inc\
  $(d0library)/inc/gcbank.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gclink.inc\
  $(d0library)/inc/gcstak.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/trkcal.for | vmstounix > $(d0root)/d0geant/cal/trkcal.f
$(d0root)/d0geant/cdc/trkcdc.f:\
  $(d0root)/d0geant/cdc/trkcdc.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/trkcdc.for | vmstounix > $(d0root)/d0geant/cdc/trkcdc.f
$(d0root)/d0geant/fdc/trkfdc.f:\
  $(d0root)/d0geant/fdc/trkfdc.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/fdc/trkfdc.for | vmstounix > $(d0root)/d0geant/fdc/trkfdc.f
$(d0root)/d0geant/lv0/trklv0.f:\
  $(d0root)/d0geant/lv0/trklv0.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/lv0/trklv0.for | vmstounix > $(d0root)/d0geant/lv0/trklv0.f
$(d0root)/d0geant/muo/trkmuo.f:\
  $(d0root)/d0geant/muo/trkmuo.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/trkmuo.for | vmstounix > $(d0root)/d0geant/muo/trkmuo.f
$(d0root)/d0geant/samus/trksam.f:\
  $(d0root)/d0geant/samus/trksam.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/samus/trksam.for | vmstounix > $(d0root)/d0geant/samus/trksam.f
$(d0root)/d0geant/trd/trktrd.f:\
  $(d0root)/d0geant/trd/trktrd.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/trktrd.for | vmstounix > $(d0root)/d0geant/trd/trktrd.f
$(d0root)/d0geant/vtx/trkvtx.f:\
  $(d0root)/d0geant/vtx/trkvtx.for\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/trkvtx.for | vmstounix > $(d0root)/d0geant/vtx/trkvtx.f
$(d0root)/d0geant/trd/trnumb.f:\
  $(d0root)/d0geant/trd/trnumb.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/trnumb.for | vmstounix > $(d0root)/d0geant/trd/trnumb.f
$(d0root)/d0geant/trd/trtabl.f:\
  $(d0root)/d0geant/trd/trtabl.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/tabx.inc\
  $(d0library)/inc/gconst.inc\
  $(d0library)/inc/worksp.inc\
  $(d0library)/inc/xray.inc\
  $(d0library)/inc/xspect.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/raddsc.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/trtabl.for | vmstounix > $(d0root)/d0geant/trd/trtabl.f
$(d0root)/d0geant/caltower/ucindx.f:\
  $(d0root)/d0geant/caltower/ucindx.for\
  $(d0library)/inc/gconst.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/caltower/ucindx.for | vmstounix > $(d0root)/d0geant/caltower/ucindx.f
$(d0root)/d0geant/dzero/uginit.f:\
  $(d0root)/d0geant/dzero/uginit.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/gclist.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gcphys.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/uginit.for | vmstounix > $(d0root)/d0geant/dzero/uginit.f
$(d0root)/d0geant/dzero/uglast.f:\
  $(d0root)/d0geant/dzero/uglast.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gclist.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/uglast.for | vmstounix > $(d0root)/d0geant/dzero/uglast.f
$(d0root)/d0geant/muo/use_mulib.f:\
  $(d0root)/d0geant/muo/use_mulib.for\
  $(d0library)/inc/mulcon.inc\
  $(d0library)/inc/muldat.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/use_mulib.for | vmstounix > $(d0root)/d0geant/muo/use_mulib.f
$(d0root)/d0geant/muo/use_muonlibrary.f:\
  $(d0root)/d0geant/muo/use_muonlibrary.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/mulcon.inc\
  $(d0library)/inc/muldat.inc\
  $(d0library)/inc/shsort.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zlinkc.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/links/izisp1.link\
  $(d0library)/inc/shlitr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/muo/use_muonlibrary.for | vmstounix > $(d0root)/d0geant/muo/use_muonlibrary.f
$(d0root)/d0geant/zebio/use_shlb.f:\
  $(d0root)/d0geant/zebio/use_shlb.for\
  $(d0library)/params/cal_offline.params\
  $(d0library)/inc/shlcon.inc\
  $(d0library)/inc/shldat.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/use_shlb.for | vmstounix > $(d0root)/d0geant/zebio/use_shlb.f
$(d0root)/d0geant/zebio/use_showerlibrary.f:\
  $(d0root)/d0geant/zebio/use_showerlibrary.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/shlcon.inc\
  $(d0library)/inc/shldat.inc\
  $(d0library)/inc/shsort.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zlinkc.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/dcalog.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gckine.inc\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gcking.inc\
  $(d0library)/params/isajet_codes.def\
  $(d0library)/links/izisp1.link\
  $(d0library)/links/izisv2.link\
  $(d0library)/links/izisp2.link\
  $(d0library)/inc/shlitr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/use_showerlibrary.for | vmstounix > $(d0root)/d0geant/zebio/use_showerlibrary.f
$(d0root)/d0geant/dzero/vdzero.f:\
  $(d0root)/d0geant/dzero/vdzero.for\
  $(d0library)/inc/gcflag.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/dzero/vdzero.for | vmstounix > $(d0root)/d0geant/dzero/vdzero.f
$(d0root)/d0geant/vtx/vlanda.f:\
  $(d0root)/d0geant/vtx/vlanda.for\
  $(d0library)/inc/gctrak.inc\
  $(d0library)/inc/gckine.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/vlanda.for | vmstounix > $(d0root)/d0geant/vtx/vlanda.f
$(d0root)/d0geant/cal/volord.f:\
  $(d0root)/d0geant/cal/volord.for\
  $(d0library)/inc/srcpr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cal/volord.for | vmstounix > $(d0root)/d0geant/cal/volord.f
$(d0root)/d0geant/utl/volpos.f:\
  $(d0root)/d0geant/utl/volpos.for\
  $(d0library)/inc/srcpr.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/utl/volpos.for | vmstounix > $(d0root)/d0geant/utl/volpos.f
$(d0root)/d0geant/utl/volpoz.f:\
  $(d0root)/d0geant/utl/volpoz.for\
  $(d0library)/inc/srcpr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/utl/volpoz.for | vmstounix > $(d0root)/d0geant/utl/volpoz.f
$(d0root)/d0geant/vtx/vtxpul.f:\
  $(d0root)/d0geant/vtx/vtxpul.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/vtxpsh.inc\
  $(d0library)/inc/vtloca.inc\
  $(d0library)/inc/vtxlnk.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/vtxpul.for | vmstounix > $(d0root)/d0geant/vtx/vtxpul.f
$(d0root)/d0geant/vtx/vtxwrt.f:\
  $(d0root)/d0geant/vtx/vtxwrt.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/vtxlnk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/vtxwrt.for | vmstounix > $(d0root)/d0geant/vtx/vtxwrt.f
$(d0root)/d0geant/zebio/wrzeb.f:\
  $(d0root)/d0geant/zebio/wrzeb.for\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/gcflag.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebio.inc\
  $(d0library)/inc/zebiog.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/mioflg.inc\
  $(d0library)/inc/getcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/wrzeb.for | vmstounix > $(d0root)/d0geant/zebio/wrzeb.f
$(d0root)/d0geant/trd/xinter.f:\
  $(d0root)/d0geant/trd/xinter.for\
  $(d0library)/inc/grutil.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/xinter.for | vmstounix > $(d0root)/d0geant/trd/xinter.f
$(d0root)/d0geant/trd/xlum.f:\
  $(d0root)/d0geant/trd/xlum.for\
  $(d0library)/inc/absor.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/gconst.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/xlum.for | vmstounix > $(d0root)/d0geant/trd/xlum.f
$(d0root)/d0geant/trd/xrad.f:\
  $(d0root)/d0geant/trd/xrad.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/xspect.inc\
  $(d0library)/inc/tabx.inc\
  $(d0library)/inc/xray.inc\
  $(d0library)/inc/d0log.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/trd/xrad.for | vmstounix > $(d0root)/d0geant/trd/xrad.f
$(d0root)/d0geant/vtx/zadped.f:\
  $(d0root)/d0geant/vtx/zadped.for\
  $(d0library)/inc/gcflag.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/zadped.for | vmstounix > $(d0root)/d0geant/vtx/zadped.f
$(d0root)/d0geant/cdc/zcnvrt.f:\
  $(d0root)/d0geant/cdc/zcnvrt.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/zcnvrt.for | vmstounix > $(d0root)/d0geant/cdc/zcnvrt.f
$(d0root)/d0geant/zebio/zebfin.f:\
  $(d0root)/d0geant/zebio/zebfin.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebio.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/zebfin.for | vmstounix > $(d0root)/d0geant/zebio/zebfin.f
$(d0root)/d0geant/zebio/zebini.f:\
  $(d0root)/d0geant/zebio/zebini.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/zebini.for | vmstounix > $(d0root)/d0geant/zebio/zebini.f
$(d0root)/d0geant/zebio/zebout.f:\
  $(d0root)/d0geant/zebio/zebout.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/zebio/zebout.for | vmstounix > $(d0root)/d0geant/zebio/zebout.f
$(d0root)/d0geant/cdc/zersup.f:\
  $(d0root)/d0geant/cdc/zersup.for\
  $(d0library)/inc/zzerop.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/cdc/zersup.for | vmstounix > $(d0root)/d0geant/cdc/zersup.f
$(d0root)/d0geant/vtx/zinduc.f:\
  $(d0root)/d0geant/vtx/zinduc.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/zinduc.for | vmstounix > $(d0root)/d0geant/vtx/zinduc.f
$(d0root)/d0geant/vtx/zsinit.f:\
  $(d0root)/d0geant/vtx/zsinit.for\
  $(d0library)/inc/vzhits.inc\
  $(d0library)/inc/vzdata.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/zsinit.for | vmstounix > $(d0root)/d0geant/vtx/zsinit.f
$(d0root)/d0geant/vtx/zstrht.f:\
  $(d0root)/d0geant/vtx/zstrht.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/vzhits.inc\
  $(d0library)/inc/vtloca.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/zstrht.for | vmstounix > $(d0root)/d0geant/vtx/zstrht.f
$(d0root)/d0geant/vtx/zstrip.f:\
  $(d0root)/d0geant/vtx/zstrip.for\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/d0log.inc\
  $(d0library)/inc/vzdata.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/vtloca.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/zstrip.for | vmstounix > $(d0root)/d0geant/vtx/zstrip.f
$(d0root)/d0geant/vtx/zstrno.f:\
  $(d0root)/d0geant/vtx/zstrno.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0geant/vtx/zstrno.for | vmstounix > $(d0root)/d0geant/vtx/zstrno.f
for:
	touch $(d0root)/d0geant/cal/absorb.for
	touch $(d0root)/d0geant/cal/anlcal.for
	touch $(d0root)/d0geant/cdc/anlcdc.for
	touch $(d0root)/d0geant/fdc/anlfdc.for
	touch $(d0root)/d0geant/lv0/anllv0.for
	touch $(d0root)/d0geant/muo/anlmuo.for
	touch $(d0root)/d0geant/samus/anlsam.for
	touch $(d0root)/d0geant/trd/anltrd.for
	touch $(d0root)/d0geant/vtx/anlvtx.for
	touch $(d0root)/d0geant/trd/aphasa.for
	touch $(d0root)/d0geant/utl/bkd0rg.for
	touch $(d0root)/d0geant/cal/bkgcah.for
	touch $(d0root)/d0geant/dzero/bkgswt.for
	touch $(d0root)/d0geant/zebio/bkisp2.for
	touch $(d0root)/d0geant/zebio/bkisv2.for
	touch $(d0root)/d0geant/vtx/blcdd1.for
	touch $(d0root)/d0geant/cdc/blcdd2.for
	touch $(d0root)/d0geant/fdc/blcdd3.for
	touch $(d0root)/d0geant/cdc/bldcda.for
	touch $(d0root)/d0geant/cdc/bldsec.for
	touch $(d0root)/d0geant/vtx/blvsec.for
	touch $(d0root)/d0geant/vtx/blvwda.for
	touch $(d0root)/d0geant/vtx/blvzda.for
	touch $(d0root)/d0geant/vtx/blvzla.for
	touch $(d0root)/d0geant/cal/bokcal.for
	touch $(d0root)/d0geant/cdc/bokcdc.for
	touch $(d0root)/d0geant/fdc/bokfdc.for
	touch $(d0root)/d0geant/lv0/boklv0.for
	touch $(d0root)/d0geant/muo/bokmuo.for
	touch $(d0root)/d0geant/samus/boksam.for
	touch $(d0root)/d0geant/trd/boktrd.for
	touch $(d0root)/d0geant/vtx/bokvtx.for
	touch $(d0root)/d0geant/caltower/cacmpr.for
	touch $(d0root)/d0geant/caltower/calbld.for
	touch $(d0root)/d0geant/utl/cd_head_flg.for
	touch $(d0root)/d0geant/zebio/cd_hstr_flg.for
	touch $(d0root)/d0geant/cdc/cdatdl.for
	touch $(d0root)/d0geant/cdc/cdatsw.for
	touch $(d0root)/d0geant/cdc/cdcwrt.for
	touch $(d0root)/d0geant/vtx/cdd1fl.for
	touch $(d0root)/d0geant/cdc/cdd2fl.for
	touch $(d0root)/d0geant/fdc/cdd3fl.for
	touch $(d0root)/d0geant/trd/cdd4fl.for
	touch $(d0root)/d0geant/cdc/cdelay.for
	touch $(d0root)/d0geant/cdc/cdhits.for
	touch $(d0root)/d0geant/cdc/cdresp.for
	touch $(d0root)/d0geant/cdc/cdrift.for
	touch $(d0root)/d0geant/vtx/chgdiv.for
	touch $(d0root)/d0geant/zebio/chkbrr.for
	touch $(d0root)/d0geant/cdc/clanda.for
	touch $(d0root)/d0geant/cal/clncal.for
	touch $(d0root)/d0geant/cdc/clncdc.for
	touch $(d0root)/d0geant/fdc/clnfdc.for
	touch $(d0root)/d0geant/lv0/clnlv0.for
	touch $(d0root)/d0geant/muo/clnmuo.for
	touch $(d0root)/d0geant/samus/clnsam.for
	touch $(d0root)/d0geant/trd/clntrd.for
	touch $(d0root)/d0geant/vtx/clnvtx.for
	touch $(d0root)/d0geant/trd/clsmen.for
	touch $(d0root)/d0geant/trd/clucle.for
	touch $(d0root)/d0geant/trd/cludel.for
	touch $(d0root)/d0geant/trd/cludig.for
	touch $(d0root)/d0geant/trd/clumix.for
	touch $(d0root)/d0geant/trd/clumrg.for
	touch $(d0root)/d0geant/trd/clupos.for
	touch $(d0root)/d0geant/trd/cluprn.for
	touch $(d0root)/d0geant/trd/clurad.for
	touch $(d0root)/d0geant/trd/clusmr.for
	touch $(d0root)/d0geant/trd/clusor.for
	touch $(d0root)/d0geant/trd/clustf.for
	touch $(d0root)/d0geant/zebio/cpd0rg.for
	touch $(d0root)/d0geant/dzero/d0main.for
	touch $(d0root)/d0geant/dzero/d0xint.for
	touch $(d0root)/d0geant/cdc/datapk.for
	touch $(d0root)/d0geant/caltower/dchit.for
	touch $(d0root)/d0geant/caltower/dehit.for
	touch $(d0root)/d0geant/fdc/deldig.for
	touch $(d0root)/d0geant/cal/detcal.for
	touch $(d0root)/d0geant/cdc/detcdc.for
	touch $(d0root)/d0geant/cal/detded.for
	touch $(d0root)/d0geant/cal/deteca.for
	touch $(d0root)/d0geant/fdc/detfdc.for
	touch $(d0root)/d0geant/lv0/detlv0.for
	touch $(d0root)/d0geant/muo/detmuo.for
	touch $(d0root)/d0geant/muo/detmuo_v1.for
	touch $(d0root)/d0geant/samus/detsam.for
	touch $(d0root)/d0geant/cal/detscn.for
	touch $(d0root)/d0geant/trd/dettrd.for
	touch $(d0root)/d0geant/cal/detuca.for
	touch $(d0root)/d0geant/vtx/detvtx.for
	touch $(d0root)/d0geant/dzero/dffini.for
	touch $(d0root)/d0geant/caltower/dhstor.for
	touch $(d0root)/d0geant/cal/digcal.for
	touch $(d0root)/d0geant/cdc/digcdc.for
	touch $(d0root)/d0geant/cal/digded.for
	touch $(d0root)/d0geant/cal/digeca.for
	touch $(d0root)/d0geant/fdc/digfdc.for
	touch $(d0root)/d0geant/lv0/diglv0.for
	touch $(d0root)/d0geant/muo/digmuo.for
	touch $(d0root)/d0geant/muo/digmuo_v1.for
	touch $(d0root)/d0geant/samus/digsam.for
	touch $(d0root)/d0geant/cal/digscn.for
	touch $(d0root)/d0geant/trd/digtrd.for
	touch $(d0root)/d0geant/cal/diguca.for
	touch $(d0root)/d0geant/vtx/digvtx.for
	touch $(d0root)/d0geant/utl/dodigi.for
	touch $(d0root)/d0geant/caltower/dschit.for
	touch $(d0root)/d0geant/caltower/dshit.for
	touch $(d0root)/d0geant/caltower/duhit.for
	touch $(d0root)/d0geant/cdc/dvecda.for
	touch $(d0root)/d0geant/trd/dwdegr.for
	touch $(d0root)/d0geant/dzero/dzemat.for
	touch $(d0root)/d0geant/dzero/dzero.for
	touch $(d0root)/d0geant/caltower/ecindx.for
	touch $(d0root)/d0geant/trd/edegen.for
	touch $(d0root)/d0geant/zebio/endzeb.for
	touch $(d0root)/d0geant/cal/enicdh.for
	touch $(d0root)/d0geant/cal/enjets.for
	touch $(d0root)/d0geant/cal/enjpts.for
	touch $(d0root)/d0geant/trd/entrd.for
	touch $(d0root)/d0geant/trd/exrgen.for
	touch $(d0root)/d0geant/trd/extrap.for
	touch $(d0root)/d0geant/cdc/fadcdl.for
	touch $(d0root)/d0geant/cdc/fadcfr.for
	touch $(d0root)/d0geant/cdc/fadcsw.for
	touch $(d0root)/d0geant/vtx/fadcvs.for
	touch $(d0root)/d0geant/vtx/fadcvz.for
	touch $(d0root)/d0geant/dzero/fajxyz.for
	touch $(d0root)/d0geant/fdc/fdcwrt.for
	touch $(d0root)/d0geant/cal/ffical.for
	touch $(d0root)/d0geant/cdc/fficdc.for
	touch $(d0root)/d0geant/d0/ffid0.for
	touch $(d0root)/d0geant/fdc/ffifdc.for
	touch $(d0root)/d0geant/lv0/ffilv0.for
	touch $(d0root)/d0geant/muo/ffimuo.for
	touch $(d0root)/d0geant/samus/ffisam.for
	touch $(d0root)/d0geant/trd/ffitrd.for
	touch $(d0root)/d0geant/vtx/ffivtx.for
	touch $(d0root)/d0geant/vtx/ficdd1.for
	touch $(d0root)/d0geant/cdc/ficdd2.for
	touch $(d0root)/d0geant/fdc/ficdd3.for
	touch $(d0root)/d0geant/cdc/fidcda.for
	touch $(d0root)/d0geant/cdc/fidsec.for
	touch $(d0root)/d0geant/vtx/fivzda.for
	touch $(d0root)/d0geant/dzero/fld0rg.for
	touch $(d0root)/d0geant/caltower/flgcah.for
	touch $(d0root)/d0geant/dzero/flgval.for
	touch $(d0root)/d0geant/cal/flicdh.for
	touch $(d0root)/d0geant/cal/fljets.for
	touch $(d0root)/d0geant/cal/fljpts.for
	touch $(d0root)/d0geant/fdc/fpdata.for
	touch $(d0root)/d0geant/fdc/fphits.for
	touch $(d0root)/d0geant/fdc/ftdata.for
	touch $(d0root)/d0geant/fdc/ftdlda.for
	touch $(d0root)/d0geant/fdc/fthits.for
	touch $(d0root)/d0geant/dzero/gdcole.for
	touch $(d0root)/d0geant/zebio/geaisa.for
	touch $(d0root)/d0geant/cal/geecel.for
	touch $(d0root)/d0geant/cal/geecip.for
	touch $(d0root)/d0geant/cal/geecmh.for
	touch $(d0root)/d0geant/cal/geecoh.for
	touch $(d0root)/d0geant/cal/geocal.for
	touch $(d0root)/d0geant/cdc/geocdc.for
	touch $(d0root)/d0geant/d0/geod0.for
	touch $(d0root)/d0geant/cal/geoeca.for
	touch $(d0root)/d0geant/cal/geoecr.for
	touch $(d0root)/d0geant/fdc/geofdc.for
	touch $(d0root)/d0geant/lv0/geolv0.for
	touch $(d0root)/d0geant/muo/geomuo.for
	touch $(d0root)/d0geant/dzero/geomvl.for
	touch $(d0root)/d0geant/samus/geosam.for
	touch $(d0root)/d0geant/trd/geotrd.for
	touch $(d0root)/d0geant/cal/geouca.for
	touch $(d0root)/d0geant/cal/geoucr.for
	touch $(d0root)/d0geant/vtx/geovtx.for
	touch $(d0root)/d0geant/cal/gescin.for
	touch $(d0root)/d0geant/vtx/get_pulse.for
	touch $(d0root)/d0geant/caltower/get_type_interact.for
	touch $(d0root)/d0geant/caltower/getbin.for
	touch $(d0root)/d0geant/caltower/getcel.for
	touch $(d0root)/d0geant/utl/getdet.for
	touch $(d0root)/d0geant/cal/geucel.for
	touch $(d0root)/d0geant/cal/geucfh.for
	touch $(d0root)/d0geant/cal/geuclk.for
	touch $(d0root)/d0geant/cal/geumsg.for
	touch $(d0root)/d0geant/dzero/gftver.for
	touch $(d0root)/d0geant/dzero/gfver1.for
	touch $(d0root)/d0geant/vtx/gget.for
	touch $(d0root)/d0geant/dzero/ginvol.for
	touch $(d0root)/d0geant/dzero/gitran.for
	touch $(d0root)/d0geant/dzero/gltrac.for
	touch $(d0root)/d0geant/dzero/gnsout.for
	touch $(d0root)/d0geant/dzero/goking.for
	touch $(d0root)/d0geant/dzero/gsmate.for
	touch $(d0root)/d0geant/dzero/gsmixt.for
	touch $(d0root)/d0geant/dzero/gspart.for
	touch $(d0root)/d0geant/dzero/gsrotm.for
	touch $(d0root)/d0geant/cdc/gtelke.for
	touch $(d0root)/d0geant/utl/gtimer.for
	touch $(d0root)/d0geant/dzero/gtmedi.for
	touch $(d0root)/d0geant/dzero/gtnino.for
	touch $(d0root)/d0geant/cdc/gtprim.for
	touch $(d0root)/d0geant/dzero/gtrnsf.for
	touch $(d0root)/d0geant/cdc/gtscnd.for
	touch $(d0root)/d0geant/dzero/gtswch.for
	touch $(d0root)/d0geant/dzero/gudigi.for
	touch $(d0root)/d0geant/muo/gufld.for
	touch $(d0root)/d0geant/muo/gufld_v1.for
	touch $(d0root)/d0geant/dzero/guhadr.for
	touch $(d0root)/d0geant/cal/guical.for
	touch $(d0root)/d0geant/cdc/guicdc.for
	touch $(d0root)/d0geant/fdc/guifdc.for
	touch $(d0root)/d0geant/dzero/guiget.for
	touch $(d0root)/d0geant/lv0/guilv0.for
	touch $(d0root)/d0geant/muo/guimuo.for
	touch $(d0root)/d0geant/samus/guisam.for
	touch $(d0root)/d0geant/trd/guitrd.for
	touch $(d0root)/d0geant/vtx/guivtx.for
	touch $(d0root)/d0geant/dzero/gukine.for
	touch $(d0root)/d0geant/dzero/guout.for
	touch $(d0root)/d0geant/dzero/guphad.for
	touch $(d0root)/d0geant/dzero/gupxyz.for
	touch $(d0root)/d0geant/dzero/gustep.for
	touch $(d0root)/d0geant/dzero/gutrak.for
	touch $(d0root)/d0geant/dzero/gutrev.for
	touch $(d0root)/d0geant/dzero/gz_clean.for
	touch $(d0root)/d0geant/utl/gzd0rg.for
	touch $(d0root)/d0geant/utl/gzd0rgz.for
	touch $(d0root)/d0geant/dzero/gzgswt.for
	touch $(d0root)/d0geant/dzero/gzgswtz.for
	touch $(d0root)/d0geant/cal/hcfill.for
	touch $(d0root)/d0geant/cal/hlpcal.for
	touch $(d0root)/d0geant/cdc/hlpcdc.for
	touch $(d0root)/d0geant/d0/hlpd0.for
	touch $(d0root)/d0geant/fdc/hlpfdc.for
	touch $(d0root)/d0geant/lv0/hlplv0.for
	touch $(d0root)/d0geant/muo/hlpmuo.for
	touch $(d0root)/d0geant/samus/hlpsam.for
	touch $(d0root)/d0geant/trd/hlptrd.for
	touch $(d0root)/d0geant/vtx/hlpvtx.for
	touch $(d0root)/d0geant/cal/inical.for
	touch $(d0root)/d0geant/cdc/inicdc.for
	touch $(d0root)/d0geant/d0/inid0.for
	touch $(d0root)/d0geant/fdc/inifdc.for
	touch $(d0root)/d0geant/lv0/inilv0.for
	touch $(d0root)/d0geant/muo/inimuo.for
	touch $(d0root)/d0geant/samus/inisam.for
	touch $(d0root)/d0geant/trd/initab.for
	touch $(d0root)/d0geant/trd/initrd.for
	touch $(d0root)/d0geant/vtx/inivtx.for
	touch $(d0root)/d0geant/cal/intcal.for
	touch $(d0root)/d0geant/cdc/intcdc.for
	touch $(d0root)/d0geant/d0/intd0.for
	touch $(d0root)/d0geant/trd/interp.for
	touch $(d0root)/d0geant/fdc/intfdc.for
	touch $(d0root)/d0geant/lv0/intlv0.for
	touch $(d0root)/d0geant/muo/intmuo.for
	touch $(d0root)/d0geant/samus/intsam.for
	touch $(d0root)/d0geant/trd/inttrd.for
	touch $(d0root)/d0geant/vtx/intvtx.for
	touch $(d0root)/d0geant/zebio/isagea.for
	touch $(d0root)/d0geant/zebio/isajet_geant_track.for
	touch $(d0root)/d0geant/zebio/isajet_geant_vtx.for
	touch $(d0root)/d0geant/zebio/isdrop.for
	touch $(d0root)/d0geant/zebio/iskine.for
	touch $(d0root)/d0geant/zebio/isout.for
	touch $(d0root)/d0geant/cal/isp1_link.for
	touch $(d0root)/d0geant/zebio/israw.for
	touch $(d0root)/d0geant/zebio/isread.for
	touch $(d0root)/d0geant/zebio/isref.for
	touch $(d0root)/d0geant/zebio/isubfn.for
	touch $(d0root)/d0geant/zebio/iszeb.for
	touch $(d0root)/d0geant/cal/jetend.for
	touch $(d0root)/d0geant/cal/jetfil.for
	touch $(d0root)/d0geant/cal/jetini.for
	touch $(d0root)/d0geant/d0/kind0.for
	touch $(d0root)/d0geant/lv0/l0_adc_expect.for
	touch $(d0root)/d0geant/lv0/l0_pack_adc.for
	touch $(d0root)/d0geant/lv0/l0_pack_header.for
	touch $(d0root)/d0geant/lv0/l0_pack_trailer.for
	touch $(d0root)/d0geant/lv0/l0_pack_vertex.for
	touch $(d0root)/d0geant/lv0/l0_run1a_setup.for
	touch $(d0root)/d0geant/lv0/l0_run1b_setup.for
	touch $(d0root)/d0geant/fdc/lastfdcstp.for
	touch $(d0root)/d0geant/trd/lasttrdstp.for
	touch $(d0root)/d0geant/dzero/lddum.for
	touch $(d0root)/d0geant/fdc/ldfpda.for
	touch $(d0root)/d0geant/fdc/ldfpsc.for
	touch $(d0root)/d0geant/fdc/ldftda.for
	touch $(d0root)/d0geant/fdc/ldftsc.for
	touch $(d0root)/d0geant/vtx/ldvsec.for
	touch $(d0root)/d0geant/vtx/ldvwda.for
	touch $(d0root)/d0geant/vtx/ldvzda.for
	touch $(d0root)/d0geant/vtx/ldvzla.for
	touch $(d0root)/d0geant/utl/locgeo.for
	touch $(d0root)/d0geant/cal/lstcal.for
	touch $(d0root)/d0geant/cdc/lstcdc.for
	touch $(d0root)/d0geant/fdc/lstfdc.for
	touch $(d0root)/d0geant/lv0/lstlv0.for
	touch $(d0root)/d0geant/muo/lstmuo.for
	touch $(d0root)/d0geant/samus/lstsam.for
	touch $(d0root)/d0geant/trd/lsttrd.for
	touch $(d0root)/d0geant/vtx/lstvtx.for
	touch $(d0root)/d0geant/dzero/lutra1.for
	touch $(d0root)/d0geant/muo/mcalexit.for
	touch $(d0root)/d0geant/cal/mencal.for
	touch $(d0root)/d0geant/cdc/mencdc.for
	touch $(d0root)/d0geant/d0/mend0.for
	touch $(d0root)/d0geant/fdc/menfdc.for
	touch $(d0root)/d0geant/lv0/menlv0.for
	touch $(d0root)/d0geant/muo/menmuo.for
	touch $(d0root)/d0geant/samus/mensam.for
	touch $(d0root)/d0geant/trd/mentrd.for
	touch $(d0root)/d0geant/vtx/menvtx.for
	touch $(d0root)/d0geant/cdc/mkdcdc.for
	touch $(d0root)/d0geant/fdc/mkfphi.for
	touch $(d0root)/d0geant/fdc/mkfthe.for
	touch $(d0root)/d0geant/vtx/mkvtxs.for
	touch $(d0root)/d0geant/vtx/mkvtxw.for
	touch $(d0root)/d0geant/muo/mscrat.for
	touch $(d0root)/d0geant/muo/msdelt.for
	touch $(d0root)/d0geant/muo/msdrft.for
	touch $(d0root)/d0geant/muo/msgnum.for
	touch $(d0root)/d0geant/muo/mshits.for
	touch $(d0root)/d0geant/muo/msindx.for
	touch $(d0root)/d0geant/muo/msindx_v1.for
	touch $(d0root)/d0geant/muo/msmud1.for
	touch $(d0root)/d0geant/muo/msmud1_v1.for
	touch $(d0root)/d0geant/muo/msmudh.for
	touch $(d0root)/d0geant/muo/mspad.for
	touch $(d0root)/d0geant/muo/mspad_v1.for
	touch $(d0root)/d0geant/muo/mspadr.for
	touch $(d0root)/d0geant/muo/mspads.for
	touch $(d0root)/d0geant/muo/mstrak.for
	touch $(d0root)/d0geant/muo/mstvc.for
	touch $(d0root)/d0geant/muo/mstvc_v1.for
	touch $(d0root)/d0geant/muo/msvchk.for
	touch $(d0root)/d0geant/muo/mudrft.for
	touch $(d0root)/d0geant/cal/mxcal.for
	touch $(d0root)/d0geant/lv0/mxlv0.for
	touch $(d0root)/d0geant/utl/norran.for
	touch $(d0root)/d0geant/dzero/nran.for
	touch $(d0root)/d0geant/vtx/nxtzst.for
	touch $(d0root)/d0geant/dzero/pcolor.for
	touch $(d0root)/d0geant/dzero/prgswt.for
	touch $(d0root)/d0geant/dzero/print_step.for
	touch $(d0root)/d0geant/muo/prmmap.for
	touch $(d0root)/d0geant/dzero/prune_gskine.for
	touch $(d0root)/d0geant/zebio/rdzeb.for
	touch $(d0root)/d0geant/zebio/rotino.for
	touch $(d0root)/d0geant/cal/rrlam.for
	touch $(d0root)/d0geant/trd/rwxspc.for
	touch $(d0root)/d0geant/samus/safill.for
	touch $(d0root)/d0geant/samus/saicmp.for
	touch $(d0root)/d0geant/samus/sam_bp.for
	touch $(d0root)/d0geant/samus/sam_ma.for
	touch $(d0root)/d0geant/samus/samadc.for
	touch $(d0root)/d0geant/samus/samgt1.for
	touch $(d0root)/d0geant/samus/samgt2.for
	touch $(d0root)/d0geant/samus/saname.for
	touch $(d0root)/d0geant/samus/saslb1.for
	touch $(d0root)/d0geant/samus/saslb2.for
	touch $(d0root)/d0geant/samus/sastat.for
	touch $(d0root)/d0geant/samus/satube.for
	touch $(d0root)/d0geant/cal/set_electron_pars.for
	touch $(d0root)/d0geant/cal/set_shower_param.for
	touch $(d0root)/d0geant/utl/setdet.for
	touch $(d0root)/d0geant/utl/setrot.for
	touch $(d0root)/d0geant/utl/setvew.for
	touch $(d0root)/d0geant/caltower/shcopy.for
	touch $(d0root)/d0geant/caltower/shinit.for
	touch $(d0root)/d0geant/caltower/shlast.for
	touch $(d0root)/d0geant/zebio/shlb_cal_xyz.for
	touch $(d0root)/d0geant/cal/shlb_elec_match_key.for
	touch $(d0root)/d0geant/zebio/shlb_move_track.for
	touch $(d0root)/d0geant/cdc/simpul.for
	touch $(d0root)/d0geant/utl/slopan.for
	touch $(d0root)/d0geant/caltower/smrfac.for
	touch $(d0root)/d0geant/dzero/snapshot.for
	touch $(d0root)/d0geant/trd/splgar.for
	touch $(d0root)/d0geant/lv0/stolv0.for
	touch $(d0root)/d0geant/dzero/store_mcen.for
	touch $(d0root)/d0geant/cal/stpcal.for
	touch $(d0root)/d0geant/cdc/stpcdc.for
	touch $(d0root)/d0geant/d0/stpd0.for
	touch $(d0root)/d0geant/fdc/stpfdc.for
	touch $(d0root)/d0geant/lv0/stplv0.for
	touch $(d0root)/d0geant/muo/stpmuo.for
	touch $(d0root)/d0geant/muo/stpmuo_v1.for
	touch $(d0root)/d0geant/samus/stpsam.for
	touch $(d0root)/d0geant/trd/stptrd.for
	touch $(d0root)/d0geant/vtx/stpvtx.for
	touch $(d0root)/d0geant/dzero/stswch.for
	touch $(d0root)/d0geant/utl/stzdib.for
	touch $(d0root)/d0geant/utl/stzdiv.for
	touch $(d0root)/d0geant/utl/stzdv1.for
	touch $(d0root)/d0geant/dzero/super_gswt.for
	touch $(d0root)/d0geant/fdc/svdlay.for
	touch $(d0root)/d0geant/cal/tevcal.for
	touch $(d0root)/d0geant/cdc/tevcdc.for
	touch $(d0root)/d0geant/fdc/tevfdc.for
	touch $(d0root)/d0geant/lv0/tevlv0.for
	touch $(d0root)/d0geant/muo/tevmuo.for
	touch $(d0root)/d0geant/samus/tevsam.for
	touch $(d0root)/d0geant/trd/tevtrd.for
	touch $(d0root)/d0geant/vtx/tevvtx.for
	touch $(d0root)/d0geant/trd/tlayer.for
	touch $(d0root)/d0geant/trd/trabsl.for
	touch $(d0root)/d0geant/zebio/trafra.for
	touch $(d0root)/d0geant/trd/trchan.for
	touch $(d0root)/d0geant/trd/trdblk.for
	touch $(d0root)/d0geant/trd/trdfmy.for
	touch $(d0root)/d0geant/trd/trdshp.for
	touch $(d0root)/d0geant/trd/trdtim.for
	touch $(d0root)/d0geant/trd/trdunp.for
	touch $(d0root)/d0geant/trd/trdwde.for
	touch $(d0root)/d0geant/trd/trdyld.for
	touch $(d0root)/d0geant/zebio/trgpmc.for
	touch $(d0root)/d0geant/trd/trhigh.for
	touch $(d0root)/d0geant/cal/trkcal.for
	touch $(d0root)/d0geant/cdc/trkcdc.for
	touch $(d0root)/d0geant/fdc/trkfdc.for
	touch $(d0root)/d0geant/lv0/trklv0.for
	touch $(d0root)/d0geant/muo/trkmuo.for
	touch $(d0root)/d0geant/samus/trksam.for
	touch $(d0root)/d0geant/trd/trktrd.for
	touch $(d0root)/d0geant/vtx/trkvtx.for
	touch $(d0root)/d0geant/trd/trnumb.for
	touch $(d0root)/d0geant/trd/trtabl.for
	touch $(d0root)/d0geant/caltower/ucindx.for
	touch $(d0root)/d0geant/dzero/uginit.for
	touch $(d0root)/d0geant/dzero/uglast.for
	touch $(d0root)/d0geant/muo/use_mulib.for
	touch $(d0root)/d0geant/muo/use_muonlibrary.for
	touch $(d0root)/d0geant/zebio/use_shlb.for
	touch $(d0root)/d0geant/zebio/use_showerlibrary.for
	touch $(d0root)/d0geant/dzero/vdzero.for
	touch $(d0root)/d0geant/vtx/vlanda.for
	touch $(d0root)/d0geant/cal/volord.for
	touch $(d0root)/d0geant/utl/volpos.for
	touch $(d0root)/d0geant/utl/volpoz.for
	touch $(d0root)/d0geant/vtx/vtxpul.for
	touch $(d0root)/d0geant/vtx/vtxwrt.for
	touch $(d0root)/d0geant/zebio/wrzeb.for
	touch $(d0root)/d0geant/trd/xinter.for
	touch $(d0root)/d0geant/trd/xlum.for
	touch $(d0root)/d0geant/trd/xrad.for
	touch $(d0root)/d0geant/vtx/zadped.for
	touch $(d0root)/d0geant/cdc/zcnvrt.for
	touch $(d0root)/d0geant/zebio/zebfin.for
	touch $(d0root)/d0geant/zebio/zebini.for
	touch $(d0root)/d0geant/zebio/zebout.for
	touch $(d0root)/d0geant/cdc/zersup.for
	touch $(d0root)/d0geant/vtx/zinduc.for
	touch $(d0root)/d0geant/vtx/zsinit.for
	touch $(d0root)/d0geant/vtx/zstrht.for
	touch $(d0root)/d0geant/vtx/zstrip.for
	touch $(d0root)/d0geant/vtx/zstrno.for
