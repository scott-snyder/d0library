#! /bin/csh -f
#-------------------------------------------------------------------------
#
# define geometry stpfiles
#
#-------------------------------------------------------------------------
#
ln -sf `uff $d0stp/gen_stpfile.dat` gen_stpfile  
ln -sf `uff $d0stp/cal_stpfile.dat` cal_stpfile 
ln -sf `uff $d0stp/cdc_stpfile.dat` cdc_stpfile  
ln -sf `uff $d0stp/fdc_stpfile.dat` fdc_stpfile  
ln -sf `uff $d0stp/trd_stpfile.dat` trd_stpfile  
ln -sf `uff $d0stp/vtx_stpfile.dat` vtx_stpfile  
ln -sf `uff $d0stp/lv0_stpfile.dat` lv0_stpfile  
ln -sf `uff $d0stp/sam_stpfile.dat` sam_stpfile  
ln -sf `uff $d0stp/muo_stpfile_1.dat` muo_stpfile  
#
#-------------------------------------------------------------------------
#
# define trd files
#
#-------------------------------------------------------------------------
#        trd xray distribution
ln -sf `uff $d0d0geant/data/xraydist.dat` XRAYDIST  
#        trd delta ray distribution
ln -sf `uff $d0d0geant/data/deldist.dat` DELDIST
#       trd shape distribution
ln -sf `uff $d0d0geant/data/shape_trd.dat` SHAPE_TRD  
#       trd xray spectrum
ln -sf `uff $d0d0geant/data/xrspect.dat` XRSPECT  
#       trd cathode strip weights
ln -sf `uff $d0d0geant/data/csweight.dat` TRD_STRIPS  
#
#
#------------------------------------------------------------------------
#
# define input, output 
#
#------------------------------------------------------------------------
#
ln -s /path/to/your/input/isajet_file.isa 	fort.31
ln -s /path/to/your/ffread_file			fort.9
