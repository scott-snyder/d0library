! SAMPLE_3.CMD
indev   : TA81              ! Generic 8mm on FNAL 
invsn   : vgbs31            ! input visual label
inlabel : vgbs31            ! input internal label
outdev  : disk              ! output to disk
outvsn  : none              ! unused
outlabel: sys$login:        ! output directory
init    : noinit            ! unused
end:                        ! end of command input
! The rest of the file is a list of input files to process
DST_N_109001.GEN_I
DST_N_109002.GEN_I
DST_N_109003.GEN_I
DST_N_109004.GEN_I
DST_N_109005.GEN_I
DST_N_109006.GEN_I
DST_N_109007.GEN_I
! end is marked by end of file
