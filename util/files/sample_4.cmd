! SAMPLE_4.CMS
indev   : disk              ! input from disk
invsn   : Job_name          ! usually used to label flag files etc.
inlabel : mc_data$hroot:[single_tracks]       ! input directory
outdev  : nl                ! output to null device 
outvsn  : none              ! unused (free to user)
outlabel: nl:               ! redundant output device
init    : noinit            ! unused
end:
!
DST_N_ELEC_50GEV.GEN_I
DST_N_MUON_50GEV.GEN_I
ELEC50M.GEN1
N_ELEC_50GEV.GEN_I
N_MUON_50GEV.GEN_I
N_PHOT_25GEV.GEN_I
N_PION_50GEV.GEN_I
PIONS50M.GEN1
STA_N_ELEC_50GEV.GEN_I
STA_N_MUON_50GEV.GEN_I
! end is marked by end of file
