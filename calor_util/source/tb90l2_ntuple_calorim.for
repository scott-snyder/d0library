      SUBROUTINE tb90l2_ntuple_calorim(cut,module,layer,eta,phi,energy)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Accumulates values for filling the calorimeter words
C-   of the ntuple. If desired, simple clustering is done for calorimeter
C-   modules.
C-
C-   Inputs  :
C-      module [I]  -  module number of module ( from tb90l2_modules.def)
C-      layer [I]   -  layer of module
C-      eta,phi [R] -  eta,phi coor of cell
C-      energy [R]  -  energy in cell
C-   Outputs : none
C-   Controls: cut [I] -  if cut = NO_E_CUT  --  sum all energy in module
C-                           cut = NARROW_E_CUT - sum energy in  narrow lims
C-                           cut = WIDE_E_CUT  - sum energy in wide lims
C-
C-   Created  28-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER cut, module, layer
      REAL    eta,phi,energy
      EXTERNAL tb90l2_did_make_cuts
      LOGICAL tb90l2_did_make_cuts
      INCLUDE 'd0$params:tb90l2_ntuple.def'
      INCLUDE 'd0$params:tb90l2_modules.def'
      INCLUDE 'd0$inc:zebcom.inc'
      INTEGER gzcaep
      EXTERNAL gzcaep
      INTEGER lcaep
      LOGICAL do_simple_clustering
      REAL    energ_total, em_total, fh_total
      REAL    ch_total, mh_total, oh_total
      SAVE  do_simple_clustering
      SAVE    energ_total, em_total, fh_total
      SAVE    ch_total, mh_total, oh_total
C
C ****  set up structure for storing energies. Make 2. 1 for cal modules and
C ****  the other for icd and mg, to not waiste so much memory.
C ****  calor_modules: indicies: 1      -   modules number
C ****                           2      -   layer number
C ****                           3      -   1 - energy_max
C ****                                      2 - energy_total
C ****                                            clusters: ener in cluster
C ****                                      3 - eta of max energy
C ****                                           clusters: corrected
C ****                                      4 - phi of max energy
C ****                                           clusters: corrected
C ****         when clustering is performed, energy_total is energy of the
C ****         3x3 cluster around max energy pad, eta,phi have ln corrections
C ****
C ****  icd_and_mg: indicies:   1       -  module number
C ****                          2       - same as calor_modules index 3
C
      REAL    calor_modules(1:LAST_LYRD_MOD,1:MAX_LYR_NUM,1:4)
      REAL    ntuple(CALORIM_BGN_O+1:CALORIM_END)!calorim words
      REAL    icd_and_mg(LAST_LYRD_MOD+1:NUM_OF_MODULES,4)
      LOGICAL did_pass_cut              ! true if event passed energy cut
      INTEGER offset, base              ! used for counters in fillin ntuple
      INTEGER i
C----------------------------------------------------------------------
      IF ( cut .EQ. no_e_cut ) THEN
        did_pass_cut = .true.
      ELSEIF ( tb90l2_did_make_cuts(cut,module,eta,phi) ) then
        did_pass_cut = .true.
      ELSE
        did_pass_cut = .false.
      ENDIF
      IF ( did_pass_cut ) THEN
        IF ( module .LE. last_lyrd_mod ) THEN        ! in calorim module
          calor_modules(module,layer,2) = calor_modules(module,layer,2)
     &      + energy                    ! sum energies
          IF ( energy .GT. calor_modules(module,layer,1) ) then ! new max
            calor_modules(module,layer,1) = energy
            calor_modules(module,layer,3) = eta
            calor_modules(module,layer,4) = phi
          ENDIF
        ELSE                            ! in icd or massless gap
          icd_and_mg(module,2) = icd_and_mg(module,2) + energy
          IF ( energy .GT. icd_and_mg(module,1) ) then  ! new max
            icd_and_mg(module,1) = energy
            icd_and_mg(module,3) = eta
            icd_and_mg(module,4) = phi
          ENDIF
        ENDIF
      ENDIF
      RETURN
C#######################################################################
      ENTRY tb90l2_ntuple_simple_clustering
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This is a hook to call tb90l2_ntuple_cluster.
C-   It must be called in the exm_do_analysis hook of examine because it
C-   depends on the ptcaep array being filled.
C-
C-   Since ntuple_cluster replace calor_modules elements, calorimeter total
C-   energies should be saved before calor_modules is modified.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   8-AUG-1991   James Richardson
C-
C----------------------------------------------------------------------
      do_simple_clustering = .true.
      energ_total = calor_modules(TB90L2EM,1,2)+
     &    calor_modules(TB90L2EM,2,2)+calor_modules(TB90L2EM,3,2)+
     &    calor_modules(TB90L2EM,4,2) + ! end of EM
     &    calor_modules(TB90L2FH,1,2)+calor_modules(TB90L2FH,2,2)+
     &    calor_modules(TB90L2FH,3,2) +         ! end of FH
     &    calor_modules(TB90L2CH,1,2) +         ! end of CH
     &    calor_modules(TB90L2MH,1,2)+calor_modules(TB90L2MH,2,2)+
     &    calor_modules(TB90L2MH,3,2)+calor_modules(TB90L2MH,4,2)+
     &    calor_modules(TB90L2MH,5,2) +         ! end of MH
     &    calor_modules(TB90L2OH,1,2)+calor_modules(TB90L2OH,2,2)+
     &    calor_modules(TB90L2OH,3,2)   ! end of OH, end of cal mods
      em_total = calor_modules(TB90L2EM,1,2)+
     &    calor_modules(TB90L2EM,2,2)+calor_modules(TB90L2EM,3,2)+
     &    calor_modules(TB90l2EM,4,2)
      fh_total = calor_modules(TB90L2FH,1,2)+
     &  calor_modules(TB90L2FH,2,2)+calor_modules(TB90L2FH,3,2)
      ch_total = calor_modules(TB90L2CH,1,2)
      mh_total = calor_modules(TB90L2MH,1,2)+
     &  calor_modules(TB90L2MH,2,2)+calor_modules(TB90L2MH,3,2)+
     &  calor_modules(TB90L2MH,4,2)+calor_modules(TB90L2MH,5,2)
      oh_total = calor_modules(TB90L2OH,1,2)+
     &  calor_modules(TB90L2OH,2,2)+calor_modules(TB90L2OH,3,2)
      CALL tb90l2_ntuple_cluster(calor_modules)
      RETURN
C#######################################################################
      ENTRY tb90l2_ntuple_calorim_fill(ntuple)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : moves appropriate values into ntuple
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  28-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      base = calorim_bgn_o               ! offset of calorim words
C
C ****  get number of channels from caep bank
C
      lcaep = gzcaep()
      IF ( lcaep .EQ. 0 ) THEN
        ntuple(base+1) = 0.    ! no caep bank (should never get here)
      ELSE
        ntuple(base+1) = float(iq(lcaep+3))
      ENDIF
      base = base + 1
C
C ****  do layered module totals
C
      IF ( do_simple_clustering ) THEN
        ntuple(base+1) = energ_total
        ntuple(base+2) = em_total
        ntuple(base+3) = fh_total
        ntuple(base+4) = ch_total
        ntuple(base+5) = mh_total
        ntuple(base+6) = oh_total
      ELSE
        ntuple(base+1) = calor_modules(TB90L2EM,1,2)+
     &    calor_modules(TB90L2EM,2,2)+calor_modules(TB90L2EM,3,2)+
     &    calor_modules(TB90L2EM,4,2) + ! end of EM
     &    calor_modules(TB90L2FH,1,2)+calor_modules(TB90L2FH,2,2)+
     &    calor_modules(TB90L2FH,3,2) +         ! end of FH
     &    calor_modules(TB90L2CH,1,2) +         ! end of CH
     &    calor_modules(TB90L2MH,1,2)+calor_modules(TB90L2MH,2,2)+
     &    calor_modules(TB90L2MH,3,2)+calor_modules(TB90L2MH,4,2)+
     &    calor_modules(TB90L2MH,5,2) +         ! end of MH
     &    calor_modules(TB90L2OH,1,2)+calor_modules(TB90L2OH,2,2)+
     &    calor_modules(TB90L2OH,3,2)   ! end of OH, end of cal mods
        ntuple(base+2) = calor_modules(TB90L2EM,1,2)+
     &    calor_modules(TB90L2EM,2,2)+calor_modules(TB90L2EM,3,2)+
     &    calor_modules(TB90l2EM,4,2)
        ntuple(base+3) = calor_modules(TB90L2FH,1,2)+
     &  calor_modules(TB90L2FH,2,2)+calor_modules(TB90L2FH,3,2)
        ntuple(base+4) = calor_modules(TB90L2CH,1,2)
        ntuple(base+5) = calor_modules(TB90L2MH,1,2)+
     &  calor_modules(TB90L2MH,2,2)+calor_modules(TB90L2MH,3,2)+
     &  calor_modules(TB90L2MH,4,2)+calor_modules(TB90L2MH,5,2)
        ntuple(base+6) = calor_modules(TB90L2OH,1,2)+
     &  calor_modules(TB90L2OH,2,2)+calor_modules(TB90L2OH,3,2)
      ENDIF
      base = base + 6
C
C ****  do layer energy,eta, and phi.
C
      DO i = 1 , TB90L2EM_LYRS
        offset = base + ( i - 1 ) * 3
        ntuple(offset+1) = calor_modules(TB90L2EM,i,2)  ! energy
        ntuple(offset+2) = calor_modules(TB90L2EM,i,3)  ! eta of max
        ntuple(offset+3) = calor_modules(TB90L2EM,i,4)  ! phi of max
      ENDDO
      base = offset + 3               ! point to last word
      DO i = 1 , TB90L2FH_LYRS
        offset = base + ( i - 1) * 3
        ntuple(offset+1) = calor_modules(TB90L2FH,i,2)  ! energy
        ntuple(offset+2) = calor_modules(TB90L2FH,i,3)  ! eta of max
        ntuple(offset+3) = calor_modules(TB90L2FH,i,4)  ! phi of max
      ENDDO
      base = offset + 3               ! point to last word
      DO i = 1 , TB90L2CH_LYRS
        offset = base + ( i - 1) * 3
        ntuple(offset+1) = calor_modules(TB90L2CH,i,2)  ! energy
        ntuple(offset+2) = calor_modules(TB90L2CH,i,3)  ! eta of max
        ntuple(offset+3) = calor_modules(TB90L2CH,i,4)  ! phi of max
      ENDDO
      base = offset + 3               ! point to last word
      DO i = 1 , TB90L2MH_LYRS
        offset = base + ( i - 1) * 3
        ntuple(offset+1) = calor_modules(TB90L2MH,i,2)  ! energy
        ntuple(offset+2) = calor_modules(TB90L2MH,i,3)  ! eta of max
        ntuple(offset+3) = calor_modules(TB90L2MH,i,4)  ! phi of max
      ENDDO
      base = offset + 3               ! point to last word
      DO i = 1 , TB90L2OH_LYRS
        offset = base + ( i - 1) * 3
        ntuple(offset+1) = calor_modules(TB90L2OH,i,2)  ! energy
        ntuple(offset+2) = calor_modules(TB90L2OH,i,3)  ! eta of max
        ntuple(offset+3) = calor_modules(TB90L2OH,i,4)  ! phi of max
      ENDDO
      base = offset + 3               ! point to last word
C
C ****  do the icd, ccmg, ecmg, and icdmg in that order
C
      ntuple(base+1) = icd_and_mg(TB90L2ICD,2)  ! energy
      ntuple(base+2) = icd_and_mg(TB90L2ICD,3)  ! eta of max
      ntuple(base+3) = icd_and_mg(TB90L2ICD,4)  ! phi of max
      base = base + 3
      ntuple(base+1) = icd_and_mg(TB90L2CCMG,2)  ! energy
      ntuple(base+2) = icd_and_mg(TB90L2CCMG,3)  ! eta of max
      ntuple(base+3) = icd_and_mg(TB90L2CCMG,4)  ! phi of max
      base = base + 3
      ntuple(base+1) = icd_and_mg(TB90L2ECMG,2)  ! energy
      ntuple(base+2) = icd_and_mg(TB90L2ECMG,3)  ! eta of max
      ntuple(base+3) = icd_and_mg(TB90L2ECMG,4)  ! phi of max
      base = base + 3
      ntuple(base+1) = icd_and_mg(TB90L2ICDMG,2)  ! energy
      ntuple(base+2) = icd_and_mg(TB90L2ICDMG,3)  ! eta of max
      ntuple(base+3) = icd_and_mg(TB90L2ICDMG,4)  ! phi of max
      RETURN
C#######################################################################
      ENTRY tb90l2_ntuple_calorim_reset
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : reset ntuple words to 0 for each event
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  28-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      CALL vzero(calor_modules(1,1,1),LAST_LYRD_MOD*MAX_LYR_NUM*4)
      CALL vzero(icd_and_mg,(NUM_OF_MODULES-LAST_LYRD_MOD)*4)
      do_simple_clustering = .false.
      RETURN
      END
