      SUBROUTINE TB90L2_NTUPLE_CLUSTER(calor_modules)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Perform a simple clustering arount beam in
C-   calorimeter modules.
C-
C-   Inputs  :
C-      calor_modules  -  array filled by tb90l2_ntuple_calorim.for
C-   Outputs :
C-      calor_modules  -  refill array with cluster info
C-   Controls:
C-
C-   Created   5-AUG-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:TB90L2_NTUPLE.DEF'
      INCLUDE 'D0$PARAMS:TB90L2_MODULES.DEF'
      REAL    tb90l2_cluster_energy
      EXTERNAL tb90l2_cluster_energy
      REAL    calor_modules(1:LAST_LYRD_MOD,1:MAX_LYR_NUM,1:4)
      REAL    eta,phi
      INTEGER num_lyr                   ! num of layers in mod
      INTEGER eta_indx, phi_indx, lyr_indx
      INTEGER eta_limits(2),phi_limits(2)       ! eta,phi lims for cluster
      INTEGER i,j, ier
      REAL    eta_crctn,phi_crctn  ! correction to eta and phi positions
      REAL    eta_factor, phi_factor    ! multi fact to eta,phi corrections
C----------------------------------------------------------------------
C
C ****  do EM
C
      DO i = 1 , TB90L2EM_LYRS
        eta = calor_modules(TB90L2EM,i,3)
        phi = calor_modules(TB90L2EM,i,4)
        CALL tb90l2_get_software_indx
     &          (eta,phi,i,TB90L2EM,eta_indx,phi_indx,lyr_indx)
        CALL tb90l2_calc_eta_phi_limits
     &         (eta_indx,phi_indx,eta_limits,phi_limits)
        IF ( i .LT. 3 ) THEN
          calor_modules(TB90L2EM,i,2) =
     &            tb90l2_cluster_energy(lyr_indx,eta_limits,phi_limits)
          CALL tb90l2_position_correction
     &          (lyr_indx,eta_limits,phi_limits,eta_crctn,phi_crctn)
          IF ( eta_crctn .EQ. 0 ) THEN
            calor_modules(TB90L2EM,i,3) = calor_modules(TB90L2EM,i,3)
     &                 + 100.
          ELSE
            calor_modules(TB90L2EM,i,3) = calor_modules(TB90L2EM,i,3)
     &          + eta_factor * eta_crctn
          ENDIF
          IF ( phi_crctn .EQ. 0 ) THEN
            calor_modules(TB90L2EM,i,4) = calor_modules(TB90L2EM,i,4)
     &                 + 100.
          ELSE
            calor_modules(TB90L2EM,i,4) = calor_modules(TB90L2EM,i,4)
     &          + phi_factor * phi_crctn
          ENDIF
        ELSEIF ( i .EQ. 3 ) THEN
          calor_modules(TB90L2EM,3,2) =
     &            tb90l2_cluster_energy(3,eta_limits,phi_limits) +
     &            tb90l2_cluster_energy(4,eta_limits,phi_limits) +
     &            tb90l2_cluster_energy(5,eta_limits,phi_limits) +
     &            tb90l2_cluster_energy(6,eta_limits,phi_limits)
          CALL tb90l2_position_correction
     &           (lyr_indx,eta_limits,phi_limits,eta_crctn,phi_crctn)
          IF ( eta_crctn .EQ. 0 ) THEN
            calor_modules(TB90L2EM,i,3) = calor_modules(TB90L2EM,i,3)
     &                 + 100.
          ELSE
            calor_modules(TB90L2EM,i,3) = calor_modules(TB90L2EM,i,3)
     &          + eta_factor * eta_crctn
          ENDIF
          IF ( phi_crctn .EQ. 0 ) THEN
            calor_modules(TB90L2EM,i,4) = calor_modules(TB90L2EM,i,4)
     &                 + 100.
          ELSE
            calor_modules(TB90L2EM,i,4) = calor_modules(TB90L2EM,i,4)
     &          + phi_factor * phi_crctn
          ENDIF
        ELSE
          calor_modules(TB90L2EM,i,2) =
     &            tb90l2_cluster_energy(lyr_indx,eta_limits,phi_limits)
          CALL tb90l2_position_correction
     &                  (lyr_indx,eta_limits,phi_limits,
     &                          eta_crctn,phi_crctn)
          IF ( eta_crctn .EQ. 0 ) THEN
            calor_modules(TB90L2EM,i,3) = calor_modules(TB90L2EM,i,3)
     &                 + 100.
          ELSE
            calor_modules(TB90L2EM,i,3) = calor_modules(TB90L2EM,i,3)
     &          + eta_factor * eta_crctn
          ENDIF
          IF ( phi_crctn .EQ. 0 ) THEN
            calor_modules(TB90L2EM,i,4) = calor_modules(TB90L2EM,i,4)
     &                 + 100.
          ELSE
            calor_modules(TB90L2EM,i,4) = calor_modules(TB90L2EM,i,4)
     &          + phi_factor * phi_crctn
          ENDIF
        ENDIF
      ENDDO
C
C ****  do rest of modules in calorimeter
C
      DO i = 2 , LAST_LYRD_MOD      ! all other calor modules
        IF ( i .EQ. TB90L2FH ) THEN
          num_lyr = TB90L2FH_LYRS
        ELSEIF ( i .EQ. TB90L2CH ) THEN
          num_lyr = TB90L2CH_LYRS
        ELSEIF ( i .EQ. TB90L2MH ) THEN
          num_lyr = TB90L2MH_LYRS
        ELSEIF ( i .EQ. TB90L2OH_LYRS) THEN
          num_lyr = TB90L2OH_LYRS
        ENDIF
        DO j = 1 , num_lyr
          eta = calor_modules(i,j,3)
          phi = calor_modules(i,j,4)
          CALL tb90l2_get_software_indx
     &          (eta,phi,j,i,eta_indx,phi_indx,lyr_indx)
          CALL tb90l2_calc_eta_phi_limits
     &         (eta_indx,phi_indx,eta_limits,phi_limits)
          calor_modules(i,j,2) =
     &            tb90l2_cluster_energy(lyr_indx,eta_limits,phi_limits)
          CALL  tb90l2_position_correction
     &          (lyr_indx,eta_limits,phi_limits,eta_crctn,phi_crctn)
          IF ( eta_crctn .EQ. 0 ) THEN
            calor_modules(i,j,3) = calor_modules(i,j,3)+100.
          ELSE
            calor_modules(i,j,3) = calor_modules(i,j,3)
     &          + eta_factor * eta_crctn
          ENDIF
          IF ( phi_crctn .EQ. 0 ) THEN
            calor_modules(i,j,4) = calor_modules(i,j,4)+100.
          ELSE
            calor_modules(i,j,4) = calor_modules(i,j,4)
     &          + phi_factor * phi_crctn
          ENDIF
        ENDDO
      ENDDO
      RETURN
C#######################################################################
      ENTRY tb90l2_ntuple_cluster_init
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : reads eta and phi correction factor from rcp
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   8-AUG-1991   James Richardson
C-
C----------------------------------------------------------------------
      CALL ezpick('TB90L2_NTUPLE_RCP')
      CALL ezget ('ETA_SCALE_FACTOR',eta_factor,ier)
      CALL ezget ('PHI_SCALE_FACTOR',phi_factor,ier)
      CALL ezrset
      END
