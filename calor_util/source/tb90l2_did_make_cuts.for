      FUNCTION tb90l2_did_make_cuts(cut,module,eta,phi)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Puts eta and phi limits relative to beam for each
C-   cal module.
C-
C-   Returned value  : TRUE if within cuts
C-   Inputs  :
C-      cut     -       narrow or wide cuts ( 1 or 2)
C-      module  -       calorimeter module
C-      eta,phi -       coordinate of hit
C-   Outputs : none
C-   Controls: none
C-
C-   Created  20-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL tb90l2_did_make_cuts
      LOGICAL tb90l2_did_make_cuts_init
      CHARACTER*20 tb90l2_get_module_name
      INCLUDE 'd0$params:tb90l2_modules.def'
      CHARACTER*(*) rcp_bank
      INTEGER cut, module
      REAL    eta,phi                   ! location of hit
      REAL    eta1,phi1                 ! dummy for init entry
C
C ****  eta_phi_range holds cut ranges index 1 - module
C ****                                       2 - 1=eta, 2=phi
C ****                                       3 - 1=narrow, 2=wide
C
      REAL    eta_phi_range(NUM_OF_MODULES,2,2)
      REAL    cut_array(4)              ! store vals as read from rcp
      CHARACTER*20 range_name           !
      INTEGER len_range_name            ! length of module name
C
C ****  eta_phi_limits holds limits indicis  1 - module
C ****                                       2 - 1=eta, 2=phi
C ****                                       3 - 1=low, 2= upper limit
C ****                                       4 - 1=narrow,2=wide cut
C
      REAL    eta_phi_limits(1:NUM_OF_MODULES,2,2,2)
      LOGICAL mod_diff_cuts             ! true if mods have diff eta,phis
      REAL    eta_cuts_min(2), eta_cuts_max(2)
      REAL    phi_cuts_min(2), phi_cuts_max(2) ! lims if not diff cuts
      REAL    temp
      INTEGER ier,i,j
      SAVE eta_phi_limits
C----------------------------------------------------------------------
      IF (mod_diff_cuts) THEN
        tb90l2_did_make_cuts = (
     &    (eta .GE. eta_phi_limits(module,1,1,cut))
     &           .AND. (eta .LE. eta_phi_limits(module,1,2,cut))
     &       .AND. (phi .GE. eta_phi_limits(module,2,1,cut))
     &           .AND. (phi .LE. eta_phi_limits(module,2,2,cut)) )
      ELSE
        tb90l2_did_make_cuts = (
     &          (eta .GE. eta_cuts_min(cut))
     &                .AND. (eta .LE. eta_cuts_max(cut))
     &    .AND. (phi .GE. phi_cuts_min(cut))
     &                .AND. (phi .LE. phi_cuts_max(cut)) )
      ENDIF
      RETURN
C#######################################################################
      ENTRY tb90l2_did_make_cuts_init(rcp_bank,eta1,phi1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads in eta phi limits from rcp bank.
C-
C-   Inputs  : eta1, phi1 of beam
C-   Outputs : none
C-   Controls: rcp_bank - which bank to read info from
C-
C-   Created  20-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
C
C ****  Get eta, phi ranges about beam for each module. If FH is 0. all ranges
C ****  are set the same as EM
C
      CALL ezpick(rcp_bank)
      CALL ezget('TB90L2EM_RANGE',cut_array,ier)    ! always get EM cuts
      eta_phi_range(TB90L2EM,1,1) = cut_array(1)
      eta_phi_range(TB90L2EM,2,1) = cut_array(2)
      eta_phi_range(TB90L2EM,1,2) = cut_array(3)
      eta_phi_range(TB90L2EM,2,2) = cut_array(4)
      CALL ezget('TB90L2FH_RANGE',cut_array,ier)    ! check FH range
      IF ( cut_array(1) .NE. 0. ) THEN
        mod_diff_cuts = .true.
        eta_phi_range(TB90L2FH,1,1) = cut_array(1)  ! d0 FH if not 0
        eta_phi_range(TB90L2FH,2,1) = cut_array(2)
        eta_phi_range(TB90L2FH,1,2) = cut_array(3)
        eta_phi_range(TB90L2FH,2,2) = cut_array(4)
        DO i = 3 , NUM_OF_MODULES
          range_name = tb90l2_get_module_name(i,len_range_name)
          CALL ezget(range_name(1:len_range_name),cut_array,ier)
          IF ( ier .NE. 0 ) THEN
            CALL errmsg('EZGET','TB90L2_CALOR_HIST_SETUP',
     &          'Unable to get range array','W')
          ENDIF
          eta_phi_range(i,1,1) = cut_array(1)
          eta_phi_range(i,2,1) = cut_array(2)
          eta_phi_range(i,1,2) = cut_array(3)
          eta_phi_range(i,2,2) = cut_array(4)
        ENDDO
      ELSE
        CALL ezget('TB90L2EM_RANGE',cut_array,ier)  ! refetch EM
        mod_diff_cuts = .false.
        DO i = 2 , NUM_OF_MODULES
          eta_phi_range(i,1,1) = cut_array(1)
          eta_phi_range(i,2,1) = cut_array(2)
          eta_phi_range(i,1,2) = cut_array(3)
          eta_phi_range(i,2,2) = cut_array(4)
        ENDDO
      ENDIF
      CALL ezrset
C
C ****  Calculate eta and phi limits around beam. i counts over modules,
C ****  j is narrow or wide cuts. For each module, first do eta lower and upper
C ****  limit then phi upper and lower limits, then repeat for wide cut. If all
C ****  the cuts are slaved to EM then just set eta_cuts_min...
C
      IF ( mod_diff_cuts ) THEN
        DO i = 1 , NUM_OF_MODULES         ! loop over modules
          DO j = 1 , 2                    ! narrow and wide
            temp = eta_phi_range(i,1,j) * 0.5     ! only do mul once
            eta_phi_limits(i,1,1,j) = eta1 - temp
            eta_phi_limits(i,1,2,j) = eta1 + temp
            temp = eta_phi_range(i,2,j) * 0.5     ! do same for phi
            eta_phi_limits(i,2,1,j) = phi1 - temp
            eta_phi_limits(i,2,2,j) = phi1 + temp
          ENDDO
        ENDDO
      ELSE
        DO j = 1 , 2
          temp = eta_phi_range(TB90L2EM,1,j) * 0.5     ! only do mul once
          eta_cuts_min(j) = eta1 - temp
          eta_cuts_max(j) = eta1 + temp
          temp = eta_phi_range(TB90L2EM,2,j) * 0.5
          phi_cuts_min(j) = phi1 - temp
          phi_cuts_max(j) = phi1 + temp
        ENDDO
      ENDIF
      RETURN
      END
