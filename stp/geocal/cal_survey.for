      SUBROUTINE CAL_SURVEY
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO ADD CALORIMETER SURVEY INFORMATION
C-                         TO THE GEOMETRY STP FILE.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   5-AUG-1992   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSCAL.LINK'
      INCLUDE 'D0$LINKS:IZCLIN.LINK'
      INCLUDE 'D0$LINKS:IZCGEH.LINK'
      INTEGER OK, IERR, LUN, ERROR, LSTPC,LOC,NFIT
      PARAMETER (LUN = 81)
C
      CALL EZLOC('CAWSTP_RCP',LOC)
      IF (LOC.EQ.0) CALL INRCP('CAWSTP_RCP',IERR)
      CALL EZPICK('CAWSTP_RCP')
      CALL EZGET('SURVEY_FIT_METHOD',NFIT,IERR)  ! 1= linear LSQ ; 2= chaba
      IF( IERR .NE. 0 ) THEN
        CALL ERRMSG('SURVEY_FIT_METHOD','CAL_SURVEY',
     +  'not in CAWSTP_RCP - use linear LSQ','W')
        NFIT = 1
      END IF
C
      CALL INRCP('CC_SURVEY_RCP',IERR)   ! read in CC_SURVEY.RCP
      IF( IERR .NE. 0 ) THEN
        CALL ERRMSG('NO_CC_SURVEY_RCP','CAL_SURVEY',
     +  'can not open CC_SURVEY_RCP','F')
      END IF
C
      CALL INRCP('CAL_SURVEY_MARKERS_RCP', IERR)  ! read in theo markers
      IF( IERR .NE. 0) THEN
        CALL ERRMSG('NO CAL_SURVEY_MARKERS_RCP','CAL_SURVEY',
     +  'can not open RCP file','F')
      END IF
C
      CALL CC_SURVEY_IN_ZEBRA        ! convert RCP file into a ZEBRA
                                     ! bank structure in division 3
C
      CALL CC_THEO_GEO_BANKS         ! put theoretical positions of
                                     ! CC modules from STP into similar
                                     ! bank structure and put reference
                                     ! links between them
C
      CALL CC_SURVEY_CYLINDER        ! fit cylinders to survey
                                     ! measurments for CC-EM, CC-FH,    
                                     ! CC-CH
C
      IF (NFIT.EQ.1) THEN
        CALL CC_SURVEY_MODULE          ! fit modules to survey measurements 
      ELSE IF(NFIT.EQ.2) THEN
        CALL CC_SURVEY_FIT             ! fit modules to survey with CHABA
      ELSE 
        CALL ERRMSG('NO FIT type IN CAWSTP_RCP','CAL_SURVEY',
     +  'TRY 1 or 2','F')
      END IF
C
      CALL INRCP('ECN_SURVEY_RCP',IERR)  ! read in ECN_SURVEY.RCP
      IF( IERR .NE. 0 ) THEN
        CALL ERRMSG('NO_ECN_SURVEY_RCP','CAL_SURVEY',
     +  'can not open ECN_SURVEY_RCP','F')
      END IF
C
      CALL ECN_SURVEY_IN_ZEBRA       ! convert RCP file into a ZEBRA bank
                                     ! structure
C
      CALL ECN_THEO_GEO_BANKS        ! put theoretical positions of ECN modules
                                     ! from RCP into bank structure and put
                                     ! reference links between them
C
      IF (NFIT.EQ.1) THEN
        CALL ECN_SURVEY_MODULE          ! fit modules to survey measurements 
      ELSE IF(NFIT.EQ.2) THEN
        CALL EC_SURVEY_FIT('NORTH') ! fit modules to survey with CHABA
      ELSE 
        CALL ERRMSG('NO FIT type IN CAWSTP_RCP','CAL_SURVEY',
     +  'TRY 1 or 2','F')
      END IF
C
      CALL INRCP('ECS_SURVEY_RCP',IERR)  ! read in ECS_SURVEY.RCP
      IF( IERR .NE. 0 ) THEN
        CALL ERRMSG('NO_ECS_SURVEY_RCP','CAL_SURVEY',
     +  'can not open ECS_SURVEY_RCP','S')
        STOP 601
      END IF
C
      CALL ECS_SURVEY_IN_ZEBRA       ! convert RCP file into a ZEBRA bank
                                     ! structure
C
      CALL ECS_THEO_GEO_BANKS        ! put theoretical positions of ECN modules
                                     ! from RCP into bank structure and put
                                     ! reference links between them
C
      IF (NFIT.EQ.1) THEN
        CALL ECS_SURVEY_MODULE          ! fit modules to survey measurements 
      ELSE IF(NFIT.EQ.2) THEN
        CALL EC_SURVEY_FIT('SOUTH') ! fit modules to survey with CHABA
      ELSE 
        CALL ERRMSG('NO FIT type IN CAWSTP_RCP','CAL_SURVEY',
     +  'TRY 1 or 2','F')
      END IF
C
      CALL CAL_SURVEY_NTUPLE ! make NTUPLES
C
      CALL CAL_ALIGNMENTS    ! apply supplementary corrections to SURVEY
                             ! measurements
      RETURN
C----------------------------------------------------------------------
      END
