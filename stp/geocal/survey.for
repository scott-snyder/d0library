      PROGRAM SURVEY
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO ADD CALORIMETER SURVEY INFORMATION
C-                         TO THE GEOMETRY ZEBRA DATABASE
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   2-MAR-1991   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSCAL.LINK'
      INCLUDE 'D0$LINKS:IZCLIN.LINK'
      INCLUDE 'D0$LINKS:IZCGEH.LINK'
      INTEGER OK, IERR, LUN, ERROR, LSTPC
      PARAMETER (LUN = 81)
C
      CALL MZEBRA(0)
      CALL INZSTP                    ! initialize and read CAL_STPFILE
      CALL CAISTP('CAL_STPFILE',OK)
      CALL MZLINK(IXSTP,'/CLINKS/',LQSTPH,LQCLYR,LQSTPH)     ! protect
                                        ! link area /CLINKS/
C
      CALL INRCP('CC_SURVEY_RCP',IERR)   ! read in CC_SURVEY.RCP
      IF( IERR .NE. 0 ) THEN
        CALL ERRMSG('NO_CC_SURVEY_RCP','SURVEY',
     +  'can not open CC_SURVEY_RCP','S')
        STOP 600
      END IF
C
      CALL INRCP('CAL_SURVEY_MARKERS_RCP', IERR)  ! read in theo markers
      IF( IERR .NE. 0) THEN
        CALL ERRMSG('NO CAL_SURVEY_MARKERS_RCP','SURVEY',
     +  'can not open RCP file','S')
        STOP 611
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
      CALL CC_SURVEY_MODULE          ! fit modules to survey
                                     ! measurements 
C
      CALL CC_SURVEY_ANALYZE         ! make plots 
C
      CALL INRCP('ECN_SURVEY_RCP',IERR)  ! read in ECN_SURVEY.RCP
      IF( IERR .NE. 0 ) THEN
        CALL ERRMSG('NO_ECN_SURVEY_RCP','SURVEY',
     +  'can not open ECN_SURVEY_RCP','S')
        STOP 601
      END IF
C
      CALL ECN_SURVEY_IN_ZEBRA       ! convert RCP file into a ZEBRA bank
                                     ! structure
C
      CALL ECN_THEO_GEO_BANKS        ! put theoretical positions of ECN modules
                                     ! from RCP into bank structure and put
                                     ! reference links between them
C
      CALL ECN_SURVEY_MODULE         ! fit modules to survey measurements
C
      CALL ECN_SURVEY_ANALYZE        ! make plots
C
C
      CALL INRCP('ECS_SURVEY_RCP',IERR)  ! read in ECS_SURVEY.RCP
      IF( IERR .NE. 0 ) THEN
        CALL ERRMSG('NO_ECS_SURVEY_RCP','SURVEY',
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
      CALL ECS_SURVEY_MODULE         ! fit modules to survey measurements
C
      CALL ECS_SURVEY_ANALYZE        ! make plots
C
      LSTPC = LC(LSTPH-IZSTPC)       ! write out new CAL_STPFILE
      LSCAL = LC(LSTPC-IZSCAL)       ! with survey info included
      LCGEH = LC(LSCAL-IZCGEH)
      LQCLIN = LC(LCGEH-IZCLIN)
      CALL MZLOGL(IXSTP,2)
      CALL ZZOPEN (LUN, 'NEW_CAL_STPFILE',ERROR,'OUTPUT')
      CALL FZLOGL(LUN,3)
      CALL DZSHOW('STP WITH CLIN',IXSTP,LSCAL,'BLV',0,0,0,0)
      CALL DZVERI('CLIN IN STP',IDVSTP,'L')
      CALL FZOUT  (LUN,IDVSTP,LSCAL,1,'P',1,0,0)
      CALL ZZCLOS (LUN,ERROR,'OUTPUT')
      STOP 1
C----------------------------------------------------------------------
      END
