      LOGICAL FUNCTION PYTHRA_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ISAUNT.INC'
C
      INCLUDE 'D0$INC:ITAPES.INC'
C
      INTEGER IEVTAP
C
      CHARACTER*80 FILEVT,FILPRT
      INTEGER GTUNIT_ID,LOUT,IER
C
C----------------------------------------------------------------------
C
      PYTHRA_INI=.TRUE.
C
      CALL INZBRA                       ! INITIALIZE ZEBRA
      CALL INZCOM(2)                    ! Initialize /ZEBCOM/
      CALL INPAWC                       ! Initialize HBOOK4
C
C ****  Read parameters into RCP bank
C
      CALL INRCP ('PYTHIA_RCP',IER)
      IF(IER.NE.0)CALL ERRMSG('PYTHRA','PYTHRA',
     &  'CANNOT OPEN RCP FILE ','W')
C
      CALL EZPICK('PYTHIA_RCP')
      CALL EZGET('GTUNIT_ID',GTUNIT_ID,IER)
C
      CALL EZ_FILE_OPEN(GTUNIT_ID,'PRINT_FILE','OF',
     &  LOUT,FILPRT,IER)
      IF(IER.NE.0)THEN
        CALL ERRMSG('PYTHRA','PYTHRA',
     &  'COULD NOT OPEN PRINTOUT FILE','W')
      ENDIF
C
      CALL HOUTPU(LOUT)                 ! set HBOOK printout to LOUT
      CALL HERMES(LOUT)                 ! set HBOOK error message
C
C ****  OUTPUT FILE
C
      CALL EZ_FILE_OPEN(GTUNIT_ID,'OUTPUT_FILE','OU',
     &  ITEVT,FILEVT,IER)
      IF(IER.NE.0)THEN
        CALL ERRMSG('PYTHRA','PYTHRA',
     &  'COULD NOT OPEN EVENT OUTPUT FILE','W')
      ENDIF
      CALL FZFILE (ITEVT,0,'O')
C
      IEVTAP=-ITEVT     ! no provision for handling unstable particles
C
C          Setup for ZEBRA only
C
      ISUNIT=IABS(IEVTAP)
C
C  Set up flags to select type of output
C
C
      CALL EZSET('PRINT_UNIT',LOUT,IER)
      CALL EZSET('OUTPUT_UNIT',ITEVT,IER)
C
C ****  
C
      CALL PJET_RCP('PYTHIA_RCP')
C
      PYTHRA_INI = IER.EQ.0
C
  999 RETURN
      END
