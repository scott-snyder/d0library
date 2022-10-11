      FUNCTION LV0DMP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dump LV0 Banks using dump facility
C-
C-   Returned value  : TRUE
C-
C-   ENTRY LV0DDF  read in banks to dump from LEVEL0_RCP file
C-                 called by LV0INI
C-   Returned value  : TRUE if OK
C-   Inputs  : SRCP
C-
C-   based on CALDMP
C-   Created  10-AUG-1992   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL LV0DMP,LV0DDF
C
      INTEGER NCHECK
      PARAMETER (NCHECK=5)
      CHARACTER*4 CHECKS(NCHECK)
C
      INTEGER MAXDMP
      PARAMETER( MAXDMP = 20 )
      CHARACTER*4 DUMPS(MAXDMP),CBANK
      CHARACTER*64 MSG
      INTEGER BANKS(MAXDMP)
C
      INTEGER I,K
      INTEGER NUMDMP,IER
C
      LOGICAL EZERROR
      EXTERNAL EZERROR
C
C  checks must match the externals
C
      EXTERNAL PRLV0H,PRL0AD,PRL0SC,PRL0VX,PRPLV0
      DATA CHECKS /'LV0H','L0AD','L0SC','L0VX','PLV0'/
C----------------------------------------------------------------------
C
C ****  DO THIS EVERY EVENT
C
      LV0DMP=.TRUE.
C
      IF(NUMDMP.EQ.0) GOTO 999    ! nothing to dump
      CALL DMPANY('LV0H',PRLV0H)
      CALL DMPANY('L0AD',PRL0AD)
      CALL DMPANY('L0SC',PRL0SC)
      CALL DMPANY('L0VX',PRL0VX)
      CALL DMPANY('PLV0',PRPLV0)
C
      RETURN
C
C
      ENTRY LV0DDF()
C
      LV0DDF=.TRUE.
      CALL EZPICK('LEVEL0_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('LEVEL0','LV0DMP','LEVEL0_RCP not found.','W')
      ELSE
        CALL EZGET_i('NUM_DUMPS',NUMDMP,IER)   ! get number of banks
        IF(NUMDMP.LE.MAXDMP)THEN
          CALL EZGET('DUMP_BANKS',BANKS,IER) ! get list of banks
        ELSE
          CALL ERRMSG('LEVEL0','LV0DMP',
     &      'DUMP REQUEST EXCEEDS MAXIMUM ALLOWED ','W')
          NUMDMP = 0
          LV0DDF=.FALSE.
          CALL EZRSET
          GOTO 999                 ! failed
        ENDIF
        CALL EZRSET
      ENDIF
C
C ****  Supply banks to dump facility
C
      DO 100 I = 1,NUMDMP
C
        CALL UHTOC(BANKS(I),4,DUMPS(I),4)
C
C           check there is a call to DMPANY for this bank
        DO 200 K = 1,NCHECK
          IF(DUMPS(I).EQ.CHECKS(K)) GO TO 201
  200   CONTINUE
        WRITE(MSG,101) DUMPS(I)
        CALL ERRMSG('LEVEL0','LV0DMP',
     &          MSG,'W')
        GO TO 100
  201   CONTINUE
C
        CALL DMPBNK(DUMPS(I),.TRUE.)   ! make bank known to DUMP facility
C
  100 CONTINUE
  101 FORMAT(' DUMP FOR BANK ',A4,' NOT CODED ')
C-----------------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
