      FUNCTION VTRDMP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dump FDC Banks using dump facility
C-
C-   Returned value  : TRUE 
C-
C-   ENTRY VTRDDF  read in banks to dump from VTRAKS_RCP file
C-                 called by VTRINI
C-   Returned value  : TRUE if OK
C-   Inputs  : SRCP
C-
C-   based on CALDMP
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NCHECK
      PARAMETER (NCHECK=3)
      CHARACTER*4 CHECKS(NCHECK)
      LOGICAL VTRDMP,VTRDDF
      INTEGER MAXDMP
      PARAMETER( MAXDMP = 20 )
      CHARACTER*4 DUMPS(MAXDMP),CBANK
      CHARACTER*64 MSG
      INTEGER BANKS(MAXDMP)
      INTEGER I,K
      INTEGER NUMDMP,IER
C          checks must match the externals
      EXTERNAL PRVTXH,PRVSEC,PRVTXT
      DATA CHECKS
     X     /'VTXH','VSEC','VTXT'/
C----------------------------------------------------------------------
C
C ****  DO THIS EVERY EVENT
C
      VTRDMP=.TRUE.
C
      IF(NUMDMP.EQ.0) GOTO 999    ! nothing to dump
      CALL DMPANY('VTXH',PRVTXH)
      CALL DMPANY('VSEC',PRVSEC)
      CALL DMPANY('VTXT',PRVTXT)
C
      RETURN
C
C
      ENTRY VTRDDF()
C
      VTRDDF=.TRUE.
      CALL EZPICK('VTRAKS_RCP')       
      CALL EZGET('NUM_DUMPS',NUMDMP,IER)   ! get number of banks
      IF(NUMDMP.LE.MAXDMP)THEN
        CALL EZGET('DUMP_BANKS',BANKS,IER) ! get list of banks
      ELSE
        CALL ERRMSG('VTRAKS','VTRDMP',
     &      'DUMP REQUEST EXCEEDS MAXIMUM ALLOWED ','W')
        NUMDMP = 0
        VTRDDF=.FALSE.
        CALL EZRSET
        GOTO 999                 ! failed
      ENDIF
      CALL EZRSET
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
        CALL ERRMSG('VTRAKS','VTRDMP',
     &          MSG,'W')
        GO TO 100
  201   CONTINUE
C
        CALL DMPBNK(DUMPS(I),.TRUE.)   ! make bank known to DUMP facility
C
  100 CONTINUE
  999 RETURN
  101 FORMAT(' DUMP FOR BANK ',A4,' NOT CODED ')
      END
