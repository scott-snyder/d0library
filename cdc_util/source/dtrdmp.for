      FUNCTION DTRDMP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dump CDC Banks using dump facility
C-
C-   Returned value  : TRUE 
C-
C-   ENTRY DTRDDF  read in banks to dump from DTRAKS_RCP file called 
C-                 by DTRINI
C-   Returned value  : TRUE if OK
C-   Inputs  : SRCP
C-
C-   Created  28-JUN-1989   Qizhong Li-Demarteau  based on CALDMP
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZERROR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NCHECK
      PARAMETER (NCHECK=8)
      CHARACTER*4 CHECKS(NCHECK)
      LOGICAL DTRDMP,DTRDDF
      LOGICAL EZERROR
      INTEGER MAXDMP
      PARAMETER( MAXDMP = 20 )
      CHARACTER*4 DUMPS(MAXDMP),CBANK
      CHARACTER*64 MSG
      INTEGER BANKS(MAXDMP)
      INTEGER I,K
      INTEGER NUMDMP,IER
C          checks must match the externals
      EXTERNAL PRCDD2, PRCDCH, PRDSEC, PRDCDA
      EXTERNAL PRDTRH, PRDTRK, PRDTSG, PRDITR
      DATA CHECKS/'CDD2','CDCH','DSEC','DCDA',
     &            'DTRH','DTRK','DTSG','DITR'/
C----------------------------------------------------------------------
C
C ****  DO THIS EVERY EVENT
C
      DTRDMP=.TRUE.
C
      IF(NUMDMP.EQ.0) GOTO 999    ! nothing to dump
      CALL DMPANY('CDD2',PRCDD2)
      CALL DMPANY('CDCH',PRCDCH)
      CALL DMPANY('DSEC',PRDSEC)
      CALL DMPANY('DCDA',PRDCDA)
      CALL DMPANY('DTRH',PRDTRH)
      CALL DMPANY('DTRK',PRDTRK)
      CALL DMPANY('DTSG',PRDTSG)
      CALL DMPANY('DITR',PRDITR)
C
      RETURN
C
C
      ENTRY DTRDDF()
C
      DTRDDF=.TRUE.
      CALL EZPICK('DTRAKS_RCP')       
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','DTRDMP',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
      CALL EZGET('NUM_DUMPS',NUMDMP,IER)   ! get number of banks
      IF (NUMDMP .LE. 0) GOTO 999
      IF(NUMDMP.LE.MAXDMP)THEN
        CALL EZGET('DUMP_BANKS',BANKS,IER) ! get list of banks
      ELSE
        CALL ERRMSG('DTRAKS','DTRDMP',
     &      'DUMP REQUEST EXCEEDS MAXIMUM ALLOWED ','W')
        NUMDMP = 0
        DTRDDF=.FALSE.
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
        CALL ERRMSG('DTRAKS','DTRDMP',MSG,'W')
        GO TO 100
  201   CONTINUE
C
        CALL DMPBNK(DUMPS(I),.TRUE.)   ! make bank known to DUMP facility
C
  100 CONTINUE
  999 RETURN
  101 FORMAT(' DUMP FOR BANK ',A4,' NOT CODED ')
      END
