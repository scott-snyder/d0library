      FUNCTION VERDMP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dump VTX Banks using dump facility
C-
C-   Returned value  : TRUE 
C-
C-   ENTRY VERDDF  read in banks to dump from VERTEX_RCP file
C-                 called by VERINI
C-   Returned value  : TRUE if OK
C-   Inputs  : SRCP
C-
C-   based on CALDMP
C-   Updated  24-SEP-1990   Qizhong Li-Demarteau  remove VTXH bank dump 
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZERROR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NCHECK
      PARAMETER (NCHECK=2)
      CHARACTER*4 CHECKS(NCHECK)
      LOGICAL VERDMP,VERDDF
      LOGICAL EZERROR
      INTEGER MAXDMP
      PARAMETER( MAXDMP = 20 )
      CHARACTER*4 DUMPS(MAXDMP),CBANK
      CHARACTER*64 MSG
      INTEGER BANKS(MAXDMP)
      INTEGER I,K
      INTEGER NUMDMP,IER
C          checks must match the externals
      EXTERNAL PRVERH,PRVERT 
      DATA CHECKS
     X     /'VERH','VERT'/
C----------------------------------------------------------------------
C
C ****  DO THIS EVERY EVENT
C
      VERDMP=.TRUE.
C
      IF(NUMDMP.EQ.0) GOTO 999    ! nothing to dump
      CALL DMPANY('VERH',PRVERH)
      CALL DMPANY('VERT',PRVERT)
C
      RETURN
C
C
      ENTRY VERDDF()
C
      VERDDF=.TRUE.
      CALL EZPICK('VERTEX_RCP')       
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('ZTRAKS','VERDMP',
     &    'Unable to find bank VERTEX_RCP','W')
        GOTO 999
      ENDIF
      CALL EZGET('NUM_DUMPS',NUMDMP,IER)   ! get number of banks
      IF(NUMDMP.LE.MAXDMP)THEN
        CALL EZGET('DUMP_BANKS',BANKS,IER) ! get list of banks
      ELSE
        CALL ERRMSG('VERTEX','VERDMP',
     &      'DUMP REQUEST EXCEEDS MAXIMUM ALLOWED ','W')
        NUMDMP = 0
        VERDDF=.FALSE.
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
        CALL ERRMSG('VERTEX','VERDMP',
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
