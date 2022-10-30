      FUNCTION TRDDMP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dump Calorimeter Banks using dump facility
C-
C-   Returned value  : TRUE 
C-
C-   ENTRY TRDDDF  read in banks to dump from TRD_RCP file
C-                 called by TRDINI
C-   Returned value  : TRUE if OK
C-   Inputs  : SRCP
C-
C-   Daria Zieminska (based on CALDMP)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NCHECK
      PARAMETER (NCHECK=2)
      CHARACTER*4 CHECKS(NCHECK)
      LOGICAL TRDDMP,TRDDDF 
      INTEGER MAXDMP
      PARAMETER( MAXDMP = 20 )
      CHARACTER*4 DUMPS(MAXDMP),CBANK
      CHARACTER*64 MSG
      INTEGER BANKS(MAXDMP)
      INTEGER I,K
      INTEGER NUMDMP,IER
C          checks must match the externals
      EXTERNAL PRTRDH,PRTLYR 
      DATA CHECKS/'TRDH','TLYR'/
C----------------------------------------------------------------------
C
C ****  DO THIS EVERY EVENT
C
      TRDDMP=.TRUE.
C
      IF(NUMDMP.EQ.0) GOTO 999    ! nothing to dump
      CALL DMPANY('TRDH',PRTRDH)
      CALL DMPANY('TLYR',PRTLYR)
C
      RETURN
C
C
      ENTRY TRDDDF()
C
      TRDDDF=.TRUE.
      CALL EZPICK('TRD_RCP')       
      CALL EZGET('NUM_DUMPS',NUMDMP,IER)   ! get number of banks
      IF(NUMDMP.LE.MAXDMP)THEN
        CALL EZGET_iarr('DUMP_BANKS',BANKS,IER) ! get list of banks
      ELSE
        CALL ERRMSG('TRD','TRDDMP',
     &      'DUMP REQUEST EXCEEDS MAXIMUM ALLOWED ','W')
        NUMDMP = 0
        TRDDDF=.FALSE.
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
        CALL ERRMSG('TRD','TRDDMP',
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
