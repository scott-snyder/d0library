      FUNCTION ELFIT_DMP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dump ELFIT Banks using dump facility
C-
C-   Returned value  : TRUE 
C-
C-   ENTRY ELFIT_DDF  read in banks to dump from ELFIT_RCP file
C-                 called by ELFIT_INI
C-   Returned value  : TRUE if OK
C-   Inputs  : SRCP
C-   Created  12-JUL-1993   Andrzej Zieminski, Daria Zieminska   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NCHECK
      PARAMETER (NCHECK=3)
      CHARACTER*4 CHECKS(NCHECK)
      LOGICAL ELFIT_DMP,ELFIT_DDF
      INTEGER MAXDMP
      PARAMETER( MAXDMP = 20 )
      CHARACTER*4 DUMPS(MAXDMP),CBANK
      CHARACTER*64 MSG
      INTEGER BANKS(MAXDMP)
      INTEGER I,K
      INTEGER NUMDMP,IER
C          checks must match the externals
      EXTERNAL PRPARH,PRPELC,PRISAL 
      DATA CHECKS
     X     /'PARH','PELC','ISAL'/
      ELFIT_DMP=.TRUE.
C
      IF(NUMDMP.EQ.0) GOTO 999    ! nothing to dump
      CALL DMPANY('PARH',PRPARH)
      CALL DMPANY('PELC',PRPELC)
      CALL DMPANY('ISAL',PRISAL)
C
      RETURN
C
C
      ENTRY ELFIT_DDF()
C
      ELFIT_DDF=.TRUE.
      CALL EZPICK('ELFIT_RCP')       
      CALL EZGET('NUM_DUMPS',NUMDMP,IER)   ! get number of banks
      IF(NUMDMP.LE.MAXDMP)THEN
        CALL EZGET_iarr('DUMP_BANKS',BANKS,IER) ! get list of banks
      ELSE
        CALL ERRMSG('ELFIT','ELFIT_DMP',
     &      'DUMP REQUEST EXCEEDS MAXIMUM ALLOWED ','W')
        NUMDMP = 0
        ELFIT_DDF=.FALSE.
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
        CALL ERRMSG('ELFIT','ELFIT_DMP',
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
