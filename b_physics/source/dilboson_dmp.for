      FUNCTION DILBOSON_DMP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dump DILBOSON Banks using dump facility
C-
C-   Returned value  : TRUE 
C-
C-   ENTRY DILBOSON_DDF  read in banks to dump from DILBOSON_RCP file
C-                 called by DILBOSON_INI
C-   Returned value  : TRUE if OK
C-   Inputs  : SRCP
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NCHECK
      PARAMETER (NCHECK=4)
      CHARACTER*4 CHECKS(NCHECK)
      LOGICAL DILBOSON_DMP,DILBOSON_DDF
      INTEGER MAXDMP
      PARAMETER( MAXDMP = 20 )
      CHARACTER*4 DUMPS(MAXDMP),CBANK
      CHARACTER*64 MSG
      INTEGER BANKS(MAXDMP)
      INTEGER I,K
      INTEGER NUMDMP,IER
C          checks must match the externals
      EXTERNAL PRPARH,PRPDIL,PRISAL,PRVITR 
      DATA CHECKS
     X     /'PARH','PDIL','ISAL','VITR'/
      DILBOSON_DMP=.TRUE.
C
      IF(NUMDMP.EQ.0) GOTO 999    ! nothing to dump
      CALL DMPANY('PARH',PRPARH)
      CALL DMPANY('PDIL',PRPDIL)
      CALL DMPANY('ISAL',PRISAL)
      CALL DMPANY('VITR',PRVITR)
C
      RETURN
C
C
      ENTRY DILBOSON_DDF()
C
      DILBOSON_DDF=.TRUE.
      CALL EZPICK('DILBOSON_RCP')       
      CALL EZGET('NUM_DUMPS',NUMDMP,IER)   ! get number of banks
      IF(NUMDMP.LE.MAXDMP)THEN
        CALL EZGET('DUMP_BANKS',BANKS,IER) ! get list of banks
      ELSE
        CALL ERRMSG('DILBOSON','DILBOSON_DMP',
     &      'DUMP REQUEST EXCEEDS MAXIMUM ALLOWED ','W')
        NUMDMP = 0
        DILBOSON_DDF=.FALSE.
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
        CALL ERRMSG('DILBOSON','DILBOSON_DMP',
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
