      FUNCTION SAMRECO_DUMP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dump SAMUS Banks using dump facility
C-
C-   Returned value  : TRUE
C-
C-   ENTRY SAMRECO_DDF  read in banks to dump from SAMRECO_RCP file
C-                      called by SAMRECO_INI
C-   Returned value  : TRUE if OK
C-   Inputs  : SRCP
C-
C-   Created   10-MAY-1991  O.Eroshin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NCHECK
      PARAMETER (NCHECK=5)
      CHARACTER*4 CHECKS(NCHECK)
      LOGICAL SAMRECO_DUMP,SAMRECO_DDF,FLGVAL
      INTEGER MAXDMP
      PARAMETER( MAXDMP = 20 )
      CHARACTER*4 DUMPS(MAXDMP),CBANK
      CHARACTER*64 MSG
      INTEGER BANKS(MAXDMP)
      INTEGER I,K,DMPUNI,IDMPUN
      INTEGER NUMDMP,IER,NUMEVD,NEVENT
C          checks must match the externals
      EXTERNAL PRSAHH,PRSAMH,PRMUOT,PRSTTH,PRSUSR
      DATA CHECKS/'SAHH','SAMH','MUOT','STTH','SUSR'/
C----------------------------------------------------------------------
C
C ****  DO THIS EVERY EVENT
C
      SAMRECO_DUMP = .TRUE.
C
      CALL DMPANY('SAHH',PRSAHH)
      CALL DMPANY('SAMH',PRSAMH)
      CALL DMPANY('MUOT',PRMUOT)
      CALL DMPANY('STTH',PRSTTH)
      CALL DMPANY('SUSR',PRSUSR)
      RETURN
C
C
      ENTRY SAMRECO_DDF()
C
      SAMRECO_DDF = .TRUE.
      CALL EZPICK('SAMRECO_RCP')                ! select SAMRECO_RCP
      CALL EZGET('NUM_DUMPS',NUMDMP,IER)        ! get number of banks
      CALL EZGET('NUM_EVENT_DUMPS',NUMEVD,IER)  ! Get number of events
C
      IF(NUMDMP.LE.MAXDMP)		THEN
        CALL EZGET('DUMP_BANKS',BANKS,IER)      ! get list of banks
      					ELSE
        CALL ERRMSG('SAMRECO','SAMRECO_DDF',
     &      'DUMP REQUEST EXCEEDS MAXIMUM ALLOWED ','W')
        NUMDMP      = 0
        SAMRECO_DDF = .FALSE.
        CALL EZRSET
        GOTO 999                                ! failed
      ENDIF
      CALL EZRSET
C
C ****  Supply banks to dump facility
C
      DO 100 I = 1,NUMDMP
        CALL UHTOC(BANKS(I),4,DUMPS(I),4)
        DO 200 K = 1,NCHECK
          IF(DUMPS(I).EQ.CHECKS(K)) GO TO 201
  200   CONTINUE
        WRITE(MSG,101) DUMPS(I)
        CALL ERRMSG('SAMRECO','SAMRECO_DDF',
     &          MSG,'W')
        GO TO 100
  201   CONTINUE
        CALL DMPBNK(DUMPS(I),.TRUE.)
  100 CONTINUE
  999 RETURN
  101 FORMAT(' DUMP FOR BANK ',A4,' NOT CODED ')
      END
