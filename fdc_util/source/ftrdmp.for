      FUNCTION FTRDMP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dump FDC Banks using dump facility
C-
C-   Returned value  : TRUE
C-
C-   ENTRY FTRDDF  read in banks to dump from FTRAKS_RCP file
C-                 called by FTRINI
C-   Returned value  : TRUE if OK
C-   Inputs  : SRCP
C-
C-   based on CALDMP
C-   Updated  20-MAR-1990   Jeffrey Bantly general cleanup
C-   Updated  29-APR-1991   Jeffrey Bantly  cleanup EZ calls, add check 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL FTRDMP,FTRDDF
C
      INTEGER NCHECK
      PARAMETER (NCHECK=13)
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
      EXTERNAL PRFDCH,PRFHLF,PRFDUN,PRFTQD,PRFTSC,PRFPSC,PRFDCT,
     X         PRFSG0,PRFSG1,PRFSG2,PRFSG3,PRFSG4,PRFSG5
      DATA CHECKS
     X     /'FDCH','FHLF','FDUN','FTQD','FTSC','FPSC','FDCT',
     X      'FSG0','FSG1','FSG2','FSG3','FSG4','FSG5'/
C----------------------------------------------------------------------
C
C ****  DO THIS EVERY EVENT
C
      FTRDMP=.TRUE.
C
      IF(NUMDMP.EQ.0) GOTO 999    ! nothing to dump
      CALL DMPANY('FDCH',PRFDCH)
      CALL DMPANY('FHLF',PRFHLF)
      CALL DMPANY('FDUN',PRFDUN)
      CALL DMPANY('FTQD',PRFTQD)
      CALL DMPANY('FTSC',PRFTSC)
      CALL DMPANY('FPSC',PRFPSC)
      CALL DMPANY('FSG0',PRFSG0)
      CALL DMPANY('FSG1',PRFSG1)
      CALL DMPANY('FSG2',PRFSG2)
      CALL DMPANY('FSG3',PRFSG3)
      CALL DMPANY('FSG4',PRFSG4)
      CALL DMPANY('FSG5',PRFSG5)
      CALL DMPANY('FDCT',PRFDCT)
C
      RETURN
C
C
      ENTRY FTRDDF()
C
      FTRDDF=.TRUE.
      CALL EZPICK('FTRAKS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('FTRAKS','FTRDMP','FTRAKS_RCP not found.','W')
      ELSE
        CALL EZGET_i('NUM_DUMPS',NUMDMP,IER)   ! get number of banks
        IF(NUMDMP.LE.MAXDMP)THEN
          CALL EZGET('DUMP_BANKS',BANKS,IER) ! get list of banks
        ELSE
          CALL ERRMSG('FTRAKS','FTRDMP',
     &      'DUMP REQUEST EXCEEDS MAXIMUM ALLOWED ','W')
          NUMDMP = 0
          FTRDDF=.FALSE.
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
        CALL ERRMSG('FTRAKS','FTRDMP',
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
