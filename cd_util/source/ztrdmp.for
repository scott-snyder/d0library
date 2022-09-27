      FUNCTION ZTRDMP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dump CD banks using dump facility
C-
C-   Returned value  : TRUE 
C-
C-   ENTRY ZTRDDF  read in banks to dump from ZTRAKS_RCP file
C-                 called by ZTRINI
C-   Returned value  : TRUE 
C-   Inputs  : SRCP
C-
C-   based on CALDMP
C-
C-   Updated   4-FEB-1991   Daria Zieminska  Call VERDMP 
C-   Updated  24-FEB-1992   Daria Zieminska   add user dump option
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NCHECK
      PARAMETER (NCHECK=11)
      CHARACTER*4 CHECKS(NCHECK)
      LOGICAL ZTRDMP,ZTRDDF,VERDMP,OK
      INTEGER MAXDMP
      PARAMETER( MAXDMP = 20 )
      CHARACTER*4 DUMPS(MAXDMP),CBANK
      CHARACTER*64 MSG
      INTEGER BANKS(MAXDMP)
      INTEGER I,K
      INTEGER NUMDMP,IER
C          checks must match the externals
      EXTERNAL PRVTXH,PRCDCH,PRFDCH,PRVTXT,PRDTRK,PRFDCT,PRTRDT,
     X         PRTPRL,PRZTRK,PRZFIT,PRZUSR
      DATA CHECKS
     X /'VTXH','CDCH','FDCH','VTXT','DTRK','FDCT','TRDT','TPRL','ZTRK',
     X  'ZFIT','ZUSR'/
C----------------------------------------------------------------------
C
C ****  DO THIS EVERY EVENT
C
      OK=VERDMP()
      ZTRDMP=.TRUE.
C
      IF(NUMDMP.EQ.0) GOTO 999    ! nothing to dump
      CALL DMPANY('VTXH',PRVTXH)
      CALL DMPANY('CDCH',PRCDCH)
      CALL DMPANY('FDCH',PRFDCH)
      CALL DMPANY('VTXT',PRVTXT)
      CALL DMPANY('DTRK',PRDTRK)
      CALL DMPANY('FDCT',PRFDCT)
      CALL DMPANY('TRDT',PRTRDT)
      CALL DMPANY('TPRL',PRTPRL)
      CALL DMPANY('ZTRK',PRZTRK)
      CALL DMPANY('ZFIT',PRZFIT)
      CALL DMPANY('ZUSR',PRZUSR)
C
      RETURN
C
C
      ENTRY ZTRDDF()
C
      ZTRDDF=.TRUE.
      CALL EZPICK('ZTRAKS_RCP')       
      CALL EZGET_i('NUM_DUMPS',NUMDMP,IER)   ! get number of banks
      IF(NUMDMP.LE.MAXDMP)THEN
        CALL EZGET('DUMP_BANKS',BANKS,IER) ! get list of banks
      ELSE
        CALL ERRMSG('ZTRAKS','ZTRDMP',
     &      'DUMP REQUEST EXCEEDS MAXIMUM ALLOWED ','W')
        NUMDMP = 0
        ZTRDDF=.FALSE.
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
        CALL ERRMSG('ZTRAKS','ZTRDMP',
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
