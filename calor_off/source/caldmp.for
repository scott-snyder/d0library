      FUNCTION CALDMP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dump Calorimeter Banks using dump facility
C-
C-   Returned value  : TRUE
C-
C-   ENTRY CALDDF  read in banks to dump from CALEVT_RCP file
C-                 called by CALOR_INI
C-   Returned value  : TRUE if OK
C-   Inputs  : SRCP
C-
C-   Created   8-MAY-1989   Rajendran Raja
C-   Updated  14-JAN-1990   Harrison B. Prosper  
C-      Add PJET 
C-   Updated  10-MAY-1990   N.A. Graf
C-      Add PELC, PPHO
C-   Updated   8-FEB-1991   Boaz Klima   
C-      Add PMUO and TRGR
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NCHECK
      PARAMETER (NCHECK=16)
      CHARACTER*4 CHECKS(NCHECK)
      LOGICAL CALDMP,CALDDF,FLGVAL
      INTEGER MAXDMP
      PARAMETER( MAXDMP = 20 )
      CHARACTER*4 DUMPS(MAXDMP),CBANK
      CHARACTER*64 MSG
      INTEGER BANKS(MAXDMP)
      INTEGER I,K,DMPUNI,IDMPUN
      INTEGER NUMDMP,IER,NUMEVD,NEVENT
C          checks must match the externals
      EXTERNAL PRHEAD,PRCAHT,PRCAEP,PRCAEH,PRCACL,PRCATE,PRPNUT,PRJETS
      EXTERNAL PRCAPH,PRPJET,PRPELC,PRPPHO,PRPTAU,PRPMUO,PRTRGR,PRCATD
      DATA CHECKS/'HEAD','CAHT','CAEP','CAEH','CATE',
     &  'CAPH','CACL','PNUT','JETS','PJET','PELC','PPHO','PTAU','CATD',
     &  'PMUO','TRGR'/
      DATA NEVENT/0/
C----------------------------------------------------------------------
C
C ****  DO THIS EVERY EVENT
C
      CALDMP=.TRUE.
C
      IF(NUMDMP.EQ.0) GOTO 999    ! nothing to dump
      CALL DMPANY('HEAD',PRHEAD)
      CALL DMPANY('CAHT',PRCAHT)
      CALL DMPANY('CAEP',PRCAEP)
      CALL DMPANY('CAEH',PRCAEH)        ! CAHITS BANKS HERE
      CALL DMPANY('CATE',PRCATE)
      CALL DMPANY('CAPH',PRCAPH)
      CALL DMPANY('PNUT',PRPNUT)
C
      CALL DMPANY('JETS',PRJETS)        ! CAJETS BANKS HERE
      CALL DMPANY('PJET',PRPJET)
C
      CALL DMPANY('CACL',PRCACL)        ! CAPHEL BANKS HERE
      CALL DMPANY('PELC',PRPELC)
      CALL DMPANY('PPHO',PRPPHO)
      CALL DMPANY('PTAU',PRPTAU)
      CALL DMPANY('CATD',PRCATD)
C
      CALL DMPANY('PMUO',PRPMUO)        ! MURECO BANKS HERE
C
      CALL DMPANY('TRGR',PRTRGR)        ! LEVEL1 BANKS HERE
C
      CALL FLSETS('DMPUSR_',.TRUE.)
C
      RETURN
C
C
      ENTRY CALDDF()
C
      CALDDF=.TRUE.
      CALL EZPICK('CALEVT_RCP')               ! select CALEVT_RCP
      CALL EZGETA('DUMP_BANKS',0,0,0,NUMDMP,IER)   ! get number of banks
C
      IF(NUMDMP.LE.MAXDMP)THEN
        CALL EZGET('DUMP_BANKS',BANKS,IER) ! get list of banks
      ELSE
        CALL ERRMSG('CALORIMETER','CALDMP',
     &      'DUMP REQUEST EXCEEDS MAXIMUM ALLOWED ','W')
        NUMDMP = 0
        CALDDF=.FALSE.
        CALL EZRSET
        GOTO 999                 ! failed
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
        CALL ERRMSG('CALORIMETER','CALDMP',
     &          MSG,'W')
        GO TO 100
  201   CONTINUE
        CALL DMPBNK(DUMPS(I),.TRUE.)
  100 CONTINUE
  999 RETURN
  101 FORMAT(' DUMP FOR BANK ',A4,' NOT CODED ')
      END
