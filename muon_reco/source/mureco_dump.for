      FUNCTION MURECO_DUMP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dump Muon Banks using dump facility
C-
C-   Returned value  : TRUE
C-
C-   ENTRY MURECO_DDF  read in banks to dump from MURECO_RCP file
C-                 called by MURECO_INI
C-   Returned value  : TRUE if OK
C-   Inputs  : SRCP
C-
C-   Created   15-nov-1989 Serban Protopopescu
C-   Modified: 17-Jun-1990 Shuichi Kunori    
C-       PRPMUO, PRMUON, PRMTRJ, PRMUCD, added
C-   Updated  10-SEP-1991   Daria Zieminska  add SAMUS dumps 
C-   Updated  14-JAN-1992   S. ABACHI  MFIT dump added
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NCHECK
      PARAMETER (NCHECK=9)
      CHARACTER*4 CHECKS(NCHECK)
      LOGICAL MURECO_DUMP,MURECO_DDF,FLGVAL,SAM,SAMRECO_DUMP
      INTEGER MAXDMP
      PARAMETER( MAXDMP = 20 )
      CHARACTER*4 DUMPS(MAXDMP),CBANK
      CHARACTER*64 MSG
      INTEGER BANKS(MAXDMP)
      INTEGER I,K,DMPUNI,IDMPUN
      INTEGER NUMDMP,IER,NUMEVD,NEVENT
C          checks must match the externals
      EXTERNAL PRISAL
      EXTERNAL PRMUOH,PRMUOT
      EXTERNAL PRMUON,PRMUCD,PRMTRJ 
      EXTERNAL PRPMUO,PRMFIT
C     -- S/R PRMUSR is a hook to user who want to dump some event
C        informations with default banks.   S/R PRUSER is empty
C        in the MUON_RECO library.
      EXTERNAL PRMUSR
      DATA CHECKS/'MUOH','MUOT','MFIT','MUON','MUCD','MTRJ'
     +  ,'PMUO','ISAL','MUSR'/
C----------------------------------------------------------------------
C
C ****  DO THIS EVERY EVENT
C
      MURECO_DUMP=.TRUE.
C
      CALL DMPANY('MUOH',PRMUOH)
      CALL DMPANY('MUOT',PRMUOT)
      CALL DMPANY('MFIT',PRMUOT)
      CALL DMPANY('MUON',PRMUON)
      CALL DMPANY('MUCD',PRMUCD)
      CALL DMPANY('MTRJ',PRMTRJ)
      CALL DMPANY('PMUO',PRPMUO)
      CALL DMPANY('ISAL',PRISAL)
      CALL DMPANY('MUSR',PRMUSR)
      SAM=SAMRECO_DUMP()
      RETURN
C
C
      ENTRY MURECO_DDF()
C
      MURECO_DDF=.TRUE.
      CALL EZPICK('MURECO_RCP')               ! select MURECO_RCP
      CALL EZGET_i('NUM_DUMPS',NUMDMP,IER)   ! get number of banks
      CALL EZGET_i('NUM_EVENT_DUMPS',NUMEVD,IER)  ! Get number of events
C
      IF(NUMDMP.LE.MAXDMP)THEN
        CALL EZGET_iarr('DUMP_BANKS',BANKS,IER) ! get list of banks
      ELSE
        CALL ERRMSG('MURECO','MURECO_DDF',
     &      'DUMP REQUEST EXCEEDS MAXIMUM ALLOWED ','W')
        NUMDMP = 0
        MURECO_DDF=.FALSE.
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
        CALL ERRMSG('MURECO','MURECO_DDF',
     &          MSG,'W')
        GO TO 100
  201   CONTINUE
        CALL DMPBNK(DUMPS(I),.TRUE.)
  100 CONTINUE
  999 RETURN
  101 FORMAT(' DUMP FOR BANK ',A4,' NOT CODED ')
      END
