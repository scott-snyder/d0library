      SUBROUTINE DBCLB_UPDATE(DECT,CALTYPE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Used in updating a second database
C-
C-   Inputs  : DECT - detector type
C-             CALTYPE - calibration type
C-   Outputs : none
C-   Controls: none
C-
C-   Created  18-JAN-1990   Srini Rajagopalan
C-   Updated  25-JAN-1991   Jan Guida  Remove DB1 and DB2, replace with
C-                                    DBCALIB$DECT_R and DBCALIB$DECT_S 
C-   Updated  25-FEB-1991   Jan Guida  Ask if correct run before continuing 
C-   Updated   2-APR-1991   Jan Guida  Remove FZ calls, add block update
C-   updated   5-Feb-1991   J.Green    Add horizontal block update
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:CALIB.DEF'
      INCLUDE 'D0$INC:LKCALIB.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:DBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      CHARACTER*48 DBFILE,DBFIL2
      CHARACTER*1 ANS,MODE
      CHARACTER*3 DECT
      CHARACTER*7 COPT
      CHARACTER*17 CALTYPE
      CHARACTER*25 PATH
      CHARACTER*80 MSG
C
      INTEGER IDATE                   ! date as DBL3 wants it - YYMMDD
      INTEGER USE_DBL                 ! 0-FZ; 1-DB direct; 2-DB server 
      INTEGER LUN,RUNNO,CRATE,IRUN,NRUN
      INTEGER MXCRAT,EUCRAT,ICRATE
      INTEGER LBANK,LSUP
      INTEGER KEY(NKYS)
      INTEGER IERR,IOS
      LOGICAL IOK,FIRST
      EQUIVALENCE (CALIB_LNK(1),LBANK)
C----------------------------------------------------------------------
C
C  Determine whether User wants block or individual transfers
C
      CALL ENDBL(USE_DBL,1)
      IF (USE_DBL.EQ.0) THEN
        CALL INTMSG(' DATABASE MODE NOT CHOSEN' )
        GO TO 999
      ENDIF
      FIRST = .TRUE.
      CALL INTMSG(
     &  ' Contiguous mode means many runs for one module or crate ')
      Call INTMSG ( 
     &  ' Run mode means all modules or crates for a given run ')
    1 CONTINUE
      CALL GETPAR(1,' Individual [I], Contiguous [C] or Run [R] mode >',
     &           'U',MODE) 
      IF (MODE.EQ.'C') THEN
        IF (DECT .NE. 'MUO') THEN
          CALL GETPAR(1,' Enter input crate number > ','I',CRATE)
        ELSE
          CALL GETPAR(1,' Enter input module number > ','I',CRATE)
        ENDIF
        CALL GETPAR(1,' Enter Starting Run Number > ','I',RUNNO)
        CALL GETPAR(1,' Number of Run to transfer [0 for ALL] > ',
     &    'I',NRUN)
        IF (NRUN.EQ.0) NRUN = 10000       ! maximum # of runs to transfer
      ELSE IF (MODE.EQ.'R') THEN
        CALL GETPAR(1,' Enter Run number > ','I',RUNNO)
        MXCRAT = EUCRAT(0,CRATE)
        ICRATE = 0
      ELSE IF (MODE.EQ.'I') THEN
        CALL INTMSG(' Each run to be entered will be prompted for ')
      ELSE IF (MODE.EQ.'Q') THEN
        GO TO 999
      ELSE
        CALL INTMSG(' Only I, C and R options are allowed, Q to Quit ')
        GO TO 1
      ENDIF
C
      IRUN = 0
    2 CONTINUE
C
C  Open and initialize SEND database
C
      DBFILE = 'DBCALIB$'//DECT//'_S'
      IOK = .TRUE.
      CALL DBCLB_INITIALIZE(DBFILE,' ',IOK)
      IF (.NOT.IOK) THEN
        CALL INTMSG(' Error in DB1 initialization ')
        GO TO 498
      ENDIF
C
C   Get address of requested Pedestal bank from data base.
C
      CALL DBCLB_PATH(CALTYPE,DECT,PATH)
C
      IF (MODE.EQ.'I') THEN
        CALL GETPAR(1,' Enter input run number [-1 to Quit]>','I',RUNNO)
        IF (RUNNO.LT.0) GO TO 999
        CALL GETPAR(1,' Enter input crate number > ','I',CRATE)
        IF (CRATE.GT.MAXCRT) THEN
          CALL INTMSG(' Invalid Crate Number ')
          GO TO 999
        ENDIF
        IF (RUNNO.LE.1 .OR. RUNNO.GT.999999990) RUNNO = 999999990
      ELSEIF (MODE.EQ.'C') THEN
        CALL DBCLB_GTKEY(PATH,RUNNO,CRATE,KEY,IERR)
        IF (IERR.NE.0) THEN
          IOK = .FALSE.
          GO TO 498
        ENDIF
        IF (KEY(4) .GT. 999999990 .AND. KEY(3).EQ.1) THEN
          CALL DBCLB_FINISH
          CALL INTMSG(' NO valid runs in database. Aborting ')
          GO TO 999
        ENDIF
        IF (FIRST) THEN
          RUNNO = KEY(11)
          IF (KEY(3) .EQ.1) RUNNO = KEY(4) + 1
          FIRST = .FALSE.
        ELSE
          IF (KEY(4) .GT. 999999990) THEN
            CALL DBCLB_FINISH
            CALL INTMSG(' NO more valid runs in database.')
            GO TO 999
          ENDIF
          RUNNO = KEY(4) + 1
        ENDIF
      ELSEIF (MODE.EQ.'R') THEN
        ICRATE = ICRATE + 1
        CRATE = EUCRAT(1,ICRATE)
      ENDIF
C
C  Fetch data and shunt it as standalone structure into memory
C
      CALL DBCLB_FETCH(PATH,RUNNO,CRATE)
      IF (LBANK.EQ.0) THEN
        WRITE(MSG,110) RUNNO,CRATE
  110   FORMAT( ' No data found for run ',I9,' crate ',I3)
        CALL INTMSG(MSG)
        IOK = .FALSE.
        GO TO 498
      ENDIF
      CALL ZSHUNT(IXSTP,LBANK,LSUP,2,1)
      WRITE(MSG,100) IC(LBANK+6),CRATE
  100 FORMAT(' Transfering data from run ',I9,'  crate ',I3)
      CALL INTMSG(MSG)
      IF (MODE.EQ.'I') THEN
        CALL GETPAR(1,' Do you want to continue? <Y*/N> ','U',ANS)
        IF (ANS.EQ.'N') THEN
          IOK = .FALSE.
          GO TO 498
        ENDIF
      ENDIF
C
  498 CALL DBCLB_FINISH                        ! End DB main
C
      IF (.NOT.IOK) THEN
        CALL INTMSG(' Error during database Read, Aborting ')
        RETURN
      ENDIF
C----------------------------------------------------------------------
C
C  Open and initialize RECEIVE database.  Enter data
C
C      IF ( USE_DBL .EQ. 1 ) THEN
        DBFILE = 'DBCALIB$'//DECT//'_R'
        IF     (USE_DBL .EQ. 1) THEN
          COPT = 'SU'
        ELSEIF (USE_DBL .EQ. 2) THEN
          COPT = 'D'
        ENDIF
        CALL DBCLB_INITIALIZE(DBFILE,COPT,IOK)
        IF (.NOT.IOK) THEN
          CALL INTMSG(' Error during Local database initialization ')
          RETURN
        ENDIF
C
        CALL DBCLB_INSERT(PATH,IOK)
        IRUN = IRUN + 1
        IF (.NOT.IOK) CALL INTMSG(' Error in DB - Insertion ')
C
  997   CALL DBCLB_FINISH
C      ELSEIF ( USE_DBL .EQ. 2 ) THEN
C        COPT = 'R-KS348911'
C        CALL VZERO(KEY,NKYS)
C        KEY(3) = IC(LBANK+6)
C        KEY(4) = IC(LBANK+5)
C        KEY(8) = IC(LBANK+9)
C        IDATE = IC(LBANK+7)/100 + MOD(IC(LBANK+7),100)*10000
C        CALL DBPKTS(IDATE, IC(LBANK+8), KEY(9))
C        KEY(11) = IC(LBANK+6)
C        CALL D0DBL3_WRITFZ('TODO_AREA',IDVSTP,10,PATH,NKYS,KEY,COPT,
C     &                      LBANK,IERR)
C        IF (IERR.NE.0) THEN
C          WRITE (MSG,1004) IERR
C 1004     FORMAT(' EUENTER:  Error ',I4,' when writing FZ file: ')
C          CALL INTMSG(MSG)
C        ENDIF
C      ELSE
C        CALL INTMSG (' DBCLB_UPDATE: USE_DBL option not properly set ')
C      ENDIF
C
C  Drop bank, Request a garbage collection and loop over for next run
C
      IF (LBANK.NE.0) CALL MZDROP(IXSTP,LBANK,' ')      ! Drop Bank
      CALL MZGARB(IXSTP,0)
      LBANK = 0
      WRITE(MSG,900)RUNNO
  900 FORMAT(' Updated Database with Run number = ',I9.9)
      IF (IOK) CALL INTMSG(MSG)
C
      IF (IRUN.GE.NRUN .AND. MODE.EQ.'C') THEN
        CALL INTMSG(' Finished processing all requested Runs ')
        IF (NRUN.EQ.10000) THEN
          CALL INTMSG(' Maximum number of histograms reached')
          CALL INTMSG(' May not have processed all runs')
        ENDIF
      ELSEIF ( ICRATE.EQ.MXCRAT .AND. MODE.EQ.'R') THEN
        IF (DECT.EQ.'MUO') THEN
          CALL INTMSG(' Finished processing all modules ')
        ELSE
          CALL INTMSG(' Finished processing all crates ')
        ENDIF
      ELSE
        GO TO 2
      ENDIF
C
  999 RETURN
      END
