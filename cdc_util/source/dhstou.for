      SUBROUTINE DHSTOU(HSTFLG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-              Store all histograms. Some histograms need special 
C-              treatment before to be stored
C-
C-   Inputs  : 
C-   Outputs : HSTFLG (.true. if any average/sigma hitograms are booked)
C-   Controls: called by DTRUSM (only when user output is requested)
C-
C-   Created  23-AUG-1988   Qizhong Li-Demarteau
C-   Updated  20-FEB-1989   Qizhong Li-Demarteau  use D0OPEN now
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL HSTEFF, HSTPUL, HSTDSC, HSTCHZ, HSTCHI, HSTSWR, HSTDLR
      LOGICAL WITHDL, HSTFLG, HSTCHD, DLMDLP, OK
      INTEGER NRUN, RUNNO, OUT, ERR, IER
      INTEGER IDVECT(2000), NUMHIS, N
      CHARACTER*40 HISFIL
      CHARACTER*80 MSG
      REAL    DUMMY1, DUMMY2, DUMMY3, DUMMY4, DUMMY5, DUMMY6
C----------------------------------------------------------------------
C
      CALL HIDALL (IDVECT,NUMHIS) ! to see if there is any histogram
C
      IF (NUMHIS.NE.0) THEN
        CALL DTSEFF(HSTEFF)
        IF (HSTEFF) THEN
          CALL HOPERA(1003,'/',1005,1003,1.,1.)
          CALL HOPERA(1004,'/',1005,1004,1.,1.)
          CALL HOPERA(1004,'/',1003,1005,1.,1.)
        ENDIF
C  
        CALL DTSPUL(HSTPUL,WITHDL)
        CALL DTSDSC(HSTDSC,DLMDLP)
        CALL DTSCHD(HSTCHD)
        CALL DTSCHZ(HSTCHZ)
        CALL DTSSWR(HSTSWR)
        CALL DTSDLR(HSTDLR)
C        HSTFLG = HSTPUL.OR.HSTDSC.OR.HSTCHD.OR.HSTCHZ
C        HSTFLG = HSTFLG.OR.HSTSWR.OR.HSTDLR
C        IF (HSTFLG) THEN
C          CALL HBAVDP(0,DUMMY1,DUMMY2,DUMMY3,DUMMY4,DUMMY5,DUMMY6)
C        ENDIF
C
        NRUN=RUNNO()
C
        CALL STRINT('CDCTST_',NRUN,HISFIL,N)
        HISFIL = HISFIL(1:N)//'.HST4'
C
        MSG=' Storing user selected histograms in '//HISFIL
        CALL INTMSG(MSG)
C        CALL GTUNIT(10,OUT,ERR)
C        CALL D0OPEN(OUT,HISFIL,'OU',OK)
C        IF(.NOT.OK) GOTO 998
C        CALL HSTORE(0,OUT)
C        CALL RLUNIT(10,OUT,ERR)
C        CLOSE(OUT)
C
C
C        Set HBOOK directory for DTRAKS
C
      CALL DHDIR('DTRAKS_RCP','HBOOK_DIRECTORY',IER,' ')
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('DTRAKS','DHSTOU',
     &    ' ERROR SETTING HBOOK DIRECTORY, GO TO TOP DIRECTORY ','W')
        CALL HCDIR('//PAWC',' ')
      ENDIF
C
        CALL HRPUT(0,HISFIL,'NT')
      ENDIF
C
  999 RETURN
C
  998 MSG=' cannot open '//HISFIL
      CALL INTMSG(MSG)
      RETURN
C
      END
