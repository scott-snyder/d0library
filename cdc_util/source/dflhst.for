      SUBROUTINE DFLHST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-              fill all histograms which has been booked through            
C-              user dialog
C-     
C-   Inputs  : 
C-   Outputs : 
C-   Controls: called by DTREVT
C-
C-   Created  19-AUG-1988   Qizhong Li-Demarteau
C-   Updated  23-OCT-1989   Qizhong Li-Demarteau  set hbook_directory
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL HSTEFF, HSTPUL, HSTDSC, HSTCHZ, HSTCHI, HSTSWR, HSTDLR
      LOGICAL WITHDL, HSTFLG, HSTCHD, DLMDLP, HSTPED, HSTDEX
      LOGICAL FIRST
      INTEGER NRUN, NID01, IER
      REAL    XLO, XHI
      SAVE FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C         Create/Set HBOOK directory for DTRAKS
C
      CALL DHDIR('DTRAKS_RCP','HBOOK_DIRECTORY',IER,' ')
      IF (IER .NE. 0) THEN
        CALL ERRMSG('DTRAKS','DFLHST',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
C       to keep run # and event # in a histogram
C
      NRUN = MOD(IQ(LHEAD+6),1000000)
      IF (FIRST) THEN
        FIRST = .FALSE.
        XLO = FLOAT(NRUN)-25.5
        XHI = FLOAT(NRUN)+24.5
        CALL HBOOK1(1,'RUN number$',50,XLO,XHI,0.)
      ENDIF
      CALL HFF1(1,NID01,FLOAT(NRUN),1.)     
C                                               
C       to fill histogams associated with STP file information
C
      CALL DTSPED(HSTPED)
      IF (HSTPED) CALL DFLPED          ! to fill the pedestals and T0
C                                               
C       to fill histogams associated with hits information
C
      CALL DTSPUL(HSTPUL,WITHDL)
      IF (HSTPUL) CALL DFLPUL(WITHDL)   
C                          !  to fill the pulse area,w,h vs dist. hist.
C 
C       to fill histogams associated with full track information
C 
      CALL DTSEFF(HSTEFF)
      CALL DTSDSC(HSTDSC,DLMDLP)
      CALL DTSCHD(HSTCHD)
      CALL DTSCHZ(HSTCHZ)
      CALL DTSCHI(HSTCHI)
      CALL DTSDEX(HSTDEX)
      CALL DTSSWR(HSTSWR)
      CALL DTSDLR(HSTDLR)
      HSTFLG = HSTEFF.OR.HSTDSC.OR.HSTCHZ.OR.HSTCHI.OR.HSTDEX
      HSTFLG = HSTFLG.OR.HSTCHD.OR.HSTSWR.OR.HSTDLR
      IF (HSTFLG) CALL DFLTRK    
      IF (HSTEFF .OR. HSTCHI) CALL DFLSEG
      IF (HSTSWR) CALL DFLSEG
C
  999 RETURN
      END
