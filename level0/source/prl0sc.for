      SUBROUTINE PRL0SC(PRUNIT,LJL0SC,BUNCH,CFL,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prints out L0SC (Level 0 Detector Scaler) bank
C-
C-   Inputs  : BUNCH = interested bunch
C-             PRUNIT = unit number for printout
C-             LKL0SC = bank address
C-             NL0SC = bunch number
C-             CFL    = 'ONE' = one bank determined by bunch number
C-                      'ALL' = all banks, ie all bunches
C-             IFL = level of printout
C-                IFL  = 0  no printout
C-                IFL >= 1  prints general info about L0SC bank
C-                IFL  = 3  prints L0SC banks
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  21-JUL-1992   Freedy Nang
C-   Updated  28-JAN-1994   Jeffrey Bantly  to include 4 Run 1B scalers 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:LV0CON.INC'
C
      INTEGER BUNCH, IBUNCH, JBUNCH
      INTEGER PRUNIT, IFL, LJL0SC, LKL0SC
      INTEGER IVERS, NSCALERS, NWORDS
      INTEGER K, GOODZ_SCALER(0:NL0_SCALERS)
      INTEGER NGOODZ_SCALER(0:NL0_SCALERS)
      INTEGER BEGBUN,ENDBUN
      INTEGER GZL0SC
      EXTERNAL GZL0SC
C
      CHARACTER*3 CFL
C----------------------------------------------------------------------
      IF ( IFL.LE.0 ) GOTO 999
      LKL0SC=LJL0SC
      IF ( LKL0SC.LE.0) LKL0SC=GZL0SC()
      IF ( LKL0SC.LE.0 ) THEN
        WRITE (PRUNIT,201) LKL0SC
        GOTO 999
      ENDIF
C
      BEGBUN=1
      ENDBUN=6
      IF ( CFL.EQ.'ONE' ) THEN
        BEGBUN=BUNCH
        ENDBUN=BUNCH
      ENDIF
      DO JBUNCH=BEGBUN,ENDBUN
        IF ( IFL.GE.1 ) THEN
          IVERS=IBITS(IQ(LKL0SC),13,5)
          CALL GTL0SC(JBUNCH,IBUNCH,NSCALERS,NWORDS,GOODZ_SCALER,
     &      NGOODZ_SCALER)
          WRITE(PRUNIT,101) IVERS, IBUNCH, NSCALERS, NWORDS
        ENDIF
        IF ( IFL.LT.3 ) GOTO 999
        IF ( IFL.EQ.3 ) THEN
          WRITE(PRUNIT,201)
          WRITE(PRUNIT,301) 
     &      (K, GOODZ_SCALER(K),NGOODZ_SCALER(K),K=0,NL0_SCALERS)
        ENDIF
      ENDDO
C
  101 FORMAT(/' SCALER banks for LV0 detector - Version ',I3,
     &  ' Current bunch is ',I3/,
     &  ' The total number of scalers is ',I4,
     &  ' and the number of words per channel is ',I4)
  201 FORMAT(8X,' SCALER ',2X,' GOOD Z SCALER ',2X,' NGOOD Z SCALER ')
  301 FORMAT(/,15(3I14/))
  901 FORMAT(/'WRONG ADDRESS, LKL0SC = ', I10)
C----------------------------------------------------------------------
  999 RETURN
      END
