      SUBROUTINE PRL0AD(PRUNIT,LJL0AD,BUNCH,CFL,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prints out L0AD (Level 0 Detector ADC) bank
C-
C-   Inputs  : BUNCH  = interested bunch
C-             PRUNIT = unit number for printout
C-             LKL0AD = bank address
C-             BUNCH  = bunch number
C-             CFL    = 'ONE' = one bank determined by bunch number
C-                      'ALL' = all banks, ie all bunches
C-             IFL    = level of printout
C-                  IFL  = 0  no printout
C-                  IFL >= 1  prints general info about L0AD banks
C-                  IFL  = 3  prints L0AD banks
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  14-JUL-1992   Freedy Nang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER BUNCH,JBUNCH
      INTEGER PRUNIT, NL0AD, IFL, LJL0AD, LKL0AD
      INTEGER IBUNCH, NCHANNELS, NWORDS
      INTEGER RAW_TIME(80), BUNCH_ID(80)
      INTEGER RAW_CHARGE(80), CORRECT_TIME(80)
      INTEGER K, IVERS
      INTEGER BEGBUN,ENDBUN
      INTEGER GZL0AD
      EXTERNAL GZL0AD
C
      CHARACTER*3 CFL
C----------------------------------------------------------------------
      IF ( IFL.LE.0 ) GOTO 999
      LKL0AD=LJL0AD
      IF ( LKL0AD.LE.0) LKL0AD=GZL0AD()
      IF ( LKL0AD.LE.0 ) THEN
        WRITE (PRUNIT,201) LKL0AD
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
          IVERS=IBITS(IQ(LKL0AD),13,5)
          CALL GTL0AD(JBUNCH,IBUNCH,NCHANNELS,NWORDS,RAW_TIME,BUNCH_ID,
     &      RAW_CHARGE,CORRECT_TIME)
          WRITE(PRUNIT,101) IVERS, IBUNCH, NCHANNELS, NWORDS
        ENDIF
        IF ( IFL.LT.3 ) GOTO 999
        IF ( IFL.EQ.3 ) THEN
          WRITE(PRUNIT,201)
          WRITE(PRUNIT,301) (K,RAW_TIME(K), BUNCH_ID(K), RAW_CHARGE(K),
     &      CORRECT_TIME(K),K=1,80)
        ENDIF
      ENDDO
C
  101 FORMAT(/' ADC banks for LV0 detector - Version ',I3,
     &  ' Current bunch is ',I3/,
     &  ' The total number of channels is ',I4,
     &  ' and the number of words per channel is ',I4)
  201 FORMAT(4X,' CHANNEL ',2X,' RAW TIME',2X,'BUNCH ID',2X,
     &  'RAW CHARGE',2X,'CORRECT TIME')
  301 FORMAT(/,80(5I11/))
  901 FORMAT(/'WRONG ADDRESS, LKL0AD = ', I10)
C----------------------------------------------------------------------
  999 RETURN
      END
