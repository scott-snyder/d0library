      SUBROUTINE L2_COMP_PNUT(LUN,LPNUT,IB,OKOK,
     &    PNUT_EX,PNUT_EY,PNUT_ET,PNUT_PHI,PNUT_ETSCALAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-        compare PNUT banks 
C
C     grabs info on the PNUT bank 
C
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   9-APR-1992   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:HEADER.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER LPNUT(*),IB,LUN
      LOGICAL OKOK
C
      INTEGER NCAND1,NCAND2,NREP1,NREP2,IP1,IP2,I,J,NRUN,NEV
      LOGICAL LEX,LEY,LET,LPHI,LETSC
      LOGICAL LDUMP,L2_NXT_REAL_DIFF,L2_NXT_INT_DIFF
      REAL PNUT_EX,PNUT_EY,PNUT_ET,PNUT_PHI,PNUT_ETSCALAR
      REAL EX1,EX2,EY1,EY2,ET1,ET2,PHI1,PHI2,ETSC1,ETSC2
      CHARACTER*1 CEX,CEY,CET,CPHI,CETSC
C
      DATA CEX/' '/,CEY/' '/,CET/' '/,CPHI/' '/,CETSC/' '/
C----------------------------------------------------------------------
C
      OKOK = .TRUE.
      NRUN = IQ(LHEAD+6)
      NEV = IQ(LHEAD+9)
C
      IF (IB.NE.2) THEN
        CALL ERRMSG('L2_COMPARE','L2_COMP_PNUT',
     &    'L2_COMP_PNUT called for IB.NE.2 ! Not allowed!!!','F')
        RETURN
      ENDIF
C
C     make sure there is PNUT - no mistakes
C
      IF (LPNUT(1).LE.0) THEN
        WRITE(LUN,'('' L2_COMP_PNUT: Run/Event '',2I7,
     &      '' has NO PNUT bank for SIMULATION'')') NRUN,NEV
        GOTO 999
      ENDIF
      IF (LPNUT(2).LE.0) THEN
        WRITE(LUN,'('' L2_COMP_PNUT: Run/Event '',2I7,
     &      '' has NO PNUT bank for DATA'')') NRUN,NEV
        GOTO 999
      ENDIF
C
      IP1 = 2 + LPNUT(1)
      IP2 = 2 + LPNUT(2)
      LEX   = L2_NXT_REAL_DIFF(IP1,IP2,EX1,EX2,PNUT_EX,CEX)
      LEY   = L2_NXT_REAL_DIFF(IP1,IP2,EY1,EY2,PNUT_EY,CEY)
      IP1 = IP1 + 2       ! skip EZ,E
      IP2 = IP2 + 2       ! skip EZ,E
      LET   = L2_NXT_REAL_DIFF(IP1,IP2,ET1,ET2,PNUT_ET,CET)
      IP1 = IP1 + 2       ! skip THETA,ETA
      IP2 = IP2 + 2       ! skip THETA,ETA
      LPHI  = L2_NXT_REAL_DIFF(IP1,IP2,PHI1,PHI2,PNUT_PHI,CPHI)
      IP1 = IP1 + 3       ! skip sigEX**2,sigEY**2,sigET
      IP2 = IP2 + 3       ! skip sigEX**2,sigEY**2,sigET
      LETSC   = L2_NXT_REAL_DIFF(IP1,IP2,ETSC1,ETSC2,
     &  PNUT_ETSCALAR,CETSC)
C
      LDUMP = LEX.OR.LEY.OR.LET.OR.LPHI.OR.LETSC
      IF (LDUMP) THEN
        OKOK = .FALSE.
        WRITE(LUN,'(/,'' L2_COMP_PNUT:  RUN/EVENT '',2I7,
     &        '' PNUT entry discrepancy'',/,
     &        '' "*" denote variables whose differences is '',
     &        ''outside of tolerances'')') NRUN,NEV
        WRITE(LUN,'(''            EX        EY       '',
     &    ''ET   PHI(deg)    ET scalar'')')
        WRITE(LUN,'('' SIM '',2(F9.1,A1),2(F8.1,A1),4X,F8.1,A1)')
     &    EX1,CEX,EY1,CEY,ET1,CET,PHI1/RADIAN,CPHI,ETSC1,CETSC
        WRITE(LUN,'('' DAT '',2(F9.1,A1),2(F8.1,A1),4X,F8.1,A1)')
     &    EX2,CEX,EY2,CEY,ET2,CET,PHI2/RADIAN,CPHI,ETSC2,CETSC
      ENDIF
C
      RETURN
C
  999 CONTINUE
C
C     things are amiss
C
      OKOK = .FALSE.
      RETURN
C
      END
