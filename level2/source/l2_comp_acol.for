      SUBROUTINE L2_COMP_ACOL(LUN,LACOL,IB,OKOK,ACOL_ET,ACOL_ETA,
     &    ACOL_PHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  compare ACOL banks
C
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   16-JUN-1994   Lewis Taylor Goss
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:HEADER.INC'
C
      INTEGER IVERSNO1,IVERSNO2,NOREPW1,NOREPW2,NOPARS1,NOPARS2,ISTAT1,I
      INTEGER ISTAT2,NUMJET1,NUMJET2,IP1,IP2,NRUN,NEV,J,LACOL(*),IB,LUN
      REAL ET1(4),ET2(4),ETA1(4),ETA2(4),PHI1(4),PHI2(4),ACOL_ET
      REAL ACOL_ETA,ACOL_PHI,MET1,MET2,PHIMET1,PHIMET2
      LOGICAL LET(4),LETA(4),LPHI(4),LVERSNO,LNOREPW,LNOPARS,LSTAT,LMET
      LOGICAL LDUMP,L2_NXT_REAL_DIFF,L2_NXT_INT_DIFF,LNUMJET,OKOK
      LOGICAL LPHIMET
      CHARACTER*1 CVERSNO,CNOPARS,CNOREPW,CSTAT,CNUMJET,CMET,CPHIMET
      CHARACTER*1 CET(4),CETA(4),CPHI(4)
C
      DATA CVERSNO/' '/,CNOPARS/' '/,CNOREPW/' '/,CSTAT/' '/,CET(1)/' '/
      DATA CNUMJET/' '/,CETA(1)/' '/,CPHI(1)/' '/,CET(2)/' '/
      DATA CPHI(2)/' '/,CETA(2)/' '/,CET(3)/' '/,CET(4)/' '/
      DATA CETA(3)/' '/,CPHI(3)/' '/,CETA(4)/' '/,CPHI(4)/' '/
      DATA CMET/' '/,CPHIMET/' '/
C----------------------------------------------------------------------
C
      OKOK = .FALSE.
      NRUN = IQ(LHEAD+6)
      NEV = IQ(LHEAD+9)
C
      IF (IB.NE.2) THEN
        CALL ERRMSG('L2_COMPARE','L2_COMP_ACOL',
     &    'L2_COMP_ACOL called for IB.NE.2 ! Not allowed!!!','F')
        RETURN
      ENDIF
C
C     make sure there is ACOL - no mistakes
C
      IF (LACOL(1).LE.0) THEN
        WRITE(LUN,'('' L2_COMP_ACOL: Run/Event '',2I7,
     &      '' has NO ACOL bank for SIMULATION'')') NRUN,NEV
        GOTO 999
      ENDIF
      IF (LACOL(2).LE.0) THEN
        WRITE(LUN,'('' L2_COMP_ACOL: Run/Event '',2I7,
     &      '' has NO ACOL bank for DATA'')') NRUN,NEV
        GOTO 999
      ENDIF
C check version number
      IP1 = LACOL(1)
      IP2 = LACOL(2)
      LVERSNO = L2_NXT_INT_DIFF(IP1,IP2,IVERSNO1,IVERSNO2,CVERSNO)
C check number of parameter sets
      LNOPARS = L2_NXT_INT_DIFF(IP1,IP2,NOPARS1,NOPARS2,CNOPARS)
C check number of repeated words
      LNOREPW = L2_NXT_INT_DIFF(IP1,IP2,NOREPW1,NOREPW2,CNOREPW)
C jump out if disagreement is found.
      LDUMP = LVERSNO .OR. LNOPARS .OR. LNOREPW
      IF (LDUMP) THEN
        WRITE(LUN,'(/,'' L2_COMP_ACOL:  RUN/EVENT '',2I7,
     &        '' ACOL entry discrepancy'',/,
     &        '' "*" denote variables whose differences are '',
     &        ''outside of tolerances'')') NRUN,NEV
        WRITE(LUN,*) '       VERSNO   NOPARS   NOREPW'
        WRITE(LUN,'('' SIM'',3(5X,I3,A))') IVERSNO1,CVERSNO,NOPARS1,
     &    CNOPARS,NOREPW1,CNOREPW
        WRITE(LUN,'('' DAT'',3(5X,I3,A))') IVERSNO2,CVERSNO,NOPARS2,
     &    CNOPARS,NOREPW2,CNOREPW
C        GOTO 999
      ENDIF
C loop over all parameter sets
      DO I = 1, NOPARS1
C check status word
        LSTAT = L2_NXT_INT_DIFF(IP1,IP2,ISTAT1,ISTAT2,CSTAT)
C check number of jets in the event
        LNUMJET = L2_NXT_INT_DIFF(IP1,IP2,NUMJET1,NUMJET2,CNUMJET)
C check missing Et in the event
        LMET = L2_NXT_REAL_DIFF(IP1,IP2,MET1,MET2,ACOL_ET,CMET)
C check phi of the missing Et in the event
        LPHIMET = L2_NXT_REAL_DIFF(IP1,IP2,PHIMET1,PHIMET2,ACOL_PHI,
     &    CPHIMET)
C check Et of jth object
        DO J=1,4
          LET(J) = L2_NXT_REAL_DIFF(IP1,IP2,ET1(J),ET2(J),ACOL_ET,
     &      CET(J))
C check eta of jth object
          LETA(J) = L2_NXT_REAL_DIFF(IP1,IP2,ETA1(J),ETA2(J),ACOL_ETA,
     &      CETA(J))
C check phi of jth object
          LPHI(J) = L2_NXT_REAL_DIFF(IP1,IP2,PHI1(J),PHI2(J),ACOL_PHI,
     &      CPHI(J))
        END DO
C check to see if disagreement found
        LDUMP = LSTAT.OR.LNUMJET.OR.LMET.OR.LPHIMET.OR.LET(1).OR.LETA(1)
     &    .OR.LPHI(1).OR.LET(2).OR.LETA(2).OR.LPHI(2).OR.LET(3).OR.
     &    LETA(3).OR.LPHI(3).OR.LET(4).OR.LETA(4).OR.LPHI(4)
        IF (LDUMP) THEN
          WRITE(LUN,'(/,'' L2_COMP_ACOL:  RUN/EVENT '',2I7,
     &      '' ACOL entry discrepancy'',/,
     &      '' "*" denote variables whose differences is '',
     &      ''outside of tolerances'')') NRUN,NEV
          WRITE(LUN,*) '   PR  ST               ------ LEADING JET ---',
     &      '---  --- JET CLOSEST TO MET --'
          WRITE(LUN,*) '   NO  AT    MET           ET      ETA     PHI',
     &      '        ET      ETA      PHI'
C write simulation data
          WRITE(LUN,10) 'SM',I,ISTAT1,CSTAT,MET1,CMET,ET1(1),CET(1),
     &      ETA1(1),CETA(1),PHI1(1),CPHI(1),ET1(2),CET(2),ETA1(2),
     &      CETA(2),PHI1(2),CPHI(2)
C write out on-line data
          WRITE(LUN,10) 'DT',I,ISTAT2,CSTAT,MET2,CMET,ET2(1),CET(1),
     &      ETA2(1),CETA(1),PHI2(1),CPHI(1),ET2(2),CET(2),ETA2(2),
     &      CETA(2),PHI2(2),CPHI(2)
   10     FORMAT(X,A2,X,I2,2X,I2,A,X,F7.2,A,4X,6(X,F7.2,A))
C
        WRITE(LUN,*) '   PR   NO     PHI     - JET FARTHEST FROM MET  ',
     &    '-  - JET CLOSEST TO LEADING-'
        WRITE(LUN,*) '   NO   JT     MET        ET      ETA       PHI ',
     &    '      ET      ETA      PHI'
C write simulation data
        WRITE(LUN,20) 'SM',I,NUMJET1,CNUMJET,PHIMET1,CPHIMET,ET1(3),
     &    CET(3),ETA1(3),CETA(3),PHI1(3),CPHI(3),ET1(4),CET(4),ETA1(4),
     &    CETA(4),PHI1(4),CPHI(4)
C write out on-line data
        WRITE(LUN,20) 'DT',I,NUMJET2,CNUMJET,PHIMET2,CPHIMET,ET2(3),
     &    CET(3),ETA2(3),CETA(3),PHI2(3),CPHI(3),ET2(4),CET(4),ETA2(4),
     &    CETA(4),PHI2(4),CPHI(4)
   20   FORMAT(X,A2,X,I2,2X,I4,A,X,F7.2,A,2X,6(X,F7.2,A))
C          GOTO 999
        ENDIF
      ENDDO
C
      OKOK = .TRUE.
      RETURN
C
  999 CONTINUE
C
C     things are amiss
C
      RETURN
C
      END
