      SUBROUTINE L2_COMP_LETA(LUN,LLETA,IB,OKOK,LETA_DETA,LETA_ET,
     &    LETA_ETA,LETA_PHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  compare LETA banks 
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
      INTEGER ISTAT2,IOBJTYP1,IOBJTYP2,NUMOBJ1,NUMOBJ2,IP1,IP2,NRUN,NEV
      INTEGER J,LLETA(*),IB,LUN
      REAL DETA1,DETA2,ET1(2),ET2(2),ETA1(2),ETA2(2),PHI1(2),PHI2(2)
      REAL LETA_DETA,LETA_ET,LETA_ETA,LETA_PHI
      LOGICAL LET(2),LETA(2),LPHI(2),LVERSNO,LNOREPW,LNOPARS,LSTAT,LDETA
      LOGICAL LDUMP,L2_NXT_REAL_DIFF,L2_NXT_INT_DIFF,LOBJTYP,LNUMOBJ
      LOGICAL OKOK
      CHARACTER*1 CVERSNO,CNOPARS,CNOREPW,CSTAT,COBJTYP,CNUMOBJ,CDETA
      CHARACTER*1 CET(2),CETA(2),CPHI(2)
C
      DATA CVERSNO/' '/,CNOPARS/' '/,CNOREPW/' '/,CSTAT/' '/,CET(1)/' '/
      DATA COBJTYP/' '/,CNUMOBJ/' '/,CDETA/' '/,CPHI(2)/' '/
      DATA CETA(1)/' '/,CPHI(1)/' '/,CET(2)/' '/,CETA(2)/' '/
C----------------------------------------------------------------------
C
      OKOK = .FALSE.
      NRUN = IQ(LHEAD+6)
      NEV = IQ(LHEAD+9)
C
      IF (IB.NE.2) THEN
        CALL ERRMSG('L2_COMPARE','L2_COMP_LETA',
     &    'L2_COMP_LETA called for IB.NE.2 ! Not allowed!!!','F')
        RETURN
      ENDIF
C
C     make sure there is LETA - no mistakes
C
      IF (LLETA(1).LE.0) THEN
        WRITE(LUN,'('' L2_COMP_LETA: Run/Event '',2I7,
     &      '' has NO LETA bank for SIMULATION'')') NRUN,NEV
        GOTO 999
      ENDIF
      IF (LLETA(2).LE.0) THEN
        WRITE(LUN,'('' L2_COMP_LETA: Run/Event '',2I7,
     &      '' has NO LETA bank for DATA'')') NRUN,NEV
        GOTO 999
      ENDIF
C check version number
      IP1 = LLETA(1)
      IP2 = LLETA(2)
      LVERSNO = L2_NXT_INT_DIFF(IP1,IP2,IVERSNO1,IVERSNO2,CVERSNO)
C check number of parameter sets
      LNOPARS = L2_NXT_INT_DIFF(IP1,IP2,NOPARS1,NOPARS2,CNOPARS)
C check number of repeated words
      LNOREPW = L2_NXT_INT_DIFF(IP1,IP2,NOREPW1,NOREPW2,CNOREPW)
C jump out if disagreement is found.
      LDUMP = LVERSNO .OR. LNOPARS .OR. LNOREPW
      IF (LDUMP) THEN
        WRITE(LUN,'(/,'' L2_COMP_LETA:  RUN/EVENT '',2I7,
     &        '' LETA entry discrepancy'',/,
     &        '' "*" denote variables whose differences are '',
     &        ''outside of tolerances'')') NRUN,NEV
        WRITE(LUN,*) '       VERSNO   NOPARS   NOREPW'
        WRITE(LUN,'('' SIM'',3(5X,I3,A))') IVERSNO1,CVERSNO,NOPARS1,
     &    CNOPARS,NOREPW1,CNOREPW
        WRITE(LUN,'('' DAT'',3(5X,I3,A))') IVERSNO2,CVERSNO,NOPARS2,
     &    CNOPARS,NOREPW2,CNOREPW
        GOTO 999
      ENDIF
C loop over all parameter sets
      DO I = 1, NOPARS1
C check status word
        LSTAT = L2_NXT_INT_DIFF(IP1,IP2,ISTAT1,ISTAT2,CSTAT)
C check object type
        LOBJTYP = L2_NXT_INT_DIFF(IP1,IP2,IOBJTYP1,IOBJTYP2,COBJTYP)
C check number of objects in the event
        LNUMOBJ = L2_NXT_INT_DIFF(IP1,IP2,NUMOBJ1,NUMOBJ2,CNUMOBJ)
C check delta eta of pair
        LDETA = L2_NXT_REAL_DIFF(IP1,IP2,DETA1,DETA2,LETA_DETA,CDETA)
C check Et of jth object
        DO J=1,2
          LET(J) = L2_NXT_REAL_DIFF(IP1,IP2,ET1(J),ET2(J),LETA_ET,      
     &      CET(J))
C check eta of jth object
          LETA(J) = L2_NXT_REAL_DIFF(IP1,IP2,ETA1(J),ETA2(J),LETA_ETA,
     &      CETA(J))
C check phi of jth object
          LPHI(J) = L2_NXT_REAL_DIFF(IP1,IP2,PHI1(J),PHI2(J),LETA_PHI,
     &      CPHI(J))
        END DO
C check to see if disagreement found
        LDUMP = LSTAT.OR.LOBJTYP.OR.LNUMOBJ.OR.LDETA.OR.LET(1).OR.
     &    LETA(1).OR.LPHI(1).OR.LET(2).OR.LETA(2).OR.LPHI(2)
        IF (LDUMP) THEN
          WRITE(LUN,'(/,'' L2_COMP_LETA:  RUN/EVENT '',2I7,
     &      '' LETA entry discrepancy'',/,
     &      '' "*" denote variables whose differences is '',
     &      ''outside of tolerances'')') NRUN,NEV
          WRITE(LUN,*) '  PR  ST  O  NO   DELTA   ',
     &      '----------- 1 -----------  ----------- 2 -----------'
          WRITE(LUN,*) '  NO  AT  B  OB    ETA    ',
     &      '    ET      ETA      PHI       ET      ETA      PHI'
C write simulation data
          WRITE(LUN,10) 'SM',I,ISTAT1,CSTAT,IOBJTYP1,COBJTYP,NUMOBJ1,
     &      CNUMOBJ,DETA1,CDETA,ET1(1),CET(1),ETA1(1),CETA(1),PHI1(1),
     &      CPHI(1),ET1(2),CET(2),ETA1(2),CETA(2),PHI1(2),CPHI(2)
C write out on-line data
          WRITE(LUN,10) 'DT',I,ISTAT2,CSTAT,IOBJTYP2,COBJTYP,NUMOBJ2,
     &      CNUMOBJ,DETA2,CDETA,ET2(1),CET(1),ETA2(1),CETA(1),PHI2(1),
     &      CPHI(1),ET2(2),CET(2),ETA2(2),CETA(2),PHI2(2),CPHI(2)
C
   10     FORMAT(A2,X,I2,2X,I2,A,X,I1,A,X,I2,A,X,F7.2,A,
     &      2(X,F7.2,A,X,F7.2,A,X,F7.2,A))
          GOTO 999
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
