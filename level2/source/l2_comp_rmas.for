      SUBROUTINE L2_COMP_RMAS(LUN,LRMAS,IB,OKOK,RMAS_MASS,RMAS_ETAB,
     &    RMAS_ET,RMAS_ETA,RMAS_PHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  compare RMAS banks 
C
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   8-JUN-1994   Lewis Taylor Goss
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:HEADER.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER IVERSNO1,IVERSNO2,NOREPW1,NOREPW2,NOPARS1,NOPARS2,ISTAT1,I
      INTEGER ISTAT2,IOBJTYP1,IOBJTYP2,NUMOBJ1,NUMOBJ2,IP1,IP2,NRUN,NEV
      INTEGER J,LRMAS(*),IB,LUN
      REAL ETAB1,ETAB2,ET1(2),ET2(2),ETA1(2),ETA2(2),PHI1(2),PHI2(2)
      REAL RMAS1,RMAS2,RMAS_MASS,RMAS_ETAB,RMAS_ET,RMAS_ETA,RMAS_PHI
      LOGICAL LET(2),LETA(2),LPHI(2),LVERSNO,LNOREPW,LNOPARS,LSTAT,LMAS
      LOGICAL LDUMP,L2_NXT_REAL_DIFF,L2_NXT_INT_DIFF,LOBJTYP,LNUMOBJ
      LOGICAL LETAB,OKOK
      CHARACTER*1 CVERSNO,CNOPARS,CNOREPW,CSTAT,COBJTYP,CNUMOBJ,CMAS
      CHARACTER*1 CETAB,CET(2),CETA(2),CPHI(2)
C
      DATA CVERSNO/' '/,CNOPARS/' '/,CNOREPW/' '/,CSTAT/' '/,CET(1)/' '/
      DATA COBJTYP/' '/,CNUMOBJ/' '/,CMAS/' '/,CETAB/' '/,CPHI(2)/' '/
      DATA CETA(1)/' '/,CPHI(1)/' '/,CET(2)/' '/,CETA(2)/' '/
C----------------------------------------------------------------------
C
      OKOK = .FALSE.
      NRUN = IQ(LHEAD+6)
      NEV = IQ(LHEAD+9)
C
      IF (IB.NE.2) THEN
        CALL ERRMSG('L2_COMPARE','L2_COMP_RMAS',
     &    'L2_COMP_RMAS called for IB.NE.2 ! Not allowed!!!','F')
        RETURN
      ENDIF
C
C     make sure there is RMAS - no mistakes
C
      IF (LRMAS(1).LE.0) THEN
        WRITE(LUN,'('' L2_COMP_RMAS: Run/Event '',2I7,
     &      '' has NO RMAS bank for SIMULATION'')') NRUN,NEV
        GOTO 999
      ENDIF
      IF (LRMAS(2).LE.0) THEN
        WRITE(LUN,'('' L2_COMP_RMAS: Run/Event '',2I7,
     &      '' has NO RMAS bank for DATA'')') NRUN,NEV
        GOTO 999
      ENDIF
C check version number
      IP1 = LRMAS(1)
      IP2 = LRMAS(2)
      LVERSNO = L2_NXT_INT_DIFF(IP1,IP2,IVERSNO1,IVERSNO2,CVERSNO)
C check number of parameter sets
      LNOPARS = L2_NXT_INT_DIFF(IP1,IP2,NOPARS1,NOPARS2,CNOPARS)
C check number of repeated words
      LNOREPW = L2_NXT_INT_DIFF(IP1,IP2,NOREPW1,NOREPW2,CNOREPW)
C jump out if disagreement is found.
      LDUMP = LVERSNO .OR. LNOPARS .OR. LNOREPW
      IF (LDUMP) THEN
        WRITE(LUN,'(/,'' L2_COMP_RMAS:  RUN/EVENT '',2I7,
     &        '' RMAS entry discrepancy'',/,
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
C check reconstructed mass of pair
        LMAS = L2_NXT_REAL_DIFF(IP1,IP2,RMAS1,RMAS2,RMAS_MASS,CMAS)
C check etaboost of pair
        LETAB = L2_NXT_REAL_DIFF(IP1,IP2,ETAB1,ETAB2,RMAS_ETAB,CETAB)
C check Et of jth object
        DO J=1,2
          LET(J) = L2_NXT_REAL_DIFF(IP1,IP2,ET1(J),ET2(J),RMAS_ET,      
     &      CET(J))
C check eta of jth object
          LETA(J) = L2_NXT_REAL_DIFF(IP1,IP2,ETA1(J),ETA2(J),RMAS_ETA,
     &      CETA(J))
C check phi of jth object
          LPHI(J) = L2_NXT_REAL_DIFF(IP1,IP2,PHI1(J),PHI2(J),RMAS_PHI,
     &      CPHI(J))
        END DO
C check to see if disagreement found
        LDUMP = LSTAT.OR.LOBJTYP.OR.LNUMOBJ.OR.LMAS.OR.LETAB.OR.LET(1)
     &    .OR.LETA(1).OR.LPHI(1).OR.LET(2).OR.LETA(2).OR.LPHI(2)
        IF (LDUMP) THEN
          WRITE(LUN,'(/,'' L2_COMP_RMAS:  RUN/EVENT '',2I7,
     &      '' RMAS entry discrepancy'',/,
     &      '' "*" denote variables whose differences is '',
     &      ''outside of tolerances'')') NRUN,NEV
          WRITE(LUN,*) '     PAR          OB  NO          ETA   ',
     &      '-------- 1 -------  -------- 2 -------'
          WRITE(LUN,*) '     NUM    STAT  TY  OB   MASS  BOOST  ',
     &      '  ET     ETA   PHI    ET     ETA   PHI'
C write simulation data
          WRITE(LUN,10) 'SIM',I,ISTAT1,CSTAT,IOBJTYP1,COBJTYP,NUMOBJ1,
     &      CNUMOBJ,RMAS1,CMAS,ETAB1,CETAB,ET1(1),CET(1),ETA1(1),
     &      CETA(1),PHI1(1),CPHI(1),ET1(2),CET(2),ETA1(2),CETA(2),
     &      PHI1(2),CPHI(2)
C write out on-line data
          WRITE(LUN,10) 'DAT',I,ISTAT2,CSTAT,IOBJTYP2,COBJTYP,NUMOBJ2,
     &      CNUMOBJ,RMAS2,CMAS,ETAB2,CETAB,ET2(1),CET(1),ETA2(1),
     &      CETA(1),PHI2(1),CPHI(1),ET2(2),CET(2),ETA2(2),CETA(2),
     &      PHI2(2),CPHI(2)
C
   10     FORMAT(X,A3,2X,I3,2X,I6,A,2X,I1,A,X,I2,A,X,F5.1,A,X,F5.3,A,
     &      2(F6.2,A,X,F5.2,A,X,F4.2,A))
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
