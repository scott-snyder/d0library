      SUBROUTINE C2EMFL(LL2EM,TT,NL1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FILL C2EM BANK FROM L2EM BANK
C-
C-   Inputs  : LL2EM = LINK TO L2EM;
C-             FOR EACH L1 CANDIDATE SEEN SO FAR:
C-              TT(1,*) = TT ETA,
C-              TT(2,*) = TT PHI,
C-              TT(3,*) = PAR SET OFFSET (*NR + X FOR C2EM POSITION, THUS
C-                          STARTS AT ZERO SINCE USED AS MULTIPLIER)
C-              TT(4,*) = INDEX INTO RESERVED LINK ARRAY (C2EM_LRLINK);
C-             NL1 = NUMBER OF L1 CANDIDATES (TT'S) SEEN SO FAR;
C-             LINDX = NEXT INDEX INTO RESERVED LINK ARRAY
C-   Outputs : NONE
C-   Controls: NO CONTROL!
C-
C-   Created  12-JAN-1994   James T. McKinley
C-   Modified 10-MAY-1994   James T. McKinley - Found bug in copy of cone
C-                          fraction.  Was using TT(4,L1PTR)*NR instead of 
C-                          TT(3,L1PTR)*NR for the repitition offset. Apparently
C-                          the wrong code got released, so needed to clean
C-                          some things up (i.e. 2 variables doing the same
C-                          thing...).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:C2EM_ZLINKA.INC'
      INTEGER LL2EM,TT(3,NL1MAX)
      INTEGER L1PTR,NL1,LC2EM,RPOINT
C-----------------------------------------------------------------------------
C check all L1 candidates seen so far to see if need to book C2EM bank
C-----------------------------------------------------------------------------
      DO L1PTR=1,NL1                    ! loop over known L1 candidates
        IF((IQ(LL2EM+4).EQ.TT(1,L1PTR)).AND.(IQ(LL2EM+5).EQ.
     &      TT(2,L1PTR))) GOTO 1        ! bank already booked for this guy
      ENDDO                             ! so just add repetition bank
C
C if get here need to book a new C2EM bank
C
      NL1 = NL1 + 1                     ! L1 canididate counter
      L1PTR = NL1                       ! L1 candidate index
C
      IF (NL1.GT.NPMAX) THEN
        CALL ERRMSG('C2EM_TOO_MANY','C2EMFL','Too Many Candidates',
     &      'W')
        GO TO 999
      ENDIF
      TT(1,NL1) = IQ(LL2EM+4)           ! TETA of L1 candidate
      TT(2,NL1) = IQ(LL2EM+5)           ! TPHI of L1 candidate
C
      CALL BKC2EM(LC2EM)                ! book a new bank for new L1 cand.
      IF(LC2EM.LE.0)THEN                ! abort if no C2EM bank
        CALL ERRMSG('C2EM BANK PROBLEM','C2EMFL',
     &      'cannot make C2EM bank!','W')
        GOTO 999
      ENDIF
C                                       ! reserve the link so that
      C2EM_LRLINK(L1PTR+1) = LC2EM      ! it is still valid later on
      LL2EM = C2EM_LRLINK(IL2EM)        ! get L2EM link again to be safe
C
C now fill C2EM bank
C-----------------------------------------------------------------------------
C *** FIXED PART OF C2EM BANK ***
C-----------------------------------------------------------------------------
      IQ(LC2EM+5) = IQ(LL2EM+4)         ! TETA
      IQ(LC2EM+6) = IQ(LL2EM+5)         ! TPHI
      IQ(LC2EM+7) = IQ(LL2EM+6)         ! IETA
      IQ(LC2EM+8) = IQ(LL2EM+7)         ! IPHI
      IQ(LC2EM+9) = IQ(LL2EM+8)         ! LYR
      IF(IQ(LL2EM+1).GT.3)THEN
        Q(LC2EM+10) =  Q(LL2EM+30)      ! AETA
      ELSE
        Q(LC2EM+10) =  -999999.0        ! NO AETA
      ENDIF
      IF(IQ(LL2EM+1).GE.4)THEN
        Q(LC2EM+11) =  Q(LL2EM+37)      ! ZETA
      ELSE
        Q(LC2EM+11) =  -999999.0        ! NO ZETA
      ENDIF
      IF(IQ(LL2EM+1).GE.3)THEN
        Q(LC2EM+12) =  Q(LL2EM+31)      ! APHI
        Q(LC2EM+13) =  Q(LL2EM+32)      ! XCLUS
        Q(LC2EM+14) =  Q(LL2EM+33)      ! YCLUS
        Q(LC2EM+15) =  Q(LL2EM+34)      ! ZCLUS
      ELSE
        Q(LC2EM+12) =  -999999.0        ! NO APHI
        Q(LC2EM+13) =  -999999.0        ! NO XCLUS
        Q(LC2EM+14) =  -999999.0        ! NO YCLUS
        Q(LC2EM+15) =  -999999.0        ! NO ZCLUS
      ENDIF
      Q(LC2EM+16) =  Q(LL2EM+9)         ! ET
      IF(IQ(LL2EM+1).GE.3)THEN
        Q(LC2EM+17) =  Q(LL2EM+35)      ! ET_ZCORR
      ELSE
        Q(LC2EM+17) =  -999999.0        ! NO ET_ZCORR
      ENDIF
      Q(LC2EM+18) =  Q(LL2EM+10)        ! SUMEM
      Q(LC2EM+19) =  Q(LL2EM+11)        ! EM1/SUMEM
      Q(LC2EM+20) =  Q(LL2EM+12)        ! (EM1+EM2)/SUMEM
      Q(LC2EM+21) =  Q(LL2EM+13)        ! EM3/SUMEM
      Q(LC2EM+22) =  Q(LL2EM+14)        ! EM4/SUMEM
      Q(LC2EM+23) =  Q(LL2EM+15)        ! FH1/SUMEM
      Q(LC2EM+24) =  Q(LL2EM+16)        ! SIGMA3
      Q(LC2EM+25) =  Q(LL2EM+17)        ! SIGMA5
      Q(LC2EM+26) =  Q(LL2EM+18)        ! SIG3+MID
      Q(LC2EM+27) =  Q(LL2EM+19)        ! SH13
      Q(LC2EM+28) =  Q(LL2EM+20)        ! SH24
      Q(LC2EM+29) =  Q(LL2EM+21)        ! SH35
      Q(LC2EM+30) =  Q(LL2EM+22)        ! SH57
C-----------------------------------------------------------------------------
C *** C2EM REPETITION BANKS ***
C-----------------------------------------------------------------------------
    1 CONTINUE                          ! skip to here if already had C2EM bank
C                                       ! new C2EM bank just continues
C
      LC2EM = C2EM_LRLINK(L1PTR+1)      ! get C2EM link (L1PTR+1 since 1->L2EM)

      IF(TT(3,L1PTR).GE.NPMAX)THEN      ! need more space than booked in C2EM
        CALL MZPUSH(IXMAIN,LC2EM,0,NR,' ')
      ENDIF
C
      LC2EM = C2EM_LRLINK(L1PTR+1)      ! get links again just to be safe
      LL2EM = C2EM_LRLINK(IL2EM)        ! push might trigger a garbage collection
C
      RPOINT = LC2EM + NFIX + TT(3,L1PTR)*NR
C
      IQ(RPOINT + 1) = IQ(LL2EM+29)     ! PARSET
      IQ(RPOINT + 2) = IQ(LL2EM+27)     ! NTRAK
      IQ(RPOINT + 3) = IQ(LL2EM+28)     ! IFAILED
      IF(IQ(LL2EM+1).GE.3)THEN
        IQ(RPOINT + 4) = IQ(LL2EM+36)   ! CUTBITS
      ELSE
        IQ(RPOINT + 4) = -1             ! NO CUTBITS
      ENDIF
      Q(RPOINT + 5) =  Q(LL2EM+23)      ! CONE_R
      Q(RPOINT + 6) =  Q(LL2EM+24)      ! F_CONE_ET
      IF(IQ(LL2EM+27).EQ.999)THEN
        Q(RPOINT + 7) = 0.0             ! clean up prob. with
        Q(RPOINT + 8) = 0.0             ! DETA,DPHI for no
      ELSE                              ! track match
        Q(RPOINT + 7) =  Q(LL2EM+25)    ! DETA
        Q(RPOINT + 8) =  Q(LL2EM+26)    ! DPHI
      ENDIF
C
      TT(3,L1PTR) = TT(3,L1PTR) + 1     ! increment p.s. counter for next p.s.
C                                       ! on this L1 cand.
  999 RETURN
      END

