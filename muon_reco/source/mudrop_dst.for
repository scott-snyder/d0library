      FUNCTION MUDROP_DST()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To drop selected banks from DST. For now drops
C-                         MUOT & MUON banks that have no PMUO counterpart.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  24-AUG-1992   SHAHRIAR ABACHI
C-   Updated  24-NOV-1992   Cecilia Gerber - keep squeezed MUD1 in DSTs
C-   12-7-92 DH protection for poor MUOT tracks
C-   Updated  17-APR-1993   Daria Zieminska   call EZRSET
C-   Updated  01-jan-1994   D. Wood  Do MUON,MUOT dropping before squeezing,
C-                          don't count a-stubs in squeezing,change GZ loops
C-                          over MUON,MUOT
C-   Updated  06-jan-1994   D. Wood  Clean up MUOT,MUON bank numbering
C-   Updated  14-feb-1994   D. Wood  include missing modules along 
C-                                   with those on track.  Increase
C-                                   total module limit from 12 to 20
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL MUDROP_DST
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LMTRH,GZMTRH,LPMUO,GZPMUO,LMUOT2,GZMUOT,LMUD1,GZMUD1
      INTEGER NS,NTRK,I,J,NPMUO,NMUON,LMUON_NEXT,LMUOT_NEXT
      INTEGER L1_MUOT,L1_MUON,IMUOT,IMUON
      INTEGER LMUON2,GZMUON,LMUON(200),LMUOT(200)
C
      INTEGER IHOFF,IDHDUMP,IERR,NT,JERR,IMOD(200),MUVERT,IER
      INTEGER IT,NON,ION(10),NOFF,IOFF(10),I11,QD(20),QUAD1,QUAD2,
     &  QUAD3
      INTEGER NSAM,IFW3,IFW4,IZ(20),IMIN,NTOT,IMIN2,IMIN3
      REAL ELCAL,ELFE,SPR1,SPR2,DZ(20),THMIN
      INTEGER KK,NTRAKS,ITRAK,NPTRAK,QUAD,IFW1,IFW2, IFW4MAX,NGOOD_MUON
      REAL XI,YI,ZI,XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,
     &     YCOSOM,ZCOSOM,CHSQBV,CHSNBV,RMOM,RMOMER
      REAL ETAMAX, THETA, TANT2, ETA, EPS
      LOGICAL FIRST, SQ_MUD1
      DATA EPS /1.0E-5/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('MURECO_RCP')
        CALL EZGET('SQ_MUD1',SQ_MUD1,IER)
        CALL EZGET('IFW4MAX',IFW4MAX,IER)
        CALL EZGET('ETAMAX',ETAMAX,IER)
        CALL EZRSET
      ENDIF
C
      MUDROP_DST = .TRUE.
C
C drop unwanted MUOTs and MUONs
      LMTRH = GZMTRH(0)
      IF(LMTRH .EQ. 0) GOTO 999
      NTRK = IQ(LMTRH + 1)
      DO I=1,NTRK
        LMUOT(I) = 0
        LMUON(I) = 0
      ENDDO
C
C - MUOT & MUON banks with PMUO counterpart
C
      NPMUO = 0
      LPMUO = GZPMUO(0)
  100 CONTINUE
      IF (LPMUO .GT. 0) THEN
        NPMUO = NPMUO + 1
        NS = IQ(LPMUO - 2)
        LMUOT(NPMUO) = LQ(LPMUO - NS - 1)
        LMUON(NPMUO) = LQ(LPMUO - NS - 2)
        LPMUO = LQ(LPMUO)
        GOTO 100
      ENDIF
C
      LMUOT2 = GZMUOT(0)
      LMUOT_NEXT = 0
      DO I=1,NTRK
        IF(LMUOT2 .GT. 0) THEN
          LMUOT_NEXT = LQ(LMUOT2)
          DO J=1,NPMUO
            IF (LMUOT2 .EQ. LMUOT(J)) THEN
              GOTO 5
            ENDIF
          ENDDO
          CALL MZDROP(IXCOM,LMUOT2, ' ')
    5     CONTINUE
        ENDIF
        LMUOT2 = LMUOT_NEXT
      ENDDO
C
      LMUON2 = GZMUON(0)
      LMUON_NEXT = 0
      DO I=1,NTRK
        IF(LMUON2 .GT. 0) THEN
          LMUON_NEXT = LQ(LMUON2)
          DO J=1,NPMUO
            IF (LMUON2 .EQ. LMUON(J)) THEN
              GOTO 6
            ENDIF
          ENDDO
          CALL MZDROP(IXCOM,LMUON2, ' ')
    6     CONTINUE
        ENDIF
        LMUON2 = LMUON_NEXT
      ENDDO
C update count of MUOT/MUON banks
      LMTRH = GZMTRH(0)
      IF(LMTRH.GT.0) IQ(LMTRH+1) = NPMUO
C clean up numbering of MUOT/MUON banks
      L1_MUOT = GZMUOT(0)
      IMUOT = 0
      DO WHILE(L1_MUOT.GT.0)
        IMUOT = IMUOT + 1
        IQ(L1_MUOT-5) = IMUOT
        L1_MUOT = LQ(L1_MUOT)
      ENDDO
C
      L1_MUON = GZMUON(0)
      IMUON = 0
      DO WHILE(L1_MUON.GT.0)
        IMUON = IMUON + 1
        IQ(L1_MUON-5) = IMUON
        L1_MUON = LQ(L1_MUON)
      ENDDO
C
C Do MUD1 squeezing
      NGOOD_MUON = 0
C
      IF (SQ_MUD1) THEN
C
C  CHECK TRACK QUALITY
C
        CALL GTMTRH(NT)
        NTOT=0
        I11=0
        IF(NT.GE.1) THEN
          IMIN=-1
          THMIN=99999.
          DO ITRAK=1,NT                  ! loop over tracks
            CALL GTMUOT(ITRAK,NPTRAK,NSAM,QUAD,IFW1,IFW2,IFW3,IFW4,
     &         XI,YI,ZI,XMAGC,YMAGC,
     &         ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,YCOSOM,ZCOSOM,
     &         CHSQBV,CHSNBV,RMOM,RMOMER,ELCAL,ELFE,SPR1,SPR2)
            IF(IFW4.LE.2 .AND. MOD(IFW1,10).NE.5) THEN
C calculate eta of the track
              THETA = ACOS(ZCOSIM)
              IF(THETA .LT. EPS) THETA = THETA + EPS
              IF(THETA .GT. 3.1) THETA = THETA - EPS
              TANT2 = TAN(THETA/2.0)
              IF(TANT2 .GT. 0.0) THEN
                ETA = -ALOG(TANT2)
              ELSE
                ETA = 99.0
              ENDIF
C
              IF(ABS(ETA).LE.ETAMAX.AND.IFW4.LE.IFW4MAX) THEN
                NGOOD_MUON = NGOOD_MUON + 1
                I11=I11+1
                IF(I11.LE.20) THEN
                  QD(I11)=QUAD
                  DZ(I11)=ZCOSIM
                  IZ(I11)=ITRAK
                  IF(ABS(ZCOSIM).LT.THMIN) THEN
                    THMIN=ABS(ZCOSIM)
                    IMIN=I11
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDDO
          IF(I11.GT.20) I11=20
        ENDIF
C
        IF(I11.GE.1) THEN      ! SETUP LIST OF MODULES ON TRAKS
CCC    ORDER MODULES BY CLOSEST TO 90 DEGREES; UP TO 3 TRACKS
          CALL MUMISS(IZ(IMIN),NON,ION,NOFF,IOFF)
C include modules on track plus any missing modules
          NTOT = 0
          DO I=1,NON
            IMOD(NTOT+I)=ION(I)
          ENDDO
          NTOT = NTOT+NON
          DO I=1,NOFF
            IMOD(NTOT+I)=IOFF(I)
          ENDDO
          NTOT= NTOT+NOFF
          IF(I11.GE.2) THEN
            IMIN2=-1
            THMIN=99999.
            DO I=1,I11
              IF(I.NE.IMIN) THEN
                IF(ABS(DZ(I)).LT.THMIN) THEN
                  IMIN2=I
                  THMIN=ABS(DZ(I))
                ENDIF
              ENDIF
            ENDDO
            CALL MUMISS(IZ(IMIN2),NON,ION,NOFF,IOFF)
            IF(NTOT+NON+NOFF.LE.20) THEN
              DO I=1,NON
                IMOD(NTOT+I)=ION(I)
              ENDDO
              NTOT = NTOT+NON
              DO I=1,NOFF
                IMOD(NTOT+I)=IOFF(I)
              ENDDO
              NTOT= NTOT+NOFF
            ENDIF
          ENDIF
          IF(I11.GE.3) THEN
            IMIN3=-1
            THMIN=99999.
            DO I=1,I11
              IF(I.NE.IMIN.AND.I.NE.IMIN2) THEN
                IF(ABS(DZ(I)).LT.THMIN) THEN
                  IMIN3=I
                  THMIN=ABS(DZ(I))
                ENDIF
              ENDIF
            ENDDO
            CALL MUMISS(IZ(IMIN3),NON,ION,NOFF,IOFF)
            IF(NTOT+NON+NOFF.LE.20) THEN
              DO I=1,NON
                IMOD(NTOT+I)=ION(I)
              ENDDO
              NTOT = NTOT+NON
              DO I=1,NOFF
                IMOD(NTOT+I)=IOFF(I)
              ENDDO
              NTOT= NTOT+NOFF
            ENDIF
          ENDIF
          CALL MUD1SQ(NTOT,IMOD,JERR)
        ENDIF
      ENDIF
C
      IF (NGOOD_MUON.EQ.0) THEN
        LMUD1 = GZMUD1(0)
        IF(LMUD1.GT.0) CALL MZDROP(IXCOM,LMUD1,' ')
      ENDIF
C
  999 CONTINUE
C
      END
