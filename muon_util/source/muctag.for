      SUBROUTINE MUCTAG(IT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC   loops over MUOT tracks and tries to see if they are cosmics
CCC   only does central quadrants
CCC   flags IFW2 (LMUOT+5) in MUOT, bit 8 if 'cosmic' (no track)
CCC   flags IFW2 (LMUOT+5) in MUOT, bit 7 if 'cosmic' (track)
CCCC  D. Hedin 6/92
CC    TD 9/92 use all opposite hits. call mu_two_oct_track
CC    TD 6/93 Avoid A layer stubs in MUOT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL XO,YO,ZO,XCOS,YCOS,ZCOS ,XCS2,YCS2,ZCS2,DOT
      REAL XX,YY,ZZ,XP,YP,WLEN,X,Y,Z,DPHI,DTHET,TH1,TH2
      REAL EPS
      INTEGER GZMUHT,GZMUOH,LMUHT,LMUOH,NMUSRT,I,LP,IADD,ILAYER(4)
      INTEGER NMOD,NPLN,NWIR,IERR,JQUAD,LAYER,IQUAD,NTOT,NTOT2,NLR
      INTEGER IORENT,MULAYR,MUQUAD,IT,NTRACK,LMUOT,GZMUOT,JT,LMUOT2
      INTEGER NPT,IFW1
      LOGICAL ANSWER
      DATA EPS/0.0000001/
CC
      CALL GTMTRH(NTRACK)
      IF(NTRACK.EQ.0) RETURN
      LMUHT=GZMUHT(0)
      LMUOH=GZMUOH(0)
      NMUSRT=IQ(LMUHT+2)
C      DO 999 IT=1,NTRACK                ! Comment for single track tagging
        LMUOT = GZMUOT(IT)
        IF(LMUOT.EQ.0) GO TO 999
        IQUAD = IQ(LMUOT+3)
        IF(IQUAD.GE.5) GO TO 999         ! SKIP ENDS
        IFW1 = IQ(LMUOT+4)
        IF(IFW1.EQ.5) GOTO 999           ! SKIP A LAYER STUBS
        NPT = IQ(LMUOT+1)
        CALL MU_TWO_OCT_TRACK(IT,IQUAD,NPT,ANSWER)
        IF(ANSWER) IQ(LMUOT+5)=IBSET(IQ(LMUOT+5),8)
        IF(Q(LMUOT+20).GT.100.) GO TO 999 ! BAD FIT
CC  USE OUTSIDE FOR ALL FOR NOW
        XO = Q(LMUOT+11)
        YO = Q(LMUOT+12)
        ZO = Q(LMUOT+13)
        XCOS = Q(LMUOT+17)
        YCOS = Q(LMUOT+18)
        ZCOS = Q(LMUOT+19)
        IF(NTRACK.GT.1) THEN    ! SEE IF OPPOSITE TRACK
          DO JT=1,NTRACK
            IF(JT.NE.IT) THEN
              LMUOT2 = GZMUOT(JT)
              IF(IQ(LMUOT2+3).LE.4.AND.Q(LMUOT2+20).LT.15..AND.
     $          IQ(LMUOT2+4).NE.5) THEN
                XCS2 = Q(LMUOT2+17)
                YCS2 = Q(LMUOT2+18)
                ZCS2 = Q(LMUOT2+19)
                DOT=XCOS*XCS2+YCOS*YCS2+ZCOS*ZCS2
                IF(DOT.GT.1.) DOT=1.
                IF(DOT.LT.-1.) DOT=-1.
                DOT=ACOS(DOT)*180./3.14159
                DPHI=(XCOS*XCS2+YCOS*YCS2)/SQRT(XCOS**2+YCOS**2)/
     A               SQRT(XCS2**2+YCS2**2)
                IF(DPHI.GT.1.) THEN
                  DPHI=1.
                ELSE IF(DPHI.LT.-1.) THEN
                  DPHI=-1.
                ENDIF
                DPHI=ACOS(DPHI)*180./3.14159           ! 2D ANGLE
                TH1=ACOS(ZCOS)*180./3.14159
                TH2=ACOS(ZCS2)*180./3.14159
                IF(DPHI.LT.90.) THEN
                  DTHET=ABS(TH1-TH2)
                ELSE
                  DTHET=TH1+TH2
                  IF(DTHET.GT.180.) DTHET=360.-DTHET
                ENDIF
                IF(DPHI.GE.160.AND.DTHET.GE.170.) THEN
                  IQ(LMUOT+5)=IBSET(IQ(LMUOT+5),6)
                  GO TO 990
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDIF
990     CONTINUE
        DO I=1,4
          ILAYER(I)=0
        ENDDO
        NTOT=0
        NTOT2=0
CCC   LOOP OVER OTHER HITS
        DO 100 I=1,NMUSRT
          LP=28*(I-1)+LMUOH
C          IF(IQ(LP+3).NE.0) GO TO 100        ! ALREADY ON A TRACK
          IADD=IQ(LP+1)       ! WIRE ADDRESS
          CALL MUADD(IADD,NMOD,NPLN,NWIR,IERR)
CC   SEE IF IN RIGHT QUADRANT
          JQUAD=MUQUAD(NMOD)
          IF(JQUAD.GE.5) GO TO 100
          IF(JQUAD.EQ.IQUAD) GO TO 100
          LAYER=MULAYR(NMOD)
CC
          IORENT=IABS(IQ(LP+5))
          WLEN=Q(LP+24)
          XX=Q(LP+21)
          YY=Q(LP+22)
          ZZ=Q(LP+23)
          X=ZZ
          IF(IORENT.EQ.1) THEN
            IF(XCOS.EQ.0.) XCOS = EPS
            Y=YY
            XP=ZO+(XX-XO)*ZCOS/XCOS       ! BEND
            YP=YO+(XX-XO)*YCOS/XCOS       ! NONBEND
          ELSE IF(IORENT.EQ.2) THEN
            IF(YCOS.EQ.0.) YCOS = EPS
            Y=XX
            XP=ZO+(YY-YO)*ZCOS/YCOS     
            YP=XO+(YY-YO)*XCOS/YCOS
          ENDIF
        IF(ABS(YP-Y).GT.WLEN/2.+300.) GO TO 100   ! OUTSIDE CHAMBER
        IF(ABS(XP-X).LT.60.) THEN ! PROJECT IN BEND
          ILAYER(LAYER)=ILAYER(LAYER)+1
          IF(ABS(Y+Q(LP+17)-YP).LT.150.) THEN 
            NTOT=NTOT+1      ! PROJECT IN BOTH     
          ELSE
            NTOT2=NTOT2+1
          ENDIF
        ENDIF            
  100   CONTINUE
        NLR=0
        DO I=1,4
          IF(ILAYER(I).GT.0) NLR=NLR+1
        ENDDO
        IF((NLR.GE.2.AND.NTOT+NTOT2.GE.4).OR.
     A    NTOT.GE.3.OR.(NTOT.GE.2.AND.NTOT2.GE.2)) THEN   ! FLAG AS COSMIC
          IQ(LMUOT+5)=IBSET(IQ(LMUOT+5),7)
        ENDIF
999   CONTINUE
      RETURN
      END
