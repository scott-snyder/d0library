      SUBROUTINE MUCTAG_TRK
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c      does the "opposite track" part of MUCTAG for all WAMUS
c      quadrants (MUCTAG only does CF)
C
C 01-dec-1993 D.Wood  based on MUCTAG
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL XO,YO,ZO,XCOS,YCOS,ZCOS ,XCS2,YCS2,ZCS2,DOT
      REAL XX,YY,ZZ,XP,YP,WLEN,X,Y,Z,DPHI,DTHET,TH1,TH2
      INTEGER I
      INTEGER JQUAD,IQUAD
      INTEGER IT,NTRACK,LMUOT,GZMUOT,JT,LMUOT2
      INTEGER NPT,IFW1,LPMUO,NS,LMUOT_TEST,GZPMUO
C
      EXTERNAL GZPMUO,GZMUOT
CC
      CALL GTMTRH(NTRACK)
      IF(NTRACK.EQ.0) RETURN
      DO 999 IT=1,NTRACK
        LMUOT = GZMUOT(IT)
        IF(LMUOT.EQ.0) GO TO 999
        IQUAD = IQ(LMUOT+3)
        IF(IQUAD.GE.13) GO TO 999         ! SKIP SAMUS
        IFW1 = IQ(LMUOT+4)
        IF(IFW1.EQ.5) GOTO 999           ! SKIP A LAYER STUBS
        NPT = IQ(LMUOT+1)
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
              IF(LMUOT2.GT.0) THEN
                IF(IQ(LMUOT2+3).LE.12.AND.Q(LMUOT2+20).LT.15..AND.
     &           IQ(LMUOT2+4).NE.5) THEN
                  XCS2 = Q(LMUOT2+17)
                  YCS2 = Q(LMUOT2+18)
                  ZCS2 = Q(LMUOT2+19)
                  DOT=XCOS*XCS2+YCOS*YCS2+ZCOS*ZCS2
                  IF(DOT.GT.1.) DOT=1.
                  IF(DOT.LT.-1.) DOT=-1.
                  DOT=ACOS(DOT)*180./3.14159
                  DPHI=(XCOS*XCS2+YCOS*YCS2)/SQRT(XCOS**2+YCOS**2)/
     &             SQRT(XCS2**2+YCS2**2)
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
C update muot flag 2
                    IQ(LMUOT+5)=IBSET(IQ(LMUOT+5),6)
C update pmuo flag 44 if present
                    LPMUO = GZPMUO(0)
                    DO WHILE(LPMUO.GT.0)
                      NS = IQ(LPMUO-2)
                      LMUOT_TEST = LQ(LPMUO-NS-1)
                      IF(LMUOT_TEST.EQ.LMUOT) THEN
                        IQ(LPMUO+44)=IBSET(IQ(LPMUO+44),6)
                      ENDIF
                      LPMUO = LQ(LPMUO)
                    ENDDO
                    GO TO 990
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDIF
990     CONTINUE
999   CONTINUE
      RETURN
      END
