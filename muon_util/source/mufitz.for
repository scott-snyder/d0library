      SUBROUTINE MUFITZ(NP,MODI,MOD,PLN,WIR,IS,XH,YH,ZH,TH,ANGLE,
     A  GAIN,XDR,DEL012,DEL123,CHI4,DELTA_T0,LWIR)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCC CHANGEABLE ROUTINE TO PLAY WITH HITS ON TRACKS--BEND VIEW
CCCC       SETUP FOR 3 OR 4-PLANE MODULES
CCCC INPUT : NP = Number of points
CCCC         MODI = module of interest; if = 0 then ignore
CCCC         MOD,PLN,WIR  =  Module, plane and wire for each hit
CCCC         IS  =  Drift time solution
CCCC         XH,YH,ZH = Coordinates of wires
CCCC         TH = Drift times
CCCC         ANGLE = Angle w.r.t. wire plane
CCCC         XDR = Drift distance
CCCC
CCCC    OUTPUT: DEL012: MINIMUM 3-MISS PLANES 012
CCCC            DEL123: MINIMUM 3-MISS PLANES 123
CCCC            CHI4:   RESIDUAL RESOLUTION IF 4 POINTS
CCCC             WITH TIME OFFSET CONSTRAINED
CCCC            DELTA_T0:  calculated correction to t0 (ADC counts)
CCCC            LWIR: WIRE NUMBER OF GOOD COMBINATION
CCCC  DH 12/89
CC    DH  4/91 minor bug
CC    DH 3/93 allow 3 adjacent cells
CC    RM 5/94 modifed for new T0 tuning code
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NP,NN,I110,I111,I112,I113,I,LWIR,NH(24)
      INTEGER SI,MODI,MODA,LASTRUN(12),LASTEV(12),COUNT
      PARAMETER (SI=40)
      REAL XH(SI),YH(SI),ZH(SI),TH(SI),ANGLE(SI),XDR(SI)
      REAL ANGLE4(0:3), XDR4(0:3),GAIN4(0:3)
      INTEGER MOD(SI),PLN(SI),WIR(SI),IS(SI)
      INTEGER J(0:3),K
      REAL DEL,DEL012,DEL123,CHI4,DELTA_T0,GAIN(SI)
C
CCCC    KLUGE ON MODULE NUMBERING
      MODA=MODI
      IF(MOD(1).EQ.1) MODA=0
      DEL012=-999999.
      DEL123=-999999.
      CHI4=-999999.
      DELTA_T0 = 999999.
      DO I=1,24
        NH(I)=0
      ENDDO
CCC   FIND MOST COMMON CELL NUMBER
      DO I=1,NP
        IF(MODA.NE.0) THEN
          IF(IABS(IS(I)).EQ.1.AND.MODA.EQ.MOD(I)) THEN
            NH(WIR(I)+1)=NH(WIR(I)+1)+1
          ENDIF
        ELSE
          IF(IABS(IS(I)).EQ.1) THEN
            NH(WIR(I)+1)=NH(WIR(I)+1)+1
          ENDIF
        ENDIF
      ENDDO
      LWIR=-1
      DO I=1,24
        IF(NH(I).EQ.4) LWIR=I-1
      ENDDO
      IF(LWIR.EQ.-1) THEN
        DO I=1,24
          IF(NH(I).EQ.3) LWIR=I-1
        ENDDO
      ENDIF
      IF(LWIR.EQ.-1) THEN
        DO I=1,24
          IF(NH(I).EQ.2) LWIR=I-1
        ENDDO
      ENDIF
      IF(LWIR.EQ.-1) THEN
        DO I=1,22
          IF(NH(I)+NH(I+1)+NH(I+2).EQ.3) LWIR=I
        ENDDO
      ENDIF
      IF(LWIR.EQ.-1) RETURN
CCC   AT LEAST A 2-DECK COMBO
      I110=0
      I111=0
      I112=0
      I113=0
      DO I=1,NP
        IF(MODA.NE.0) THEN
          IF(IABS(IS(I)).EQ.1.AND.MODA.EQ.MOD(I)) THEN
            NN=IABS(WIR(I)-LWIR)
            IF(PLN(I).EQ.0.AND.NN.LE.1) I110=I
            IF(PLN(I).EQ.1.AND.NN.LE.1) I111=I
            IF(PLN(I).EQ.2.AND.NN.LE.1) I112=I
            IF(PLN(I).EQ.3.AND.NN.LE.1) I113=I
          ENDIF
        ELSE
          IF(IABS(IS(I)).EQ.1) THEN
            NN=IABS(WIR(I)-LWIR)
            IF(PLN(I).EQ.0.AND.NN.LE.1) I110=I
            IF(PLN(I).EQ.1.AND.NN.LE.1) I111=I
            IF(PLN(I).EQ.2.AND.NN.LE.1) I112=I
            IF(PLN(I).EQ.3.AND.NN.LE.1) I113=I
          ENDIF
        ENDIF
      ENDDO
C
      IF (I110.NE.0.AND.I111.NE.0.AND.I112.NE.0.AND.I113.NE.0) THEN
C
C------ We have a four-deck chamber with all four layers hit
C
        XDR4(0) = XDR(I110)
        XDR4(1) = XDR(I111)
        XDR4(2) = XDR(I112)
        XDR4(3) = XDR(I113)
        ANGLE4(0) = ANGLE(I110)
        ANGLE4(1) = ANGLE(I111)
        ANGLE4(2) = ANGLE(I112)
        ANGLE4(3) = ANGLE(I113)
        GAIN4(0) = GAIN(I110)
        GAIN4(1) = GAIN(I111)
        GAIN4(2) = GAIN(I112)
        GAIN4(3) = GAIN(I113)
C
C------ Find the DELTA_T0 that gives the closest fit to a straight line
C
        CALL MDRF4(ZH(I110),ZH(I111),ZH(I112),ZH(I113),XDR4,
     &    YH(I110),YH(I111),YH(I112),
     &    YH(I113),TH(I110),TH(I111),TH(I112),TH(I113),
     &    ANGLE4,GAIN4,CHI4,DEL012,DELTA_T0)
C
      ELSEIF (I110*I111*I112.NE.0 .AND. MODA .GE. 100) THEN
C
C------ Solve for the DELTA_T0 that gives a straight line for 3 hits
C
        CALL MDRF3(ZH(I110),ZH(I111),ZH(I112),XDR(I110),
     &    XDR(I111),XDR(I112),YH(I110),YH(I111),YH(I112),
     &    TH(I110),TH(I111),TH(I112),ANGLE(I110),ANGLE(I111),
     &    ANGLE(I112),GAIN(I110),GAIN(I111),GAIN(I112),
     &    DEL,DELTA_T0)
        IF (DELTA_T0 .NE. 999999.) DEL012 = DEL         ! 3-miss residual
      ENDIF
C
  999 CONTINUE
C
      RETURN
      END
