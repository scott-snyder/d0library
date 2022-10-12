      SUBROUTINE DFSTRK(ILYR,ISCTR,IWIRE,NPULSE,HITLST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  27-AUG-1990   Srini Rajagopalan
C-            07-APR-1992   DC - protection against bad HITADR,HITLEN reads
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDCMAP.INC'
      INCLUDE 'D0$LINKS:IZCDD2.LINK'
C
      INTEGER EVTDAT(0:499)
      INTEGER ILYR,ISCTR,IWIRE,LABEL
      INTEGER LCHA,CHNL,BIN,INDEX,MAX_PULSE
      INTEGER POINT,END,HITLEN,HITADR
      INTEGER MASK8,MASK16,NPULSE
      INTEGER I,IFIRST,ILAST,IAREA,S
      INTEGER ISUML,ISUMF,IPED,FBIN
C
      REAL HITLST(5,0:2,0:15)
      REAL B(0:500),INCRE
      REAL THR1,THR2
      REAL SW_THR1,DL_THR1,SW_THR2,DL_THR2
      REAL SUM,SUMX,AREA
C
      data MASK8 / z'FF' /
      data MASK16 / z'FFFF' /
      DATA SW_THR1,SW_THR2,DL_THR1,DL_THR2 /3.0,5.0,6.0,10.0/
      DATA IPED /10/
C
C----------------------------------------------------------------------
C
      SUM = 0
      SUMX = 0
C
C  unpack channel length and channel number
C
      NPULSE = 0
      THR1 = SW_THR1
      THR2 = SW_THR2
      MAX_PULSE = 1
      IF (IWIRE.EQ.0 .OR. IWIRE.EQ.6) MAX_PULSE = 2
      IF (IWIRE.GT.6) THEN
        THR1 = DL_THR1
        THR2 = DL_THR2
        MAX_PULSE = 5
      ENDIF
      CALL DCODER(LABEL,ILYR,ISCTR,IWIRE,0,2)
      POINT = MAP(LABEL)              ! Get pointer to channel data
      LCHA = IAND(IQ(POINT), MASK16)
      IF (LCHA.LE.4) GO TO 999
      CHNL = IAND(ISHFT(IQ(POINT), -16), MASK16)
      IF (CHNL.NE.LABEL) GO TO 999
      END = POINT - LCHA/4 + 1
C
      DO WHILE (POINT.GT.END)
        POINT = POINT - 1
        HITLEN = IAND(IQ(POINT), MASK16)
        HITADR = IAND(ISHFT(IQ(POINT), -16), MASK16)
C
        IF (HITADR.GT.500) GOTO 999   ! Bad HITADR read out, skip event
        IF (HITLEN.GT.500) GOTO 290   ! Bad HITLEN exatrcted, go to next HIT
C
        DO INDEX = 1,HITLEN/4-1
          POINT = POINT - 1
          BIN = HITADR - (4*INDEX) + 1
          EVTDAT(BIN + 3) = IAND(IQ(POINT), MASK8)
          EVTDAT(BIN + 2) = IAND(ISHFT(IQ(POINT),  -8), MASK8)
          EVTDAT(BIN + 1) = IAND(ISHFT(IQ(POINT), -16), MASK8)
          EVTDAT(BIN)     = IAND(ISHFT(IQ(POINT), -24), MASK8)
        ENDDO
        FBIN = BIN
        B(BIN) = FLOAT(EVTDAT(BIN))
        DO 200 BIN = FBIN+1,HITADR
          B(BIN) = FLOAT(EVTDAT(BIN) - EVTDAT(BIN-1))
  200   CONTINUE
C
        ISUMF = FBIN
        IFIRST = FBIN
        ILAST = FBIN
  201   I = ILAST + 1
  202   IF (I .GT. HITADR) GO TO 300
        IF (B(I).GT.THR1) THEN
          IF (B(I-1).GT.THR1) THEN
            IF (EVTDAT(I).GE.IPED) THEN
              IF (B(I+1).GE.THR2 .OR. B(I)+B(I-1).GE.THR2) THEN
                IFIRST = I - 2
                IF (B(IFIRST) .LE. 0) IFIRST = I - 1
                ISUMF = I - 2
                GO TO 230
              ELSE
                I = I + 3
              ENDIF
            ELSE
              I = I + 1
            ENDIF
          ELSE
            I = I + 1
          ENDIF
        ELSE
          I = I + 2
        ENDIF
        GO TO 202
  230   CONTINUE
C
        INCRE = 0.
        DO 240 I = IFIRST,HITADR
          IF (B(I) .LE. 0) THEN
            ILAST = I
            ISUML = I
            GO TO 250
          ENDIF
          INCRE = INCRE + 1.
          SUM = SUM + B(I)
          SUMX = SUMX + INCRE*B(I)
  240   CONTINUE
        ILAST = HITADR
C
  250   CONTINUE
        IF ((EVTDAT(ILAST-1)-IPED).LT.20) GO TO 300
        S = ISUML
  260   S = S + 1
        IF (S+2 .GE. HITADR) THEN
          ISUML = HITADR
          GO TO 270
        ENDIF
C
        IF (-B(S).LE.THR1 .AND. -B(S+1).LE.THR1 .AND. B(S+2) .LE.THR1)
     &      THEN
          ISUML = S
          GO TO 270
        ENDIF
        GO TO 260
C
C *** Compute Area
C
  270   IAREA = 0
        DO 280 I = ISUMF,ISUML
          IAREA = IAREA + EVTDAT(I)
  280   CONTINUE
        AREA = FLOAT(IAREA) - ((ISUML-ISUMF+1)*IPED)
C
        IF (AREA.LT.40.) GO TO 300
C
C *** Drift Time and Pulse Area
C
        NPULSE = NPULSE + 1
        HITLST(NPULSE,0,IWIRE) = 10.*(SUMX/SUM + IFIRST + 0.5)    ! Drift time
        HITLST(NPULSE,1,IWIRE) = EVTDAT(ILAST) - IPED             ! Peak Height
        HITLST(NPULSE,2,IWIRE) = AREA                             ! Area
        IF (NPULSE.GE.MAX_PULSE) GO TO 999
        GO TO 201
  290   CONTINUE
        POINT = POINT - (HITLEN/4) + 1
  300   CONTINUE
      ENDDO
C
C
  999 RETURN
      END
