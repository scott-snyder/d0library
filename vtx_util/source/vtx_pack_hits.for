      SUBROUTINE VTX_PACK_HITS(LVTXT,HITS,ARRAY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Pack VTX hits-on-tracks information into ARRAY
C-
C-   Inputs  : LVTXT  = pointer to VTXT bank
C-   Outputs : HITS = Total number of matched hits on track
C-             ARRAY(1,HITS) = bits  0-15: time(end=0)
C-                             bits 16-31: time(end=1)
C-             ARRAY(2,HITS) = bits  0-31: Packed hit end=0 (see VWDA word 6)
C-             ARRAY(3,HITS) = bits  0-31: Packed hit end=1   "    "    "
C-             ARRAY(4,HITS) = bits  0-9 : Peak (end=0)
C-                             bits 10-21: drift time
C-                             bits 22-31: Wire address: 256*lay+8*sec+wire
C-             ARRAY(5,HITS) = bits  0-9 : Peak (end=1)
C-                             bits 10-21: Interpolated z-position from CDC/FDC
C-                             bits 22-31: Gain constant 
C-   Controls: none
C-
C-   Created  14-JAN-1994   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVTTH.LINK'
C LOCALS
      INTEGER LVTXT,HITS,ARRAY(5,24)
C LOCALS
      REAL    STAGGR
      PARAMETER (STAGGR = 0.01)
      REAL    DZDR,ZVTX,DRFT,X,Y,Z,R,TZERO,TIME(0:1),PEAK(0:1),ENV
      INTEGER LVTTH,NHITS,OLDLAY,OLDSEC,I,IADD,LAY,SEC,WIR,LR,HIT,WEND
      INTEGER LVWDA,LVSEC,LVALS,LVTMW,LVGNL,NWVSEC,NWVWDA,PNT,STAT,INT
      INTEGER HITNO,PNTV,PACK(0:1)
C EXTERNALS:
      INTEGER GZVWDA,GZVSEC,GZVALS,GZVTMW,GZVGNL
C----------------------------------------------------------------------
      HITS = 0
      DZDR = Q(LVTXT+14)
      ZVTX = Q(LVTXT+15)
      LVTTH = LQ(LVTXT-IZVTTH)
      NHITS = IQ(LVTXT+2)
      OLDLAY = -1
      DO I = 0,NHITS-1
        IADD = IQ(LVTTH+6+4*I)
        LAY = IBITS(IADD,9,2)
        SEC = IBITS(IADD,4,5)
        WIR = IBITS(IADD,1,3)
        LR  = IBITS(IADD,0,1)
        HIT = IQ(LVTTH+7+4*I)
        IF ((LAY .NE. OLDLAY) .OR. (SEC .NE. OLDSEC)) THEN
          LVWDA = GZVWDA(LAY,SEC)
          LVSEC = GZVSEC(LAY,SEC)
          LVALS = GZVALS(LAY,SEC)
          LVTMW = GZVTMW(LAY)
          LVGNL = GZVGNL(LAY)
          NWVSEC = IQ(LVSEC+3)
          NWVWDA = IQ(LVWDA+3)
          OLDLAY=LAY
          OLDSEC=SEC
        ENDIF
        PNT = LVSEC + IQ(LVSEC+12+WIR) + NWVSEC*(HIT-1)
        DRFT= Q(PNT+1+LR) - STAGGR*(1-2*MOD(WIR,2))*(1-2*MOD(SEC,2))
        STAT = IBITS(IQ(PNT+9),0,2)
        IF (STAT .EQ. 3) THEN
          HITS = HITS + 1
          X = C(LVALS+7+7*WIR) + C(LVALS+3)*DRFT
          Y = C(LVALS+8+7*WIR) + C(LVALS+4)*DRFT
          Z = ZVTX + DZDR*SQRT(X**2+Y**2)
          X = X + C(LVALS+10+7*WIR)*Z + C(LVALS+12+7*WIR)*Z*Z
          Y = Y + C(LVALS+11+7*WIR)*Z + C(LVALS+13+7*WIR)*Z*Z
          R = SQRT(X*X + Y*Y)
          DO WEND = 0,1
            TZERO = C(LVTMW + (8*SEC+WIR)*IC(LVTMW+3)+2*WEND+6)
            HITNO = IBITS(IQ(PNT+9),16+8*WEND,8)
            PNTV = LVWDA + IQ(LVWDA+20+2*WIR+WEND) + 8*(HITNO-1)
            TIME(WEND)  = Q(PNTV+1)-TZERO
            PEAK(WEND)  = Q(PNTV+4)
            CALL UCOPY(Q(PNTV+5),PACK(WEND),1)
          ENDDO
C -->FILL WORD 1
          INT = MIN0(2**16-1,MAX0(0,NINT((TIME(0)+500.)*10.)))
          ARRAY(1,HITS) = INT
          INT = MIN0(2**16-1,MAX0(0,NINT((TIME(1)+500.)*10.)))
          CALL MVBITS(INT,0,16,ARRAY(1,HITS),16)
C -->FILL WORDS 2 AND 3
          ARRAY(2,HITS) = PACK(0)
          ARRAY(3,HITS) = PACK(1)
C -->FILL WORD 4
          INT = MIN0(2**10-1,MAX0(0,NINT(  PEAK(0)        )))
          ARRAY(4,HITS) = INT
          INT = MIN0(2**12-1,MAX0(0,NINT( (DRFT+2.)*1000.  )))
          CALL MVBITS(INT,0,12,ARRAY(4,HITS),10)
          INT  = 256*LAY + 8*SEC + WIR
          CALL MVBITS(INT,0,10,ARRAY(4,HITS),22)
C --> FILL WORD 5
          INT = MIN0(2**10-1,MAX0(0,NINT(  PEAK(1)         )))
          ARRAY(5,HITS) = INT
          INT = MIN0(2**12-1,MAX0(0,NINT( (Z+60.)*30.      )))
          CALL MVBITS(INT,0,12,ARRAY(5,HITS),10)
          CALL VTX_GAIN0(LAY,SEC,WIR,ENV)
          INT = MIN0(2**10-1,MAX0(0,NINT( ENV*500.         )))
          CALL MVBITS(INT,0,10,ARRAY(5,HITS),22)
        ENDIF
      ENDDO
  999 RETURN
      END
