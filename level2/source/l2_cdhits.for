      SUBROUTINE L2_CDHITS(ROAD,ILYR,ISCTR,IWIRE,NPULSE,
     &  HIT,HITLST,TRGR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns list of hits on the specified channel
C-
C-   Inputs  : ILYR, ISCTR, IWIRE - CDC Layer, Sector, and Wire Number
C-   Outputs : HIT     Returns delta_PHI position from SW of hit nearest 
C-                     the given road road center
C-             HITLST(Hit#, HITADR, HITLEN, POINT) of hit cluster in road 
C-                     for possible full FADC unpacked timing
C-   Controls:
C-
C-   Created  27-AUG-1990   Srini Rajagopalan     created as DFSTRK
C-   Updated  26-AUG-1991   D Claes - Shorten/simplify and fix bugs
C-                                    Add quick hit-counting
C-            03-SEP-1991   D Claes - Count hits if DRIFT time within ROAD
C-            23-SEP-1991   D Claes - following conversation with QZLI
C-                                    Check timing pulse for trigger time
C-                                    Accepts TRGR as input 
C-            09-NOV-1992   D Claes - HIT returns delPHI to wire of closest hit
C-            05-NOV-1993   D Claes - Bug fix for WIDE roads when PHI =~ 2*PI
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:PI.DEF/LIST'
      INCLUDE 'D0$INC:CDCLNK.INC'
      INCLUDE 'D0$INC:CDCMAP.INC'
      INCLUDE 'D0$INC:L2TRAK.INC/LIST'
C
      INTEGER CHNL,CLOSEST,END,HITLEN,HITADR,HITLST(5,3)
      INTEGER I,ILYR,ISCTR,IWIRE,JWIRE,LABEL,LCHA,L2DTMW
      INTEGER MASK16,NPULSE,POINT,TPOINT,TPULSE
C
      REAL BEG_PULS,BT,DIFF,DIFF2,DRIFT,DRIFT_AVG
      REAL DRIFT_MAX,DRIFT_MIN,END_PULS,HIT
      REAL PHI_CELL,PHI_1,PHI_2,PHI_MAX,PHI_MIN, PHI_0,PULST(5)
      REAL ROAD(1:6,0:6,0:3)  ! MAX:MIN,WIRE,LAYER roads
      REAL TEMP, TIMLST(5), TRGR, TZERO, VELOCITY 
C
C     PARAMETER (MASK16 = 'FFFF'X)
      PARAMETER (MASK16 = 65535)
C
C       Discussions with Joey, Jim - drifts are straight (along y in cell
C       coordinate system) but drift velocities seem to be cell dependent
C       (35-40 um/ns) and Tzeroes are NOT well known, and vary wire to wire
C
      BT = 1000./106.
C
C  unpack channel length and channel number
C
C   Need trigger time for COSMIC data.  None in MC.  An overall stable
C   T0 for the detector will serve for real data taking.
C
      NPULSE = 0
      CALL DCODER(LABEL,ILYR,ISCTR,IWIRE,0,2)   ! Encodes and outputs 
C                                               ! CDC channel address
      POINT = MAP(LABEL)                ! Get pointer to channel data
      LCHA = IAND(IQ(POINT), MASK16)
      IF (LCHA.LE.4) GO TO 999
C
      CHNL = IAND(ISHFT(IQ(POINT), -16), MASK16)
      IF (CHNL.NE.LABEL) GO TO 999      ! Neccessary check since
      END = POINT - LCHA/4 + 1          ! MAP is never re-zeroed
C
C                                                     ! Center of Cell
      IF (MC_FLAG) THEN                 ! Design orientation of CDC
        PHI_CELL = (1 + MOD(ILYR,2) + 2*ISCTR) * WIDSEC  ! MC (design position)
        IF ((ISCTR.EQ.31).AND.(ILYR.EQ.1 .OR. ILYR.EQ.3)) PHI_CELL = 0
      ELSE                              ! DATA(Installed position)
        PHI_CELL = (MOD(ILYR,2) + 2*ISCTR) * WIDSEC
      ENDIF
C
      PHI_0 = (ROAD(1,IWIRE,ILYR) + ROAD(2,IWIRE,ILYR))/2 - PHI_CELL
      PHI_1 = ROAD(1,IWIRE,ILYR) - PHI_CELL     ! Change  to  CELL 
      PHI_2 = ROAD(2,IWIRE,ILYR) - PHI_CELL     ! coordinate system
C
      IF (PHI_0 .GT. PI) THEN
        PHI_1 = PHI_1 - TWOPI
        IF (ROAD(2,IWIRE,ILYR).GT.0) THEN       ! Otherwise this value already
          PHI_2 = PHI_2 - TWOPI                 ! had TWOPI subtracted in the
        ENDIF                                   ! L2_ROAD road calculation
      ENDIF
      IF (PHI_0 .LT. -PI) THEN
        IF (ROAD(1,IWIRE,ILYR).LT.0) THEN       ! Similar precaution here
          PHI_1 = PHI_1 + TWOPI
        ENDIF
        PHI_2 = PHI_2 + TWOPI
      ENDIF
C
      IF (ABS(PHI_1).GT.ABS(PHI_2)) THEN
        PHI_MAX = ABS(PHI_1)
        PHI_MIN = ABS(PHI_2)
      ELSE
        PHI_MAX = ABS(PHI_2)
        PHI_MIN = ABS(PHI_1)
      ENDIF
C
C       The drift velocity is stored in cm/ns
C
      DRIFT_MIN = RADIUS(IWIRE,ILYR) * TAN(PHI_MIN)  ! in centimeters
      DRIFT_MAX = RADIUS(IWIRE,ILYR) * TAN(PHI_MAX)  ! in centimeters
C
      L2DTMW = LC(L2DTMH - 1 - ILYR)      ! Pointer to wire info for this Layer
      JWIRE = L2DTMW + (ISCTR*IC(L2DTMW+4) + IWIRE)*IC(L2DTMW+3) + 4
C
C       Now    VELOCITY = C(JWIRE+2)   and   Tzero = C(JWIRE+1)
C       To change drift distance to drift time use the relation
C              DISTANCE = (time - Tzero)*DRIFT_VELOCITY
C
      TZERO    = C(JWIRE+1)
      VELOCITY = C(JWIRE+2)
C
C  ROAD CENTER (for determing hits closest to center of road)
C
      DRIFT_AVG = SIGN(1.,PHI_1)*DRIFT_MIN+SIGN(1.,PHI_2)*DRIFT_MAX
      DRIFT_AVG = ABS(DRIFT_AVG/2.)
C
      DO WHILE (POINT.GT.END .AND. NPULSE.LT.5)
        POINT = POINT - 1               ! Bottom of NEXT hit's data
        HITLEN = IAND(IQ(POINT), MASK16)
        HITADR = IAND(ISHFT(IQ(POINT), -16), MASK16)  ! Timing bin position
C                                                     ! of end of hit  data
C
        IF (HITLEN.EQ.0)   GOTO 999                   ! Bad HITLEN read out
        IF (HITADR.EQ.0)   GOTO 999                   ! Bad HITADR read out
        IF (HITADR.GT.500) GOTO 999                   ! Bad HITADR read out
C
C       Check if timing falls within ROAD
C       If NOT, skip to NEXT pulse!
C
C   The offsets (Qizhong's TRGOFF) have already been added 
C   back  in  to the  Trigger  times  reported  by  DTRAKS 
C   which  selects  roads  for  the  old  COSMIC RAY  data
C
        IF (TRGFLG) THEN                                  ! COSMIC timing pulse
          END_PULS = (HITADR * 10. - TRGR - TZERO) * VELOCITY
          BEG_PULS = ((HITADR-HITLEN+5)*BT-TRGR-TZERO)*VELOCITY
        ELSE IF (MC_FLAG) THEN                            ! MC uses no offset
          END_PULS = (HITADR * 10. - TZERO) * VELOCITY    
          BEG_PULS = ((HITADR - HITLEN + 5) * BT - TZERO) * VELOCITY
        ELSE                                              ! Real DATA
          END_PULS = (HITADR * 10. - TZERO - TRGOFF) * VELOCITY    
          BEG_PULS = ((HITADR-HITLEN+5)*BT-TZERO-TRGOFF)*VELOCITY
        ENDIF
C
        IF (BEG_PULS.LT.-0.50) GOTO 999               ! Bad HITLEN extracted
C                                                     ! and  can't calculate
C                                                     ! next POINT  position
C
        IF ((PHI_1*PHI_2).GT.0) THEN            ! Road does NOT straddle wire
          IF (END_PULS .LT. DRIFT_MIN) GOTO 300 ! Pulse comes too early
        ENDIF
        IF (BEG_PULS .GT. DRIFT_MAX) GOTO 300   ! Pulse comes too late
C                                                            
        TPULSE = 0
        TPOINT = POINT                                       ! Returns timing
        CALL L2_HITFIND(1,HITADR,HITLEN,POINT,TPULSE,TIMLST) ! of hits on the
C                                                            ! Sense Wire
C
        DRIFT = BEG_PULS                                     ! Default
C
C Test effects of Leading EDGE computation on efficiency
C
        IF (TPULSE.EQ.0) GOTO 250
C
        DO I = 1, TPULSE
C
          IF (TRGFLG) THEN                               ! COSMIC timing pulse
            DRIFT = (TIMLST(I) - TRGR - TZERO) * VELOCITY
          ELSE IF (MC_FLAG) THEN                         ! MC uses no offset
            DRIFT = (TIMLST(I) - TZERO) * VELOCITY    
          ELSE                                           ! Real DATA
            DRIFT = (TIMLST(I) - TZERO - TRGOFF) * VELOCITY    
          ENDIF
C
          IF ((PHI_1*PHI_2).GT.0) THEN           ! Road does NOT straddle wire
            IF (DRIFT .LT. DRIFT_MIN) GOTO 200   ! Pulse comes too early
          ENDIF
          IF (DRIFT .GT. DRIFT_MAX) GOTO 200     ! Pulse comes too late
          GOTO 250                               ! Found leading edge in road
  200     CONTINUE
        ENDDO
        GOTO 400
C
  250   CONTINUE
        NPULSE = NPULSE + 1
        IF (NPULSE. GT. 5) THEN
          NPULSE = 5
          GOTO 999
        ENDIF
C       PULST(NPULSE) = BEG_PULS
        PULST(NPULSE) = DRIFT/RADIUS(IWIRE,ILYR)
        HITLST(NPULSE,1) = HITADR
        HITLST(NPULSE,2) = HITLEN
        HITLST(NPULSE,3) = TPOINT
C
        GOTO 400                  ! Since L2_HITFIND already increments POINT
C
  300   CONTINUE
        POINT = POINT - (HITLEN/4) + 1
  400   CONTINUE
      ENDDO
C
  999 CONTINUE
      HIT = PULST(1)
      IF (NPULSE .GT. 1) THEN               ! Move hit closest to road
        CLOSEST = 1                         ! center first in the list
C       DIFF = ABS(PULST(1)-DRIFT_AVG)
        DIFF = ABS(PULST(1)-DRIFT_AVG/RADIUS(IWIRE,ILYR))
        DO I = 2, NPULSE
C         DIFF2 = ABS(PULST(I)-DRIFT_AVG)
          DIFF2 = ABS(PULST(I)-DRIFT_AVG/RADIUS(IWIRE,ILYR))
          IF (DIFF2.LT.DIFF) THEN
            DIFF = DIFF2
            CLOSEST = I
          ENDIF
        ENDDO
        HIT = PULST(CLOSEST)
C        IF (CLOSEST.NE.1) THEN
C          TEMP = HITLST(1,1)
C          HITLST(1,1) = HITLST(CLOSEST,1)
C          HITLST(CLOSEST,1) = TEMP
C          TEMP = HITLST(1,2)
C          HITLST(1,2) = HITLST(CLOSEST,2)
C          HITLST(CLOSEST,2) = TEMP
C        ENDIF
      ENDIF
      RETURN
      END
