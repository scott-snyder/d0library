      SUBROUTINE SVTX_TO_VCAL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create compressed calibration bank, VCAL from
C-               quantities in VGNL,VTMW...
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  14-FEB-1994   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:VCAL.PARAMS'
c Locals:
      LOGICAL FIRST
      INTEGER LVCAL,LAY,LVGNL,N_OUTER(0:2),N_INNER(0:2),PTV,OFF,PTG
      INTEGER WORD,LVTMW,PTVS,SEC,NSEC(0:2),WIR,PTW,LVDTM
      REAL    BPCM,T0_LC,T0_OFF,RELG_LC,RELG_OFF,ABSG_LC,ABSG_OFF
      REAL    G0,G1,T0_0,T0_1,RELG,LUM,PABS,TDEGC
      REAL    AREAC_LC,WSCAL_LC,WSCAL_OFF,LUM_LC
      REAL    PABS_LC,TDEGC_LC,AVE_ABSG(0:2)
      INTEGER BIT,MAXT,MAXA,MAXR,MAXL,MAXP,MAXTD
      INTEGER LUMTIME,ENVTIME,LUM_STAT,ENV_STAT
c Externals:
      INTEGER GZVCAL,GZVGNL,GZVTMW,GZVDTM
c Data:
      
      DATA NSEC/15,31,31/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        MAXR = 2**RELG_BITS - 1
        MAXA = 2**ABSG_BITS - 1
        MAXT = 2**T0_BITS - 1
        MAXL = 2**LUM_BITS - 1
        MAXP = 2**PABS_BITS - 1
        MAXTD= 2**TDEGC_BITS - 1
        AREAC_LC = AREAC_FS/256
        RELG_LC = 2*RELG_WID/(MAXR+1)
        RELG_OFF= 1. - RELG_WID
        WSCAL_LC = 2.*WSCAL_WID/256
        WSCAL_OFF= 1. - WSCAL_WID
        LUM_LC = LUM_SPAN/(MAXL+1)
        PABS_LC = PABS_SPAN/(MAXP+1)
        TDEGC_LC = TDEGC_SPAN/(MAXTD+1)
      ENDIF
      LVCAL = GZVCAL(0)
      IF (LVCAL .GT. 0) CALL MZDROP(IXSTP,LVCAL,' ')
      CALL BKVCAL(LVCAL)
      CALL D3UPT(IQ(LHEAD+4),IC(LVCAL+5))
C
C ****  Save VGNL banks:  Make sure existing VGNL bank has 20 bins/cm
C
      DO LAY = 0,2
        LVGNL = GZVGNL(LAY)
        BPCM = C(LVGNL+6)
        IF ( ABS(BPCM-20.) .GT. .01)
     &    CALL ERRMSG('VGNL bank structure unexpected','SVTX_TO_VCAL',
     &    ' ','F')
        N_OUTER(LAY) = IC(LVGNL+15)
        N_INNER(LAY) = IC(LVGNL+14)
        C(LVCAL+9+LAY) = C(LVGNL+7)    ! 1+2*r_inp/R_wire
C
C ****  Area corrections for PWC region
C
        PTV = LVCAL + 20 + 2*LAY
        IC(PTV) = 0
        OFF = 0
        DO PTG = 16,23
          IF (OFF .EQ. 32) THEN
            PTV = PTV + 1
            IC(PTV) = 0
            OFF = 0
          ENDIF
          WORD = MAX0(0,MIN0(255,NINT(C(LVGNL+PTG)/AREAC_LC)))
          CALL MVBITS(WORD, 0, 8, IC(PTV), OFF)
          OFF = OFF + 8
        ENDDO
      ENDDO
      PTV = LVCAL + 25
      DO LAY = 0,2
        PTV = PTV + 1
        IC(PTV) = 0
        OFF = 0
        LVGNL = GZVGNL(LAY)
C
C ****  Save area correction for OUTER category (e.g. wires 0 & 7)
C
        DO PTG = 24,24+N_OUTER(LAY)
          IF (OFF .EQ. 32) THEN
            PTV = PTV + 1
            IC(PTV) = 0
            OFF = 0
          ENDIF
          WORD = MAX0(0,MIN0(255,NINT(C(LVGNL+PTG)/AREAC_LC)))
          CALL MVBITS(WORD, 0, 8, IC(PTV), OFF)
          OFF = OFF + 8
        ENDDO
        PTV = PTV + 1
        OFF = 0
        IC(PTV) = 0
C
C ****  Save area correction for INNER category (e.g. wires 1-6)
C
        DO PTG = 24+N_OUTER(LAY)+1,24+N_OUTER(LAY)+1+N_INNER(LAY)
          IF (OFF .EQ. 32) THEN
            PTV = PTV + 1
            IC(PTV) = 0
            OFF = 0
          ENDIF
          WORD = MAX0(0,MIN0(255,NINT(C(LVGNL+PTG)/AREAC_LC)))
          CALL MVBITS(WORD, 0, 8, IC(PTV), OFF)
          OFF = OFF + 8
        ENDDO
      ENDDO
C
C ****  Save T0's and gains.  First, need to compute packing constants
C
      CALL VCAL_T0PACK(T0_LC,T0_OFF)
      CALL VCAL_GNPACK(AVE_ABSG)
      PTVS = LVCAL + 65
      PTV  = LVCAL + 71
      OFF  = 0
      DO LAY = 0,2
        ABSG_LC = 2.*ABSG_WID*AVE_ABSG(LAY)/(MAXA+1)
        ABSG_OFF= AVE_ABSG(LAY)*(1. - ABSG_WID)
        LVTMW = GZVTMW(LAY)
        LVGNL = GZVGNL(LAY)
        DO SEC = 0,NSEC(LAY)
          DO WIR = 0,7
            PTW = LVTMW + (8*SEC + WIR)*IC(LVTMW+3) + 5
            PTG = LVGNL + 16*SEC + 2*WIR + 6 + 3*41
            IF (SEC .EQ. 0) THEN
C
C ****  These are the static SCALE factors -- one/wire-layer
C
              IF (OFF .EQ. 32) THEN
                OFF = 0
                PTVS = PTVS + 1
              ENDIF
              WORD = MAX0(0,
     &               MIN0(255,NINT((C(PTW+5)-WSCAL_OFF)/WSCAL_LC)))
              CALL MVBITS(WORD,0,8,IC(PTVS),OFF)
              OFF = OFF + 8
            ENDIF
C
C ****  Here are the gains and T0;s
C
            T0_0 = C(PTW+1)
            T0_1 = C(PTW+3)
            G0   = C(PTG)
            G1   = C(PTG+1)
            RELG = G1/G0
            BIT = 0
            IC(PTV) = 0
            WORD = MAX0(0,MIN0(MAXT,NINT((T0_0-T0_OFF)/T0_LC)))
            CALL MVBITS(WORD,0,T0_BITS,IC(PTV),BIT)
            BIT = BIT + T0_BITS
            WORD = MAX0(0,MIN0(MAXT,NINT((T0_1-T0_OFF)/T0_LC)))
            CALL MVBITS(WORD,0,T0_BITS,IC(PTV),BIT)
            BIT = BIT + T0_BITS
            WORD = MAX0(0,MIN0(MAXA,NINT((G0-ABSG_OFF)/ABSG_LC)))
            CALL MVBITS(WORD,0,ABSG_BITS,IC(PTV),BIT)
            BIT = BIT + ABSG_BITS
            WORD = MAX0(0,MIN0(MAXR,NINT((RELG-RELG_OFF)/RELG_LC)))
            CALL MVBITS(WORD,0,RELG_BITS,IC(PTV),BIT)
            PTV = PTV + 1
          ENDDO
        ENDDO
      ENDDO
C
C ****  Now, fill the other stuff...
C
      CALL VDYNGET(LUM,PABS,TDEGC,LUMTIME,ENVTIME)
      BIT = 0
      WORD = MAX0(0,MIN0(MAXL,NINT((LUM-LUM_OFF)/LUM_LC)))
      CALL MVBITS(WORD,0,LUM_BITS,IC(LVCAL+19),BIT)
      BIT = BIT + LUM_BITS
      WORD = MAX0(0,MIN0(MAXP,NINT((PABS-PABS_OFF)/PABS_LC)))
      CALL MVBITS(WORD,0,PABS_BITS,IC(LVCAL+19),BIT)
      BIT = BIT + PABS_BITS
      WORD = MAX0(0,MIN0(MAXTD,NINT((TDEGC-TDEGC_OFF)/TDEGC_LC)))
      CALL MVBITS(WORD,0,TDEGC_BITS,IC(LVCAL+19),BIT)
      LVDTM = GZVDTM(0,0)
      IC(LVCAL+4) = IC(LVDTM-5)
      IC(LVCAL+6) = LUMTIME
      IC(LVCAL+7) = ENVTIME
      C(LVCAL+8) = C(LVDTM+6)
      CALL UCOPY(AVE_ABSG(0),C(LVCAL+12),3)
      C(LVCAL+15) = T0_OFF
      C(LVCAL+16) = T0_LC
      CALL VTX_DYNSTAT(ENV_STAT,LUM_STAT)
      CALL MVBITS(ENV_STAT+1,0,2,IC(LVCAL),0)
      CALL MVBITS(LUM_STAT+1,0,2,IC(LVCAL),2)
  999 RETURN
      END
