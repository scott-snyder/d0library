      SUBROUTINE VCAL_TO_SVTX(ENV_STAT,LUM_STAT,GFAC,DFAC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill VSTP banks (VGNL and VTMW) from VCAL bank
C-
C-   Inputs  : VCAL bank
C-   Outputs : ENV_STAT -- Status from VTX_ENVADJ
C-             LUM_STAT -- Status from VTX_LUMADJ or VTX_HVADJ
C-             GFAC     -- Gain adjustment factor due to environment
C-             DFAC     -- Drift velocity adjustment factor due to envir.
C-   Controls: 
C-
C-   Created  14-FEB-1994   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:VCAL.PARAMS'
c I/O:
      LOGICAL  ENV_STAT,LUM_STAT
      REAL     GFAC,DFAC
C Locals:
      LOGICAL FIRST
      INTEGER LVCAL,LVGNL,LAY,SEC,NSEC(0:2),WIR,PTV,OFF,PTG,ERR
      INTEGER N_OUTER(0:2),N_INNER(0:2),WORD,LVTMW,LVDTM,PTW,LUM_INDEX
      INTEGER BIT
      REAL    BPCM,WSCALE(0:7,0:2),RELG_OFF,RELG_LC,ABSG_LC,ABSG_OFF
      REAL    AREAC_LC,WSCAL_LC,WSCAL_OFF
      REAL    T0_OFF,T0_LC,RELG,ABSG,T0_0,T0_1
      REAL    LUM_LC,PABS_LC,TDEGC_LC
      REAL    LUM,PABS,TDEGC
      REAL    PAR(6),POFF,PEXP,TOFF,TEXP,KG,G0
      EQUIVALENCE (POFF,PAR(1)),(PEXP,PAR(2)),(TOFF,PAR(3)),
     &            (TEXP,PAR(4)),(KG,PAR(5)),(G0,PAR(6))
C Externals:
      INTEGER GZVCAL,GZVGNL,GZVTMW,GZVDTM
C Data:
      DATA    NSEC/15,31,31/
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('ENV_PAR',PAR,ERR)
        CALL EZRSET
        AREAC_LC = AREAC_FS/256.
        WSCAL_LC = 2.*WSCAL_WID/256.
        WSCAL_OFF= 1. - WSCAL_WID
        LUM_LC = LUM_SPAN/FLOAT(2**LUM_BITS)
        PABS_LC = PABS_SPAN/FLOAT(2**PABS_BITS)
        TDEGC_LC = TDEGC_SPAN/FLOAT(2**TDEGC_BITS)
      ENDIF
      LVCAL = GZVCAL(1)
C
C ****  Fill VGNL banks:  Make sure existing VGNL bank has 20 bins/cm.
C
      DO LAY = 0,2
        LVGNL = GZVGNL(LAY)
        BPCM = C(LVGNL+6)
        IF (ABS(BPCM-20.) .GT. .01) 
     &    CALL ERRMSG('VGNL bank structure unexpected','VCAL_TO_SVTX',
     &    ' ','F')
        N_OUTER(LAY) = IC(LVGNL+15)
        N_INNER(LAY) = IC(LVGNL+14)
        C(LVGNL+7) = Q(LVCAL+9+LAY)   ! 1+2*r_inp/R_wire
C
C ****  Area correction for PWC region
C
        PTV = LVCAL + 20 + 2*LAY
        OFF = 0
        DO PTG = 16,23
          IF (OFF .EQ. 32) THEN
            OFF = 0
            PTV = PTV + 1
          ENDIF
          WORD = IBITS( IQ(PTV),OFF, 8)
          C(LVGNL+PTG) = AREAC_LC*WORD
          OFF = OFF + 8
        ENDDO
      ENDDO
      PTV = LVCAL + 25
      DO LAY = 0,2
        PTV = PTV + 1
        OFF = 0
        LVGNL = GZVGNL(LAY)
C
C ****  Fill area correction for OUTER category (e.g. wires 0 & 7)
C
        DO PTG = 24,24+N_OUTER(LAY)
          IF (OFF .EQ. 32) THEN
            OFF = 0
            PTV = PTV + 1
          ENDIF
          WORD = IBITS( IQ(PTV),OFF,  8)
          C(LVGNL+PTG) = AREAC_LC*WORD
          OFF = OFF + 8
        ENDDO
        PTV = PTV + 1
        OFF = 0
C
C ****  Fill area correction for INNER category (e.g. Wires 1-6)
C
        DO PTG = 24+N_OUTER(LAY)+1,24+N_OUTER(LAY)+1+N_INNER(LAY)
          IF (OFF .EQ. 32) THEN
            OFF = 0
            PTV = PTV + 1
          ENDIF
          WORD = IBITS( IQ(PTV),OFF, 8)
          C(LVGNL+PTG) = AREAC_LC*WORD
          OFF = OFF + 8
        ENDDO
      ENDDO
C
C ****  Get the static SCALE parameters for VTMW banks
C
      PTV = LVCAL + 65
      OFF = 0
      DO LAY = 0,2
        DO WIR = 0,7
          IF (OFF .EQ. 32) THEN
            OFF = 0
            PTV = PTV + 1
          ENDIF
          WORD = IBITS( IQ(PTV),OFF, 8)
          WSCALE(WIR,LAY) = WSCAL_LC*WORD + WSCAL_OFF
          OFF = OFF + 8
        ENDDO
      ENDDO
C
C ****  Fill GAINS and T0's and static SCALE values
C
      RELG_OFF = 1. - RELG_WID
      RELG_LC  = 2.*RELG_WID/(2**RELG_BITS)
      T0_OFF   = Q(LVCAL+15)
      T0_LC    = Q(LVCAL+16)
      PTV = LVCAL + 71
      DO LAY = 0,2
        ABSG_LC  = 2.*Q(LVCAL+12+LAY)*ABSG_WID/(2**ABSG_BITS)
        ABSG_OFF = Q(LVCAL+12+LAY)*(1. - ABSG_WID)
        LVGNL = GZVGNL(LAY)
        LVTMW = GZVTMW(LAY)
        DO SEC = 0,NSEC(LAY)
          DO WIR = 0,7
            BIT = 0
            WORD = IBITS(IQ(PTV),BIT,T0_BITS)
            T0_0 = T0_LC*WORD + T0_OFF
            BIT = BIT + T0_BITS
            WORD = IBITS(IQ(PTV),BIT,T0_BITS)
            T0_1 = T0_LC*WORD + T0_OFF
            BIT = BIT + T0_BITS
            WORD = IBITS(IQ(PTV),BIT,ABSG_BITS)
            ABSG = ABSG_LC*WORD + ABSG_OFF
            BIT = BIT + ABSG_BITS
            WORD = IBITS(IQ(PTV),BIT,RELG_BITS)
            RELG = RELG_LC*WORD + RELG_OFF
            PTG =  LVGNL + 16*SEC + 2*WIR + 3*41 + 6
            PTW  = LVTMW + (8*SEC + WIR)*IC(LVTMW+3) + 5
            C(PTG  ) = ABSG
            C(PTG+1) = ABSG*RELG
            C(PTW+1) = T0_0
            C(PTW+2) = .6
            C(PTW+3) = T0_1
            C(PTW+4) = .6
            C(PTW+5) = WSCALE(WIR,LAY)
            PTV = PTV + 1
          ENDDO
        ENDDO
      ENDDO
C
C ****  Fill dynamic SCALE value im VDTM
C
      DO LAY = 0,2
        DO SEC = 0,NSEC(LAY)
          LVDTM = GZVDTM(LAY,SEC)
          LUM_INDEX = IC(LVDTM-5)
          DO WHILE (LVDTM .GT. 0)
            C(LVDTM+6) = Q(LVCAL+8)
            LVDTM = LC(LVDTM)
          ENDDO
        ENDDO
      ENDDO
C
C ****  FILL I/O LIST
C 
      BIT = 0
      WORD  = IBITS(IQ(LVCAL+19),BIT,LUM_BITS)
      LUM  = FLOAT(WORD)*LUM_LC + LUM_OFF
      BIT = BIT + LUM_BITS
      WORD  = IBITS(IQ(LVCAL+19),BIT,PABS_BITS)
      PABS = FLOAT(WORD)*PABS_LC + PABS_OFF
      BIT = BIT + PABS_BITS
      WORD  = IBITS(IQ(LVCAL+19),BIT,TDEGC_BITS)
      TDEGC = FLOAT(WORD)*TDEGC_LC + TDEGC_OFF
      GFAC  = G0*EXP( KG*(TDEGC+TOFF)/(PABS+POFF) )
      DFAC  = (TDEGC+TOFF)**TEXP/(PABS+POFF)**PEXP
      CALL VDYNSAVE('LUM',LUM,IQ(LVCAL+6))
      CALL VDYNSAVE('PABS',PABS,IQ(LVCAL+7))
      CALL VDYNSAVE('TDEGC',TDEGC,IQ(LVCAL+7))
      ENV_STAT = IBITS(IQ(LVCAL),0,2) - 1
      LUM_STAT = IBITS(IQ(LVCAL),2,2) - 1
C
C ****  Finally, see if luminosity index has changed (this is for benefit
C ****  of data taken prior to 15-Dec-1992)
C
      LVCAL = GZVCAL(1)
      IF (LUM_INDEX .NE. IQ(LVCAL+4)) THEN
        LUM = FLOAT(IQ(LVCAL+4))/100.
        CALL VTX_LUMDTM(LUM)
      ENDIF
  999 RETURN
      END
