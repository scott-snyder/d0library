C----------------------------------------------------------------------
C-   COMMON used to store corrections and calibrations
C-   written in TRCFAD.FOR, read in TPRLFIL.FOR
C-   Created  15-JAN-1993   Alain PLUQUET
C-   PAD(LAYER,WIRE,K) I=1,2,3     layer
C-              J=1,2,3,4   hit wire
C-              PAD(LAYER,WIRE,1)  electronic gain
C-              PAD(LAYER,WIRE,2)  epicor
C-              PAD(LAYER,WIRE,3)  apc (additive pedestal correction)
C-              PAD(LAYER,WIRE,4)  pedestal
C-              PAD(LAYER,WIRE,5)  sector correction
C-              PAD(LAYER,WIRE,6)  wire correction
C-              PAD(LAYER,WIRE,7)  high voltage correction
C-              PAD(LAYER,WIRE,8)  angular correction (sin theta)
C-              PAD(LAYER,WIRE,9)  gas correction  
C-              PAD(LAYER,WIRE,10) T canary
C-              PAD(LAYER,WIRE,11) T TRD
C-              PAD(LAYER,WIRE,12) P canary
C-              PAD(LAYER,WIRE,13) P TRD
C-              PAD(LAYER,WIRE,14) gain canary
C-              PAD(LAYER,WIRE,15) HV anode
C-              PAD(LAYER,WIRE,16) HV window
C-              PAD(LAYER,WIRE,17) HV potential
C-   STATUS(LAYER,WIRE,K) I=1,2,3  LAYER
C-               J=1,2,3,4 (hit wire)
C-              STATUS(LAYER,WIRE,1)  electronic gain
C-              STATUS(LAYER,WIRE,2)  epicor
C-              STATUS(LAYER,WIRE,3)  apc (additive pedestal correction)
C-              STATUS(LAYER,WIRE,4)  pedestal
C-              STATUS(LAYER,WIRE,5)  sector correction
C-              STATUS(LAYER,WIRE,6)  wire correction
C-              STATUS(LAYER,WIRE,7)  high voltage correction
C-              STATUS(LAYER,WIRE,8)  angular correction (sin theta)
C-              STATUS(LAYER,WIRE,9)  gas correction  
C-  corrections :
C-  CANG,CEPI,CAPC,CGAS,CSEC,CWIR,CWIR,CHVT,CELE,CPED
C-  status of each correction (0=OK) :
C-  EANG,EEPI,EAPC,EGAS,ESEC,EWIR,EWIR,EHVT,EELE,EPED
C-  high voltages,temperatures,pressures,canary :
C-  HVA,HVP,HVW,TCAN,TTRD,PCAN,PTRD,GCAN
C----------------------------------------------------------------------
      COMMON /TRDCOR/PAD(3,4,20),STATUS(3,4,10),
     &  CANG,CEPI,CAPC,CGAS,CSEC,CWIR,CHVT,CELE,CPED,
     &  EANG,EEPI,EAPC,EGAS,ESEC,EWIR,EHVT,EELE,EPED,
     &  HVA,HVP,HVW,TCAN,TTRD,PCAN,PTRD,GCAN
      REAL PAD
      INTEGER STATUS
      REAL    CANG,CEPI,CAPC,CGAS,CSEC,CWIR,CHVT,CELE,CPED
      INTEGER EANG,EEPI,EAPC,EGAS,ESEC,EWIR,EHVT,EELE,EPED
      REAL    HVA,HVP,HVW,TCAN,TTRD,PCAN,PTRD,GCAN
