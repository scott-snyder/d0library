      SUBROUTINE TOP_LEPTONS_LONG_ELECTRON_DUMP(LUN,LPELC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Long Electron Dump
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  15-APR-1993   Stephen J. Wimpenny
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      EXTERNAL TOP_LEPTONS_UTIL_CALC_DR
C
      INTEGER LUN,IER
      INTEGER LPELC,LHMTC,LZTRK,LDTRK,LFDCT,LVTXT,LTRDT
C
      REAL PI,TWOPI
      REAL EMFRAC,EISOL,EMCORE
      REAL DETA_VEC12,DPHI_VEC12,DR_VEC12
      REAL ZTRAK_THETA,ZTRAK_PHI,ZTRAK_ETA
      REAL TRD_TOTE,TRD_TRUNC,TRD_CANARY,TRD_TOTE_NORM,TRD_TRUNC_NORM
      REAL TOP_LEPTONS_UTIL_CALC_DR
C
      DATA PI,TWOPI/ 3.141593,6.283185/
C
C *** Calorimeter Profile + Isolation Information
C
      WRITE(LUN,1000) Q(LPELC+18),Q(LPELC+16),Q(LPELC+17),Q(LPELC+15)
      CALL TOP_LEPTONS_UTIL_CALOR_RATIOS(LPELC,EMFRAC,
     1  EMCORE,EISOL,IER)
      IF(IER.GE.0) THEN
        WRITE(LUN,1010) EMFRAC,EISOL
      ELSE
        WRITE(LUN,1011) EMFRAC,EISOL
      ENDIF
C
C *** HMatrix Chisquares
C
      LHMTC=LQ(LPELC-1)
      IF(LHMTC.GT.0) THEN
        WRITE(LUN,1030) Q(LHMTC+5),Q(LHMTC+7)
      ENDIF
C
C *** Central tracking and dE/dx
C
      WRITE(LUN,1020) Q(LPELC+21),Q(LPELC+22)
      LDTRK=0
      LFDCT=0
      LVTXT=0
      LTRDT=0
      LZTRK=LQ(LPELC-3)
      IF(LZTRK.GT.0) THEN
        LDTRK=LQ(LZTRK-7)
        LFDCT=LQ(LZTRK-8)
        LVTXT=LQ(LZTRK-6)
        LTRDT=LQ(LZTRK-9)
        CALL TOP_LEPTONS_UTIL_CALTRAK_ANGLES(LZTRK,ZTRAK_THETA,
     1    ZTRAK_PHI,ZTRAK_ETA)
        DETA_VEC12=Q(LPELC+9)-ZTRAK_ETA
        DPHI_VEC12=Q(LPELC+10)-ZTRAK_PHI
        IF(DPHI_VEC12.GT.PI) DPHI_VEC12=TWOPI-DPHI_VEC12
        DR_VEC12=TOP_LEPTONS_UTIL_CALC_DR(DETA_VEC12,DPHI_VEC12)
        WRITE(LUN,1021) ZTRAK_THETA,ZTRAK_PHI,ZTRAK_ETA,DR_VEC12
        IF(LDTRK.GT.0) WRITE(LUN,1022) Q(LDTRK+20)
        IF(LFDCT.GT.0) WRITE(LUN,1023) Q(LFDCT+20)
        IF(LVTXT.GT.0) WRITE(LUN,1024) Q(LVTXT+20)
C
C *** TRD information
C
        IF(LTRDT.GT.0) THEN
          TRD_TOTE=Q(LTRDT+4)
          TRD_TRUNC=Q(LTRDT+5)
          TRD_CANARY=Q(LTRDT+11)
          TRD_TOTE_NORM=TRD_TOTE
          IF(TRD_CANARY.GT.1.0E-3) THEN
            TRD_TOTE_NORM=TRD_TOTE/TRD_CANARY
          ENDIF
          TRD_TRUNC_NORM=TRD_TRUNC
          IF(TRD_CANARY.GT.1.0E-3) THEN
            TRD_TRUNC_NORM=TRD_TRUNC/TRD_CANARY
          ELSE
            TRD_CANARY=0.0
          ENDIF
          IF(TRD_CANARY.GT.1000.) TRD_CANARY=-999.
          WRITE(LUN,1040) TRD_TOTE,TRD_TRUNC,TRD_CANARY,TRD_TOTE_NORM,
     1      TRD_TRUNC_NORM
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
 1000 FORMAT(5X,' energy in dR=0.4 cone : em,total = ',2F8.1,
     1 /,5X,' energy in core cone (dR=0.2) : em,total = ',2F8.1)
 1010 FORMAT(5X,' (CACL) em fraction = ',F8.2,' isolation = ',
     1 F8.2)
 1011 FORMAT(5X,' (PELC) em fraction = ',F8.2,' isolation = ',
     1 F8.2)
 1020 FORMAT(5X,' no. matching CD tracks = ',F6.1,
     4 ' distance to closest track (cm) = ',F6.1)
 1021 FORMAT(5X,' theta,phi,eta,dR of closest track = ',4F8.2)
 1022 FORMAT(7X,' dE/dx in CDC = ',F8.1)
 1023 FORMAT(7X,' dE/dx in FDC = ',F8.1)
 1024 FORMAT(7X,' dE/dx in VTX = ',F8.1)
 1030 FORMAT(5X,' HMatrix Chiquares (Full,Trunmcated) = ',
     1 2F8.1)
 1040 FORMAT(5X,' TRD Anode Energies (Full,Truncated,Canary) = ',2F8.2,
     1 F10.2,/,
     2 5X,'  =>   Normalised Energies (Full,Truncated) = ',2F8.2)
      END
