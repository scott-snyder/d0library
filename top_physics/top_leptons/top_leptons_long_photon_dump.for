      SUBROUTINE TOP_LEPTONS_LONG_PHOTON_DUMP(LUN,LPPHO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Long Photon Dump
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
      INTEGER LUN,IER
      INTEGER LPPHO,LHMTC
C
      REAL EMFRAC,EISOL,EMCORE
C
C *** Calorimeter Profile + Isolation Information
C
      WRITE(LUN,1000) Q(LPPHO+18),Q(LPPHO+16),Q(LPPHO+17),Q(LPPHO+15)
      CALL TOP_LEPTONS_UTIL_CALOR_RATIOS(LPPHO,EMFRAC,EMCORE,EISOL,
     1  IER)
      IF(IER.GE.0) THEN
        WRITE(LUN,1010) EMFRAC,EISOL
      ELSE
        WRITE(LUN,1011) EMFRAC,EISOL
      ENDIF
C
C *** HMatrix Chisquares
C
      LHMTC=LQ(LPPHO-1)
      IF(LHMTC.GT.0) THEN
        WRITE(LUN,1020) Q(LHMTC+5),Q(LHMTC+7)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
 1000 FORMAT(5X,' energy in dR=0.4 cone : em,total = ',2F8.1,
     1 /,5X,' energy in core cone (dR=0.2) : em,total = ',2F8.1)
 1010 FORMAT(5X,' (CACL) em fraction = ',F8.2,' isolation = ',
     1 F8.2)
 1011 FORMAT(5X,' (PPHO) em fraction = ',F8.2,' isolation = ',
     1 F8.2)
 1020 FORMAT(5X,' HMatrix Chiquares (Full,Trunmcated) = ',
     1 2F8.1)
      END
