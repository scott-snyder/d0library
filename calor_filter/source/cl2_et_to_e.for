      FUNCTION CL2_ET_TO_E(IETA_IN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : correction factor from
C-    L2 CAEP (nominal ET) to true ET, based on level 2 vertex
C-      a value of 0.0 is returned for the correction if for an illegal cell
C-
C-   Inputs  : IETA_IN  Calorimeter readout tower number
C-   Outputs : correction factor so ETtrue = ETnominal*CL2_ET_CORR(IETA_IN)
C-   Controls: none
C-
C----------------------------------------------------------------------
C-   Created  11-DEC-1992   William G. Cobau -- Based on CL2_ET_CORR
C-                                                and CL2_SNTH
C-   Updated  27-OCT-1993   William Cobau    -- really set correction to 
C-                                                zero for bad input 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INTEGER IETA_IN,IPHI,LYR,IER,IPHI_IN,LYR_IN
      REAL    CL2_ET_TO_E,XCELL,YCELL,ZCELL
C----------------------------------------------------------------------
      IPHI = 1  !always exists
      LYR = 11  !almost always exixts
      CL2_ET_TO_E = 0.0
      IF (IETA_IN.EQ.0) RETURN
      IF (ABS(IETA_IN).GE.NETAL) THEN
        IF (ABS(IETA_IN).EQ.NETAL) THEN
          LYR = 13  ! last cell has no layer 11
        ELSE
          RETURN
        ENDIF
      ENDIF
      CALL CELXYZ(IETA_IN,IPHI,LYR,XCELL,YCELL,ZCELL,IER)
C
C- Here is conversion from Et to E 
C-    This correction is correct for every event 
C
      CL2_ET_TO_E = SQRT(1.0 + (ZCELL)**2/(XCELL**2+YCELL**2))
C----------------------------------------------------------------------
  999 RETURN
      END
