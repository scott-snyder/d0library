      FUNCTION TOP_LEPTONS_UTIL_CALC_DR(DETA,DPHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns dR given deta and dphi
C-
C-   Inputs  : 
C-              DETA - eta difference between two vectors
C-              DPHI - phi difference between two vectors
C-
C-   Outputs : 
C-              DR   - R difference between input vectors
C-
C-   Controls:  None
C-
C-   Created   8-SEP-1992   Stephen J. Wimpenny
C-   Modified 22-Mar-1993   Name changed for library compatibility
C-                          (was Dr_From_Deta_Dphi)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL DETA,DPHI,DR,TEMP,TOP_LEPTONS_UTIL_CALC_DR
C
      DR=0.0
      TEMP=DPHI**2+DETA**2
      IF(TEMP.GT.1.0E-6) DR=SQRT(TEMP)
      TOP_LEPTONS_UTIL_CALC_DR=DR
C----------------------------------------------------------------------
  999 RETURN
      END
