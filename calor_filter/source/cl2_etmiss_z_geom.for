      SUBROUTINE CL2_ETMISS_Z_GEOM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  set sin theta(z) part of /CL2_ETMISS_geom/
C-                          for this event
C-
C-   Inputs  : L0VT bank (creates if needed)
C-   Outputs : /CL2_ETMISS_GEOM/
C-   Controls: none
C-
C-   Created  23-OCT-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:CL2_ETMISS_GEOM.INC'
      INTEGER IETA
      REAL    CL2_ET_CORR
C----------------------------------------------------------------------
C
C
C...calculate sin theta correction for towers at a given eta for this event
      DO IETA = -NETAL,NETAL
        ET_CORR(IETA)= CL2_ET_CORR(IETA)  !use Z to correct wrt Zv=0
      ENDDO
      RETURN
      END
