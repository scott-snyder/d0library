      SUBROUTINE GMEDT(QUAD,NUMED)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : WHAT PART OF THE TOROID YOU ARE IN
C-
C-   Inputs  : QUAD - quadrant 1-4 CF 5-12 EF
C-   Outputs : NUMED : 1 - CF REGION, 2 - EF & SAMUS 
C-   Controls: 
C-
C-   Created  13-MAR-1991  AKl
C-   Updated  20-APR-1991   AKl 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER QUAD,NUMED
C
C----------------------------------------------------------------------
       IF(QUAD .LE. 4)NUMED=1        ! CF 
       IF(QUAD .GT. 4)NUMED=2        ! EF 
  999 RETURN
      END
