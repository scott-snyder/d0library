      REAL*8 FUNCTION FCONLV(X,FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate deviation from desired confidence
C-     level for a given cross section. Called by DZERO (CERNLIB)
C-
C-   Returned value  : Deviation from desired confidence level
C-   Inputs  : X - Cross Section
C-   Outputs : 
C-   Controls: FLAG - Control flag (not used)
C-
C-   Created   5-JUL-1993   Richard Partridge
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      EXTERNAL FSIGMA
C
      INCLUDE 'D0$INC:TOP_LIMDATA.INC'
C
      REAL*8 X
      INTEGER FLAG
C
      REAL*8 DGAUSS
C
C----------------------------------------------------------------------
C
C  Calculate confidence level for the cross section given by X
C
      FCONLV = DGAUSS(FSIGMA,SIG_MIN(ICL),X,EPS) + SIG_INT(ICL-1)
      FCONLV = FCONLV / SIG_INT(ITER) - CL
C
  999 RETURN
      END
