      SUBROUTINE RESET_TRDCOR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : reset all variables in common TRDCOR
C-
C-   Inputs  : none
C-   Outputs : none (writes directly in TRDCOR common)
C-   Controls: none 
C-
C-   Created  15-JAN-1993   Alain PLUQUET
C-   Updated   3-MAY-1993   Alain PLUQUET  correct bug  
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TRDCOR.INC'      
      CALL VZERO (STATUS,120)
      CALL VFILL (PAD,240,0.)
      CANG=0.
      CEPI=0.
      CAPC=0.
      CGAS=0.
      CSEC=0.
      CWIR=0.
      CWIR=0.
      CHVT=0.
      CELE=0.
      CPED=0.
      EANG=0
      EEPI=0
      EAPC=0
      EGAS=0
      ESEC=0
      EWIR=0
      EWIR=0
      EHVT=0
      EELE=0
      EPED=0
      HVA=0.
      HVP=0.
      HVW=0.
      TCAN=0.
      TTRD=0.
      PCAN=0.
      PTRD=0.
      GCAN=0.
      END
