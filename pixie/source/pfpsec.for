      SUBROUTINE PFPSEC(ICELL,TCELL,INR,OUTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draws one sector (ICELL) of the end view of 
C-                         PHI.
C-
C-   Inputs  : ICELL - Sector number to be drawn
C-             TCELL - Total number of cells in end view
C-             INR   - Inner radius of end view
C-             OUTR  - Outer radius of end view
C-
C-   Created  20-FEB-1989   Lupe Rosas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL RANG
      REAL RAD(2)
      REAL ANG(2)
      REAL INR,OUTR
      REAL DELANG ! ANGULAR 1/2 WIDTH OF CELL IN DEGREES
      INTEGER ICELL,TCELL
C----------------------------------------------------------------------
      DELANG=360./(TCELL*2.)
      RANG=0.+(FLOAT(ICELL))*DELANG*2.
C
C  Calculate the center position of the first drawn sector, in this
C  case, sector 0 is drawn first.
C
      ANG(1)=-DELANG+RANG
      ANG(2)=+DELANG+RANG
      RAD(1)=INR
      RAD(2)=OUTR
C
C  Draw sector 
C  
      CALL JPINTR(1)
      CALL PTSECT(RAD,ANG,1.)
C----------------------------------------------------------------------
  999 RETURN
      END
