      LOGICAL FUNCTION ENDSEG(XG,YG,AL,XG1,YG1,AL1,TOL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Interface to REAL FUNCTION ENDSEG1.
C-
C-   Inputs  : XG,YG,AL    - cog and asmuth of 1st segment
C-             XG1,YG1,AL1 - cog and asmuth of 2nd segment
C-             TOL  - maximum separation of segments extrapolated to bisector of
C-                    line connecting COG points. 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   5-OCT-1993   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL XG,YG,AL,XG1,YG1,AL1,TOL
      REAL ENDSEG1
C----------------------------------------------------------------------
      ENDSEG = ENDSEG1(XG,YG,AL,XG1,YG1,AL1) .LE. TOL
  999 RETURN
      END
