C----------------------------------------------------------------------
C
C                   DDDDDDDD           0000   0
C                   D       D         0    0 0
C                   D        D       0      0
C                   D         D     0      0 0
C                   D         D     0     0  0
C                   D         D     0    0   0
C                   D         D     0   0    0
C                   D         D     0  0     0
C                   D        D       00     0
C                   D       D        00    0
C                   DDDDDDDD        0  0000
C
C
C----------------------------------------------------------------------
C
C#######################################################################
      PROGRAM LO_MAIN_R
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To calculate leading order triple differential
C-                          cross sections - d3sigma/(dVAR1*dVAR2*dVAR3)
C-
C-   Inputs  :             User is Prompted for Input Variables
C-   Outputs :             N-tuple - XT and List of Inputs to Calculation
C-   Controls:             User is Prompted for Controls
C-
C-   Created  18-Oct-1993   Sandor Feher and Patrick Mooney
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C----------------------------------------------------------------------
C
C   Read INPUT from LO_RCP file
C
      CALL LO_IN_R
C
C----------------------------------------------------------------------
C
C   Perform CALCULATIONS and fill N-tuples
C
      CALL LO_CALC
C
C----------------------------------------------------------------------
C
C   Perform end
C
      CALL LO_OUT
C
C----------------------------------------------------------------------
      END
