C----------------------------------------------------------------------
      Integer Function NWires_Hit(Theta,ZV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns number of wires within CDC fiducial
C-                         volume on the road.
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   8-NOV-1993   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      Integer     I
      Real        Theta, ZV, RMin, Step, CT, ZMax
      Data        RMin /52.09/, Step /0.72/, ZMax /89.71/
C
      NWires_Hit = 0
      CT = 1./TAN(Theta)
      Do I = 0,27
        If (Abs(ZV + (RMin + I*Step)*CT) .lt. ZMax)
     &    NWires_Hit = NWires_Hit + 1
      End Do
C----------------------------------------------------------------------
  999 RETURN
      END
