C----------------------------------------------------------------------
      Logical Function LCheck(Phi,PhiMin,PhiMax)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Checks if Phi belongs to [PhiMin,PhiMax]
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   4-NOV-1993   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      Implicit        None
      Real            Pi, TwoPi, HalfPi, Rad
      Include        'D0$INC:PI.INC'
      Real            Phi, PhiMin, PhiMax
C
      LCheck = .True.
      If ((PhiMin .lt. 0) .and. (PhiMin + TwoPi .lt. Phi))      Return
      If ((PhiMax. gt. TwoPi) .and. (PhiMax - TwoPi .gt. Phi))  Return
      If ((PhiMin .lt. Phi) .and. (Phi .lt. PhiMax))            Return
      LCheck = .False.
C----------------------------------------------------------------------
      Return
      End
