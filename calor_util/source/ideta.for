      Integer Function IDETA(ETA1,ETA2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return absolute difference between calorimeter
C-                         addresses ETA1 and ETA2
C-
C-   Inputs  :      ETA1, ETA2  = calorimeter eta of two cells
C-   Return value : IDETA       = absolute difference in cal. etas
C-   Controls: 
C-
C-   Created  19-APR-1995   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      Integer   Eta1, Eta2
C
      If (Eta1*Eta2 .gt. 0) Then        ! Have same sign
        IDETA = ABS(Eta1 - Eta2)
      Else If (Eta1*Eta2 .lt. 0) Then   ! Different sign: no IETA = 0!
        IDETA = ABS(Eta1 - Eta2) - 1 
      Else
C        Type *,'Wrong calorimeter Eta: ',ETA1,ETA2
      End If
C----------------------------------------------------------------------
  999 RETURN
      END
