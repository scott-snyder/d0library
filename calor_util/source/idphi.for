      Integer Function IDPHI(PHI1,PHI2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return absolute difference between calorimeter
C-                         addresses PHI1 and PHI2
C-
C-   Inputs  :      PHI1, PHI2  = calorimeter phi of two cells (1..64)
C-   Return value : IDETA       = absolute difference in cal. phis (0..32)
C-   Controls: 
C-
C-   Created  19-APR-1995   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      Integer   Phi1, Phi2
C
      If (Phi1.gt.64 .or. Phi1.le.0 .or. Phi2.gt.64 .or. Phi2.le.0) Then
C        Type *,'Wrong calorimeter Phi: ',Phi1,Phi2
      Else
        IDPHI = ABS(Phi1 - Phi2)
        If (IDPHI .gt. 32) IDPHI = 64 - IDPHI
      End If
C----------------------------------------------------------------------
  999 RETURN
      END
