C----------------------------------------------------------------------
      Real Function ETAD(ZV,THETA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate detector eta given theta, z vtx
C-
C-   Returned value  : Detector eta
C-   Inputs  : ZV, Theta - primary vertex position and physical theta.
C-   Outputs : None
C-   Controls: None
C-
C-   Created   7-JUN-1993   Anthony L. Spadafora
C-   Updated  21-FEB-1994   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      Implicit    None
      Real        ZV,THETA
      Real        RCC,ZCC_MAX,ZEC
      Parameter ( RCC =91.6 )         ! EM3
      Parameter ( ZCC_MAX = 135.85 )  ! ETA = 1.2 AT R=90 CM
      Parameter ( ZEC = 178.9 )       ! EM3
      Real        Z
C
      Z = ZV + RCC/TAN(THETA)
      If(ABS(Z) .le. ZCC_MAX) Then  ! in CC
        Z = Z/RCC
      Else                          ! in EC
        If(COS(THETA) .gt. 0.) Then
          Z = 1./(1. - ZV/ZEC)
        Else
          Z = 1./(1. + ZV/ZEC)
        Endif
        Z = Z/TAN(THETA)
      Endif
      ETAD = ALOG(Z + SQRT(Z**2+1.))
C
  999 Return
      End
