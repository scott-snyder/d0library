      SUBROUTINE SLFIT(X,Y,S,N,A,B,CHIS,SA,SB,COR,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Straight-line fit Y = AX + B to the points
C-                         with errors
C-
C-   Inputs  : X(N), Y(N), S(N) -- REAL arrays with abcissa, ordinate,
C-                                 and error on each data point
C-             N                -- INTEGER number of points
C-   Outputs : A, B -- coefficients
C-             CHIS -- chi**2 of the fit
C-             SA, SB, COR -- errors on parameters A, B, and the correlation
C-   Controls: IERR =  0 on success
C-             IERR = -1 for D =< 0
C-             S(I) <= 0 -- skip the point
C-
C-   Created   2-MAY-1995   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER   N, IERR, I
      REAL      X(N), Y(N), S(N)
      REAL      A, B, CHIS, SA, SB, COR, S11, S12, S22, G1, G2, G3, D
C----------------------------------------------------------------------
      S11 = 0.
      S12 = 0.
      S22 = 0.
      G1  = 0.
      G2  = 0.
      G3  = 0.
      Do I = 1,N
        If (S(I) .gt. 0) Then
          D = 1./S(I)**2
          S11 = S11 + D
          S12 = S12 + X(I)*D
          S22 = S22 + X(I)**2*D
          G1  = G1  + Y(I)*D
          G2  = G2  + X(I)*Y(I)*D
          G3  = G3  + Y(I)**2*D
        End If
      End Do
      D = S11*S22 - S12**2
      If (D .le. 0.) Then
        A    =  0.
        B    =  0.
        SA   =  0.
        SB   =  0.
        COR  =  0.
        CHIS =  0.
        IERR = -1
        Return
      End If
      D = 1./D
      A = (G2*S11 - G1*S12)*D
      B = (G1*S22 - G2*S12)*D
      SA = SQRT(S11*D)
      SB = SQRT(S22*D)
      COR = -S12*D
      CHIS = (A**2*S22 + B**2*S11 + G3) + 2.*(A*B*S12 - A*G2 - B*G1)
C
  999 RETURN
      END
