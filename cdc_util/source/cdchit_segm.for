C----------------------------------------------------------------------
      Subroutine CDCHIT_Segm(ZV,PhiMin,PhiMax,TheMin,TheMax,NSeg,IFlag)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns number of segments on the road
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   9-NOV-1993   Gregory L. Landsberg
C-   Updated   7-FEB-1995   Gregory L. Landsberg  -- Switch to VXY_BEAM1 
C-   Updated  27-Jan-1996   sss - compile with g77.
C-
C----------------------------------------------------------------------
      Implicit      None
      Real          Pi, TwoPi, HalfPi, Rad
      Include      'D0$INC:PI.INC'
      Include      'D0$INC:ZEBCOM.INC'
      Integer       NSeg, GZDTSG, LDTSG, I, J, IFlag(3), IFL, ISTATUS
      Real          PhiMin, PhiMax, TheMin, TheMax, X, Y, Phi, Theta
      Real          R, Z, Z1, Z2, ZHit, ZV, XV, YV, ZVTX, DXV, DYV
      Logical       LCheck
C
      integer mask_0003fffe
      parameter (mask_0003fffe = '0003FFFE'X)
C
      IFL = 0
      NSeg = 0
      IFlag(1) = 0
      IFlag(2) = 0
      IFlag(3) = 0
      CALL VXY_BEAM1(ZV,XV,DXV,YV,DYV,ISTATUS)
      Do I = 0,3          ! Loop over layers
        LDTSG = GZDTSG(I)
        Do 5 J = 1,IQ(LDTSG+1)
          X = Q(LDTSG+22*J-17)-XV
          Y = Q(LDTSG+22*J-16)-YV
          R = SQRT(X**2+Y**2)
          Phi = ACOS(X/R)
          If (Y .LT. 0) Phi = TwoPi - Phi
          If (LCheck(Phi,PhiMin,PhiMax)) Then
            If (IQ(LDTSG+22*J-11) .eq. 0) Then
              If (IQ(LDTSG+22*J-5) .eq. 0) Then
                NSeg = NSeg + 1
                IFlag(IFL+1) = IFlag(IFL+1) + 1
                Go To 5
              Else
                Z = ZHit(IAND(IQ(LDTSG+22*J-5),mask_0003FFFE))
                If (Abs(Z) .gt. 99) Then
                  NSeg = NSeg + 1
                  IFlag(IFL+1) = IFlag(IFL+1) + 1
                  Go To 5
                End If
                IFL = 1
                Go To 4
              End If
            Else If (IQ(LDTSG+22*J-5) .eq. 0) Then
              Z = ZHit(IAND(IQ(LDTSG+22*J-11),mask_0003FFFE))
              If (Abs(Z) .gt. 99) Then
                NSeg = NSeg + 1
                IFlag(IFL+1) = IFlag(IFL+1) + 1
                Go To 5
              End If
              IFL = 1
              Go To 4
            Else
              Z1 = ZHit(IAND(IQ(LDTSG+22*J-11),mask_0003FFFE))
              Z2 = ZHit(IAND(IQ(LDTSG+22*J-5),mask_0003FFFE))
              If (Abs(Z1) .gt. 99) Then
                If (Abs(Z2) .gt. 99) Then
                  NSeg = NSeg + 1
                  IFlag(IFL+1) = IFlag(IFL+1) + 1
                  Go To 5
                End If
                Z = Z2
                IFL = 1
                Go To 4
              Else If (Abs(Z2) .gt. 99) Then
                Z = Z1
                IFL = 1
                Go To 4
              Else
                Z = (Z1 + Z2)/2.
                IFL = 2
                Go To 4
              End If
            End If
    4       Theta = ATAN2(R,Z-ZV)
            If ((Theta .gt. TheMin) .and. (Theta .lt. TheMax)) Then
              NSeg = NSeg + 1
              IFlag(IFL+1) = IFlag(IFL+1) + 1
            End If
          End If
    5   Continue
      End Do
C----------------------------------------------------------------------
  999 RETURN
      END
