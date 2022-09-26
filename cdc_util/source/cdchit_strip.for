C----------------------------------------------------------------------
      Subroutine CDCHIT_Strip(ZV,NH,IH,PhiMin,PhiMax,TheMin,TheMax,NG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns number of segments on the road
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   9-NOV-1993   Gregory L. Landsberg
C-   Updated  27-Jan-1996   sss - compile with g77.
C-
C----------------------------------------------------------------------
      Implicit      None
      Real          Pi, TwoPi, HalfPi, Rad
      Include      'D0$INC:PI.INC'
      Include      'D0$INC:ZEBCOM.INC'
      Integer       IH(1), NH, K, L, NW
      Integer       NG, GZDTSG, LDTSG, I, J, IWord
      Real          PhiMin, PhiMax, TheMin, TheMax, X, Y, Phi, The
      Real          R, Z, ZV, Z1, Z2, ZHit
      Logical       LCheck
      Common /CDCHIT/ NW
C
      integer mask_0003fffe
      data mask_0003fffe / z'0003FFFE' /
      integer mask_00400000
      data mask_00400000 / z'00400000' /
      integer mask_FFBFFFFF
      data mask_FFBFFFFF / z'FFBFFFFF' /
C
      NG = 0
      If (NH .le. 0) Return
C
C ---- Throw away hits which are on the tracks
C
      I = 1
      Do While (I .le. NH)
        If (IAND(IH(I),mask_00400000) .ne. 0) Call Squeeze(I,NH,IH)
        I = I + 1
      End Do
      NW = NH
      If (NH .eq. 0) Return
C
      Do 1 I = 0,3          ! Loop over layers
        LDTSG = GZDTSG(I)
        If (LDTSG .le. 0) Go To 1
        Do 5 J = 1,IQ(LDTSG+1)
          If (IQ(LDTSG+22*J-19) .ne. 0) Then  ! segment is on a DTRAK
            If (NH .eq. 0) Return
            Do K = 5,11
              IWORD = IAND(IQ(LDTSG+22*J-K),mask_0003FFFE)
              L = 1
              Do While (L .le. NH)
                If (IWORD .eq. IAND(IH(L),mask_0003FFFE)) Then
                  If ((iand(IH(L), mask_00400000)) .eq. 0)
     &              Call Squeeze(L,NH,IH)
                End If
                L = L + 1
              End Do
            End Do
            Go To 5
          End If
          X = Q(LDTSG+22*J-17)
          Y = Q(LDTSG+22*J-16)
          R = SQRT(X**2+Y**2)
          Phi = ACOS(X/R)
          If (Y .LT. 0) Phi = TwoPi - Phi
          If (LCheck(Phi,PhiMin,PhiMax)) Then
            The = (TheMin + TheMax)/2.
            If (IQ(LDTSG+22*J-11) .eq. 0) Then
              If (IQ(LDTSG+22*J-5) .eq. 0) Then
                Go To 4
              Else
                Z = ZHit(iand(IQ(LDTSG+22*J-5), mask_0003FFFE))
                If (Abs(Z) .gt. 99) Go To 4
              End If
            Else If (IQ(LDTSG+22*J-5) .eq. 0) Then
              Z = ZHit(iand(IQ(LDTSG+22*J-11), mask_0003FFFE))
              If (Abs(Z) .gt. 99) Go To 4
            Else
              Z1 = ZHit(iand(IQ(LDTSG+22*J-11), mask_0003FFFE))
              Z2 = ZHit(iand(IQ(LDTSG+22*J-5), mask_0003FFFE))
              If (Abs(Z1) .gt. 99) Then
                If (Abs(Z2) .gt. 99) Go To 4
                Z = Z2
              Else If (Abs(Z2) .gt. 99) Then
                Z = Z1
              Else
                Z = (Z1 + Z2)/2.
              End If
            End If
            The = ATAN2(R,Z-ZV)
    4       Continue
            If ((The .gt. TheMin) .and. (The .lt. TheMax)) Then ! Count hits
              Do K = 5,11
                IWORD = IAND(IQ(LDTSG+22*J-K),mask_0003FFFE)
                Do L = 1,NH
                  If (IWORD .eq. IAND(IH(L),mask_0003FFFE)) Then
                    If (IAND(IH(L),mask_00400000) .eq. 0) Then
                      IH(L) = IOR(IH(L),mask_00400000)
                      NG = NG + 1
                    End If
                  End If
                End Do
              End Do
            Else  ! Throw away hits on this segment
              If (NH .eq. 0) Return
              Do K = 5,11
                IWORD = IAND(IQ(LDTSG+22*J-K),mask_0003FFFE)
                L = 1
                Do While (L .le. NH)
                  If (IWORD .eq. IAND(IH(L),mask_0003FFFE)) Then
                    If (iand(IH(L), mask_00400000) .eq. 0)
     &                Call Squeeze(L,NH,IH)
                  End If
                  L = L + 1
                End Do
              End Do
            End If
          End If
    5   Continue
    1 Continue
C
      Do I = 1,NH
        IH(I) = IAND(IH(I),mask_FFBFFFFF)
      End Do
C----------------------------------------------------------------------
      Return
      End
