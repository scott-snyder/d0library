C----------------------------------------------------------------------
      Subroutine CDCHIT_Hits(ZV,PhiMin,PhiMax,TheMin,TheMax,
     &           NW,NHW,NCell,NHits,IH,RH,PH,NZ,IZ,ZC,N3D,Coord)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get addresses of hits on the road
C-
C-   Inputs  : PhiMin, PhiMax -- minimum and maximum Phi of the road
C-   Outputs : NHits - number of hits on the road (either phi solution)
C-   Controls: None
C-
C-   Created   4-NOV-1993   Gregory L. Landsberg
C-   Updated  23-FEB-1994   Gregory L. Landsberg  -- minor bugs fixed
C-   Updated  22-AUG-1994   Gregory L. Landsberg  -- switch to VXY_BEAM
C-   Updated   7-FEB-1995   Gregory L. Landsberg  -- switch to VXY_BEAM1 
C-   Updated  27-APR-1995   Gregory L. Landsberg  -- added NCloud info 
C-   Updated  27-Jan-1996   sss - compile with g77.
C-
C----------------------------------------------------------------------
      Implicit        None
      Real            Pi, TwoPi, HalfPi, Rad
      Include        'D0$INC:PI.INC'
      Include        'D0$INC:ZEBCOM.INC'
      Real            PhiMin, PhiMax, RH(1024), PH(1024), ZC(256)
      Real            PhiP, PhiN, RPos, RNeg
      Real            ZPos, XArr(2), YArr(2), Area, XV, YV, ZV
      Integer         NCell, NHits, IH(1024), IZ(256), IAddr(128)
      Integer         ISec, ILay, IWir, IWord, I, J, NZ, ITrack
      Integer         LDHIT, GZDHIT, IMask, Words(3), IHit, NHW, I28(28)
      Integer         ISide, IZTRK, N3D, NW, NWires_Hit, ISTATUS
      Integer         NCloud, ICloud
      Logical         LCheck
      Real            Coord(3,1), Theta, TheMin, TheMax, DXV, DYV
C
      integer mask_fffc07ff
      parameter (mask_fffc07ff = 'FFFC07FF'X)
C
      NCell = 0
      Call CDCHIT_Cell(PhiMin,PhiMax,NCell,IAddr)
      If (NCell .le. 0) Return
C
      NW = NWires_Hit((TheMax+TheMin)/2.,ZV)
      CALL VXY_BEAM1(ZV,XV,DXV,YV,DYV,ISTATUS)
C
      LDHIT = GZDHIT()
      If (LDHIT .LE. 0) Then
        Call ERRMSG('CAPHEL','CDCHIT_HITS',
     &              'DHIT bank is not found','W')
        Return
      End If
      NZ = 0
      NHits = 0
      NCloud = 0
      Do I = 1,IQ(LDHIT+2)
        IWORD = IQ(LDHIT + IQ(LDHIT+3)*(I-1) + 4)
        IMASK = IOR(IWORD, mask_FFFC07FF)
        Do J = 1,NCell
          If (IMASK .eq. IAddr(J)) Then      ! The cell matches
            Call GTDHIT(I,WORDS,ILAY,ISEC,IWIR,IHIT,ITrack,ISIDE,IZTRK,
     &                  XARR,YARR,ZPOS,AREA)
            If (ITrack .eq. 0) NCloud = NCloud + 1
            If ((IWIR .eq. 0) .or. (IWIR .eq. 6)) Then
              If ((ZPOS .ne. 999.9) .AND. (ZPOS .NE. 99.99)) Then
                NZ = NZ + 1
                ZC(NZ) = ZPOS
                IZ(NZ) = IWord
              End If
            End If
C
            RPOS = SQRT((XArr(1)-XV)**2 + (YArr(1)-YV)**2)
            PhiP = ACOS((XArr(1)-XV)/RPOS)
            If (YArr(1)-YV .lt. 0) PhiP = TwoPi - PhiP
            If (ITrack .eq. 0) Then     ! Both Phi hit solutions
C                                       ! are checked
              RNEG = SQRT((XArr(2)-XV)**2 + (YArr(2)-YV)**2)
              PhiN = ACOS((XArr(2)-XV)/RNEG)
              If (YArr(2)-YV .lt. 0) PhiN = TwoPi - PhiN
            End If
C
C ----  Check if hit or its mirror reflection is on the road. If both,
C ----  only the first solution is kept in order to avoid double hits.
C
            If ( LCheck(PhiP,PhiMin,PhiMax) ) Then
              NHits = NHits + 1
              IH(NHits) = IWORD
              RH(NHits) = RPOS
              PH(NHits) = PhiP
            Else If ( ITrack .eq. 0 ) Then
              If ( LCheck(PhiN,PhiMin,PhiMax) ) Then
                NHits = NHits + 1
                IH(NHits) = IWORD
                RH(NHits) = RNEG
                PH(NHits) = PhiN
              End If
            End If
          End If
        End Do
      End Do
C
      Do I = 1,28
        I28(I) = 0
      End Do
      Do I = 1,NHits
        IMASK = IH(I)
        ILay = IBITS(IMASK,16,2)
        IWir = IBITS(IMASK,8,3)
        J = 7*ILAY+IWIR+1
        If (J .le. NW) I28(J) = I28(J) + 1
      End Do
      NHW = 0
      Do I = 1,28
        If (I28(I) .ne. 0) NHW = NHW + 1
      End Do
C
      N3D = 0
      Do I = 1,NZ
        IMask = IZ(I)
        Do J = 1,NHits
          If (IH(J) .eq. IMask) Then
            Theta = ATAN2(RH(J),ZC(I)-ZV)
            If ((Theta .gt. TheMin) .and. (Theta .lt. TheMax)) Then
              N3D = N3D + 1
              Coord(1,N3D) = PH(J)
              Coord(2,N3D) = RH(J)
              Coord(3,N3D) = ZC(I)
            End If
          End If
        End Do
      End Do
C----------------------------------------------------------------------
      Return
C
      ENTRY GETCLOUD(ICloud)
C
      ICloud = MIN(1023,NCloud)
      Return
      End
