      Subroutine CDCHIT_INFO(ZV,PhiMin,PhiMax,TheMin,TheMax,Hits_Segm,
     &                       NH,NHC,N3D,NSegZ,RW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns CDC hit information for HMTE/P banks
C-
C-   Inputs  : ZV - current vertex Z-coordinate
C-             PhiMin, PhiMax - road size in Phi
C-             TheMin, TheMax - road size in Theta
C-             HITS_SEGM - if .True. perform segment analysis
C-   Outputs : NH, NHC - total and corrected # of XY-hits on the road
C-             N3D - number of 3D-hits on the road
C-             NSegZ - number of segments with Z-info on the road
C-             RW - fraction of hitted wires
C-   Controls: None
C-
C-   Created  23-FEB-1994   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      Implicit    None
      Include    'D0$INC:ZEBCOM.INC'
      Real        ZV, PhiMin, PhiMax, TheMin, TheMax, RW, W
      Real        Coord(3,128), RH(1024), PH(1024), ZC(256)
      Integer     IH(1024), IZ(256), GZDLYR, GZDTSG, NZ, NSeg
      Integer     GZCDCH, LCDCH, LDLYR, IFlag(3)
      Integer     NH, NHC, N3D, NSegZ, NCell, NW, NHW, NGood, LAYER
      Logical     Hits_Segm
C
      Call CDCHIT_HITS(ZV,PhiMin,PhiMax,TheMin,TheMax,
     &                 NW,NHW,NCell,NHC,IH,RH,PH,NZ,IZ,ZC,N3D,Coord)
C
      If (NW .gt. 0) Then
        RW = Float(NHW)/Float(NW)
        W  = 28./Float(NW)
        NH = Min(Int(NHC*W),255)
      Else
        RW = 1.
        W  = 28.
        NH = 255
      End If
      N3D = MIN(N3D*W,15.)
C
      If ( HITS_SEGM ) Then
        IF (GZDTSG(0) .LE. 0) THEN
           CALL ERRMSG('HITSINFO','CDCHIT_INFO',
     &    'NO CDC TRACK SEGMENTS','W')
           NSEGZ = 0
        ELSE
          Call CDCHIT_SEGM(ZV,PhiMin,PhiMax,TheMin,TheMax,NSeg,IFlag)
          NSegZ = MIN((IFlag(2) + IFlag(3))*W,15.)
        ENDIF
      End If
C
      Call CDCHIT_Strip(ZV,NHC,IH,PhiMin,PhiMax,TheMin,TheMax,NGood)
      NHC = MIN(NHC*W,255.)
C----------------------------------------------------------------------
  999 Return
      End
