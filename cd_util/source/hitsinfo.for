      Subroutine HITSINFO(LHMTP,PACKED)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Analyzes and returns packed hit information
C-                         in the central detector
C-
C-   Inputs  :  LHMTP     -- pointer to either HMTP or HMTE bank
C-   Outputs :  PACKED(3) -- INTEGER array with packed hit info
C-   Controls:
C-
C-   Created  23-FEB-1994   Gregory L. Landsberg
C-   Updated   6-MAR-1994   Gregory L. Landsberg -- introducing VTX code
C-   Updated  31-AUG-1994   Gregory L. Landsberg -- VXY_BEAM call 
C-   Updated   7-FEB-1995   Gregory L. Landsberg -- VXY_BEAM1 call 
C-   Updated  15-FEB-1995   Gregory L. Landsberg -- Check CDC/FDC/VTXON,VERIFY 
C-   Updated  28-FEB-1995   Steven M. Glenn - Added TRD hit information 
C-   Updated  27-APR-1995   Gregory L. Landsberg -- Added NCloud info
C-   Updated   7-JUL-1995   Gregory L. Landsberg -- LVERT=0 case debugged
C-
C----------------------------------------------------------------------
      Implicit      None
      Save          L_First, L_Run, L_Hist, L_VTX
      Real          Pi, TwoPi, HalfPi, Rad, Eta, Phi0, Theta0
      Real          Theta, Phi, TheMin, TheMax, PhiMin, PhiMax, RR
      Real          ZV(20)
      Include      'D0$INC:ZEBCOM.INC'
      Include      'D0$INC:PI.INC'
      Integer       LPPHO, LHMTP, LVERT
      Integer       GZVERH, GZDHIT, GZFHIT
      Integer       N3D, N3D_M, NHW_M
      Integer       NH, NW, NHW, NVert, NWV, NWV_M
      Integer       IV, NH_M, NH2, NH3, NHW2, NHW3, NH2_M, NH3_M
      Integer       NTot, LenOcc
      Integer       NSeg, NSeg_M
      Integer       IErr, PACKED(3), ISTATUS
      Integer       IPho, IPrim, LVERH, NCloud
      integer nanode(3), ncathode(3), ilay, trd_stat, nalh, nclh
      Logical       L_Run, L_Hist, HITS_SEGM, L_First, L_RCP
      Logical       L_VTX, L_Cent, L_CDC, L_FDC, FLGVAL, VRFFLG
      logical l_trd
      Real          FDC_Theta, FDC_Phi, CDC_Theta, CDC_Phi, EtaD
      Real          VTX_Theta, VTX_Phi, Eps, XV, DXV, YV, DYV
      real trd_theta, trd_phi, eanode(6,3), ecathode(6,3)
      real v1(6), v2(6), sinth
      Real          X, Y, Z, RW, RW_M, R2, R3, R2_M, R3_M, R0
      Data          L_First /.True./, L_Hist /.True./, L_Run /.True./
      Data          L_VTX /.False./, Eps / 1.E-9 /
      Character*255 CurDir

      integer ihpelc/4HPELC/
      integer ihppho/4HPPHO/

      integer z00000001
      data z00000001 / z'00000001' /
      integer z00000002
      data z00000002 / z'00000002' /
      integer z80000000
      data z80000000 / z'80000000' /
      integer z00000008
      data z00000008 / z'00000008' /
      integer z00000010
      data z00000010 / z'00000010' /
C
      PACKED(1) = 0
      PACKED(2) = 0
      PACKED(3) = 0
      If (LHMTP .eq. 0) Then
        Call ErrMsg('CAPHEL','HITSINFO',
     &              'Zero pointer to HMTP/HMTE bank','W')
        Return
      End If
      If (L_First) Then
        L_First = .False.
        L_RCP = .True.
    2   CALL EZPICK('CAPHEL_RCP')
        CALL EZERR(IErr)
        If (IErr .ne. 0) THEN
          If (L_RCP) Then
            L_RCP = .False.
            Call ErrMsg('CAPHEL','HITSINFO',
     &                'CAPHEL_RCP was not opened','W')
            CALL INRCP('CAPHEL_RCP',IErr)       ! read in RCP file
            CALL EZERR(IErr)
            If (IErr .ne. 0) Then
              Call ErrMsg('CAPHEL','HITSINFO',
     &                  'Failed to read CAPHEL_RCP','F')
              Return
            End If
            Go To 2
          Else
            Call ErrMsg('CAPHEL','HITSINFO',
     &                        'Failed to find CAPHEL_RCP bank','F')
            Return
          End If
        End If
        CALL EZGET_l('HITS_INFO',L_Run,IErr)
        If (.not. L_Run) Then
          L_Hist = .False.
          Go To 999
        End If
        If ((IErr .eq. 0) .and. L_Hist)
     &                   CALL EZGET_l('HITS_HIST',L_Hist,IErr)
        If (IErr .eq. 0) CALL EZGET_l('HITS_SEGM',HITS_SEGM,IErr)
        If (IErr .eq. 0) CALL EZGET('FDC_THETA',FDC_Theta,IErr)
        If (IErr .eq. 0) CALL EZGET('FDC_PHI',FDC_Phi,IErr)
        If (IErr .eq. 0) CALL EZGET('CDC_THETA',CDC_Theta,IErr)
        If (IErr .eq. 0) CALL EZGET('CDC_PHI',CDC_Phi,IErr)
        If (IErr .eq. 0) CALL EZGET_l('HITS_VTX',L_VTX,IErr)
        If (L_VTX) Then
          If (IErr .eq. 0) CALL EZGET('VTX_THETA',VTX_Theta,IErr)
          If (IErr .eq. 0) CALL EZGET('VTX_PHI',VTX_Phi,IErr)
        End If
        if (ierr .eq. 0) call ezget_l('HITS_TRD',l_trd,ierr)
        if (l_trd) then
           if (ierr .eq. 0) call ezget('TRD_THETA',trd_theta,ierr)
           if (ierr .eq. 0) call ezget('TRD_PHI',trd_phi,ierr)
        endif
C
        If (IErr .ne. 0) Call ErrMsg('CAPHEL','HITSINFO',
     &                        'Failed to read CAPHEL_RCP bank','F')
        CALL EZRSET
C
        VRFFLG = FLGVAL('VERIFY')
        If (VRFFLG) L_Hist = .True.
C
        L_RCP = .True.
    3   CALL EZPICK('ZTRAKS_RCP')
        CALL EZERR(IErr)
        If (IErr .ne. 0) THEN
          If (L_RCP) Then
            L_RCP = .False.
            Call ErrMsg('CAPHEL','HITSINFO',
     &                'ZTRAKS_RCP was not opened','W')
            CALL INRCP('ZTRAKS_RCP',IErr)       ! read in RCP file
            CALL EZERR(IErr)
            If (IErr .ne. 0) Then
              Call ErrMsg('CAPHEL','HITSINFO',
     &                  'Failed to read ZTRAKS_RCP','F')
              Return
            End If
            Go To 3
          Else
            Call ErrMsg('CAPHEL','HITSINFO',
     &                        'Failed to find ZTRAKS_RCP bank','F')
            Return
          End If
        End If
C
        CALL EZGET_l('CDCON',L_CDC,IErr)
        If (IErr .eq. 0) CALL EZGET_l('FDCON',L_FDC,IErr)
        If (IErr .eq. 0) CALL EZGET_l('VTXON',L_Cent,IErr)
        If (IErr .eq. 0) CALL EZGET_l('TRDON',L_TRD,IErr)
        If (IErr .ne. 0) Call ErrMsg('CAPHEL','HITSINFO',
     &                        'Failed to read ZTRAKS_RCP bank','F')
        If (.not.L_Cent) L_VTX = .False.  ! ZTRAKS setting supersedes 
        CALL EZRSET
C
        If (.not. L_Hist) Go To 1
C
C---------> General histograms are coming <-----------
C
        CALL HCDIR(CURDIR,'R')
        CALL DHDIR('CAPHEL_RCP','HBOOK_DIRECTORY3',IErr,' ')
        IF (IERR .NE. 0) Then
          CALL ERRMSG('HITSINFO-W-HOHBK','HITSINFO',
     &                'Error setting HBOOK directory','W')
          Call HCDIR(CURDIR,' ')
          L_Hist = .False.
        End If
C
        Call Hbook1(100,'Number of FDC hits$',100,0.,2500.,0.)
        Call Hbook1(101,'Number of CDC hits$',100,0.,2500.,0.)
        Call HBook1(102,'Number of vertices$',10,-0.5,9.5,0.)
C
        Call Hbook1(1001,'Detector eta for photon$',30,-3.,3.,0.)
        Call HBook1(1002,'Truncated Chi**2 for photon$',30,0.,300.,0.)
        Call HBook1(1003,'Isolation for photon$',20,0.,0.2,0.)
        Call HBook1(1004,'Pt of photon$',50,0.,100.,0.)
C
        Call Hbook1(1101,'Detector eta for electron$',30,-3.,3.,0.)
        Call HBook1(1102,'Truncated Chi**2 for electron$',30,0.,300.,0.)
        Call HBook1(1103,'Isolation for electron$',20,0.,0.2,0.)
        Call HBook1(1104,'Pt of electron$',50,0.,100.,0.)
C
C---------> FDC related histograms are coming <-----------
C
C ********  Photons
        Call Hbook1(2001,'Max number of FDC hits, photon$',
     &    50,-0.5,149.5,0.)
        Call Hbook1(2002,'Max number of hit FDC wires, photon$',
     &    33,-0.5,32.5,0.)
        Call Hbook1(2003,'Ratio of hit FDC wires, photon$',
     &    25,0.,1.,0.)
C ********  Electrons
        Call Hbook1(2101,'Max number of FDC hits, electron$',
     &    50,-0.5,149.5,0.)
        Call Hbook1(2102,'Max number of hit FDC wires, electron$',
     &    33,-0.5,32.5,0.)
        Call Hbook1(2103,'Ratio of hit FDC wires, electron$',
     &    25,0.,1.,0.)
C
C---------> CDC related histograms are coming <-----------
C
C ********  Photons
        Call Hbook1(3001,'Max number of CDC XY-hits, photon$',
     &    50,0.,50.,0.)
        Call Hbook1(3002,'Max number of corr. XY-hits, photon$',
     &    50,0.,50.,0.)
        Call HBook1(3004,'Max number of 3D-hits on the road, photon$',
     &    20,-0.5,19.5,0.)
        Call Hbook1(3005,'Max number of CDC Z-segments, photon$',
     &    20,-0.5,19.5,0.)
        Call HBook1(3006,'Max ratio of hit wires, photon$',
     &    20,0.,1.,0.)
C ********  Electrons
C
        Call Hbook1(3101,'Max number of CDC XY-hits, electron$',
     &    50,0.,50.,0.)
        Call Hbook1(3102,'Max number of corr. XY-hits, electron$',
     &    50,0.,50.,0.)
        Call HBook1(3104,'Max number of 3D-hits on the road, electron$',
     &    20,-0.5,19.5,0.)
        Call Hbook1(3105,'Max number of CDC Z-segments, electron$',
     &    20,-0.5,19.5,0.)
        Call HBook1(3106,'Max ratio of hit wires, electron$',
     &    20,0.,1.,0.)
C
C---------> VTX related histograms are coming <-----------
C
C ********  Photons
        Call Hbook1(4001,'Max number of hit FDC wires, photon$',
     &    25,-0.5,24.5,0.)
        Call Hbook1(4002,'Max number of FDC XY-hits, photon$',
     &    64,-0.5,127.5,0.)
        Call Hbook1(4003,'Ratio of XY-hit FDC wires, photon$',
     &    25,0.,1.,0.)
        Call Hbook1(4004,'Max number of FDC 3D-hits, photon$',
     &    32,-0.5,63.5,0.)
        Call Hbook1(4005,'Ratio of 3D-hit FDC wires, photon$',
     &    25,0.,1.,0.)
C ********  Electrons
        Call Hbook1(4101,'Max number of hit FDC wires, electron$',
     &    25,-0.5,24.5,0.)
        Call Hbook1(4102,'Max number of FDC XY-hits, electron$',
     &    64,-0.5,127.5,0.)
        Call Hbook1(4103,'Ratio of XY-hit FDC wires, electron$',
     &    25,0.,1.,0.)
        Call Hbook1(4104,'Max number of FDC 3D-hits, electron$',
     &    32,-0.5,63.5,0.)
        Call Hbook1(4105,'Ratio of 3D-hit FDC wires, electron$',
     &    25,0.,1.,0.)
C
C   TRD histograms
C
C ********  Photons
C
        call hbook1(5001, 'Hit anodes in Layer 1, photons$',
     &     8, 0.0, 8.0, 0.0)
        call hbook1(5002, 'Hit anodes in Layer 2, photons$',
     &     8, 0.0, 8.0, 0.0)
        call hbook1(5003, 'Hit anodes in Layer 3, photons$',
     &     8, 0.0, 8.0, 0.0)
        call hbook1(5004, 'Hit cathodes in Layer 1, photons$',
     &     8, 0.0, 8.0, 0.0)
        call hbook1(5005, 'Hit cathodes in Layer 2, photons$',
     &     8, 0.0, 8.0, 0.0)
        call hbook1(5006, 'Hit cathodes in Layer 3, photons$',
     &     8, 0.0, 8.0, 0.0)
        call hbook1(5007, 'Total hit anodes, photons$',
     &     10, 0.0, 10.0, 0.0)
        call hbook1(5008, 'Total hit cathodes, photons$',
     &     10, 0.0, 10.0, 0.0)
        call hbook1(5009, 'Number of hit anode layers, photons$',
     &     10, 0.0, 10.0, 0.0)
        call hbook1(5010, 'Number of hit cathode layers, photons$',
     &     10, 0.0, 10.0, 0.0)
C
C ********  Electrons
C
        call hbook1(5101, 'Hit anodes in Layer 1, electrons$',
     &     8, 0.0, 8.0, 0.0)
        call hbook1(5102, 'Hit anodes in Layer 2, electrons$',
     &     8, 0.0, 8.0, 0.0)
        call hbook1(5103, 'Hit anodes in Layer 3, electrons$',
     &     8, 0.0, 8.0, 0.0)
        call hbook1(5104, 'Hit cathodes in Layer 1, electrons$',
     &     8, 0.0, 8.0, 0.0)
        call hbook1(5105, 'Hit cathodes in Layer 2, electrons$',
     &     8, 0.0, 8.0, 0.0)
        call hbook1(5106, 'Hit cathodes in Layer 3, electrons$',
     &     8, 0.0, 8.0, 0.0)
        call hbook1(5107, 'Total hit anodes, electrons$',
     &     10, 0.0, 10.0, 0.0)
        call hbook1(5108, 'Total hit cathodes, electrons$',
     &     10, 0.0, 10.0, 0.0)
        call hbook1(5109, 'Number of hit anode layers, electrons$',
     &     10, 0.0, 10.0, 0.0)
        call hbook1(5110, 'Number of hit cathode layers, electrons$',
     &     10, 0.0, 10.0, 0.0)
C
      End If
      If (L_Hist) CALL DHDIR('CAPHEL_RCP','HBOOK_DIRECTORY3',IErr,' ')
C
    1 Continue
      If ( .not. L_Run ) Go To 999
C
      LVERH = GZVERH()
      If (LVERH .le. 0) Then
        LVERT = 0
      Else
        NVERT = IQ(LVERH+2)+IQ(LVERH+3)
        LVERT = LQ(LVERH-1)
        IV = 0
        IPrim = 0
      End If
C
      If (LVERT .le. 0) Then
        ZV(1) = 0.
        NVERT = 1
        IV = 1
        IPrim = 1
      Else
        Do While (LVERT .gt. 0)
          IV = IV + 1
          ZV(IV) = Q(LVERT+5)
          If ((IPrim .eq. 0) .and. (IAND(IQ(LVERT+2),z80000000).ne.0))
     &         IPrim = IV
          LVERT = LQ(LVERT)
        End Do
      End If
      If (IV .ne. NVERT) Then
        Call ErrMsg('CAPHEL','HITSINFO',
     &              'Number of vertices differs from VERH stored','W')
        NVERT = IV
      End If
      If (IPrim .eq. 0) Then
        Call ErrMsg('CAPHEL','HITSINFO','No primary vertices found','W')
        IPrim = 1
      End If
      If (L_Hist) Call HFill(102,Float(NVERT),0.,1.)
      CALL VXY_BEAM1(ZV(1),XV,DXV,YV,DYV,ISTATUS)
C
      If (L_Hist) Then
        NTot = IQ(GZFHIT()+2)
        Call HFill(100,Float(Ntot),0.,1.)
        NTot = IQ(GZDHIT()+2)
        Call HFill(101,Float(Ntot),0.,1.)
      End If
C
      LPPHO = LQ(LHMTP+1)
      If (IQ(LPPHO-4) .eq. iHPPHO) Then
        IPho = 0
      Else If (IQ(LPPHO-4) .eq. iHPELC) Then
        IPho = 1
      Else
        Call ErrMsg('CAPHEL','HITSINFO',
     &              'LPPHO/LPELC points to the wrong bank','W')
        Go To 999
      End If
C
      If (L_Hist) Call HFill(1004+100*IPho,Q(LPPHO+7),0.,1.)
C
      X  = Q(LPPHO+23)
      Y  = Q(LPPHO+24)
      Z  = Q(LPPHO+25)
      Phi0 = Q(LPPHO+10)
      RR = MAX(SQRT((X-XV)**2 + (Y-YV)**2),Eps)
      R0 = MAX(SQRT(X**2 + Y**2),Eps)
C
      Theta = ATAN2(RR,Z-ZV(IPrim))
      Phi = ACOS((X-XV)/RR)
      If (Y-YV .lt. 0) Phi = TwoPi - Phi
C
      Eta = EtaD(ZV(IPrim),Theta)
      If (L_Hist) Call HFill(1001+100*IPho,Eta,0.,1.)
      If (L_Hist) Call HFill(1002+100*IPho,Q(LHMTP+7),0.,1.)
      If (L_Hist) Call HFill(1003+100*IPho,
     &           (Q(LPPHO+16)-Q(LPPHO+17))/MAX(Q(LPPHO+17),Eps),0.,1.)
C
      If (Abs(Eta) .lt. 1.2) Then
        L_Cent = .True.
      Else
        L_Cent = .False.
      End If
C
      NH_M   = 0
      NH2_M  = 0
      NH3_M  = 0
      NHW_M  = 0
      NWV_M  = 0
      N3D_M  = 0
      NSeg_M = 0
      RW_M   = 0.
      R2_M   = 0.
      R3_M   = 0.
      if (l_trd) then
        v1(1) = xv
        v1(2) = yv
        v2(1) = xv
        v2(2) = yv
        do ilay = 1,3
          nanode(ilay) = 0
          ncathode(ilay) = 0
        enddo
        trd_stat = 1
      endif
C
C  Loop over vertices
C
      Do IV = 1,NVert
        Theta  = ATAN2(RR,Z-ZV(IV))
        Theta0 = ATAN2(R0,Z-ZV(IV))
        if (l_trd) Then           ! Define TRD roads
          v1(3) = zv(iv)
          sinth = sin(theta - trd_theta)
          v1(4) = sinth * cos(phi - trd_phi)
          v1(5) = sinth * sin(phi - trd_phi)
          v1(6) = cos(theta - trd_theta)
          sinth = sin(theta + trd_theta)
          v2(3) = zv(iv)
          v2(4) = sinth * cos(phi + trd_phi)
          v2(5) = sinth * sin(phi + trd_phi)
          v2(6) = cos(theta + trd_theta)
        End If
C
        If ( L_VTX ) Then         ! VTX analysis follows...
          TheMin = MAX(Theta - VTX_Theta, 0.)
          TheMax = MIN(Theta + VTX_Theta, Pi)
C
          PhiMin = Phi - VTX_Phi
          PhiMax = Phi + VTX_Phi
C
          Call VHits_In_Road(ZV(IV),PhiMin,PhiMax,TheMin,TheMax,
     &                       NWV,NH2,NHW2,NH3,NHW3)
          If (NWV .lt. 8) Then   ! Do not use VTX info for this road
            NH2 = 0
            NH3 = 0
            R2 = 0.
            R3 = 0.
          Else
            NH2 = Min((NH2*24)/NWV,255)
            NH3 = Min((NH3*24)/NWV,255)
            R2  = Float(NHW2)/Float(NWV)
            R3  = Float(NHW3)/Float(NWV)
          End If
        Else
          NH2 = 0
          NH3 = 0
          NWV = 0
          R2  = 0.
          R3  = 0.
        End If
        If (.not. L_Cent) Then    ! FDC analysis follows...
          If (L_FDC) Then
            TheMin = MAX(Theta0 - FDC_Theta, 0.)
            TheMax = MIN(Theta0 + FDC_Theta, Pi)
C
            PhiMin = Phi0 - FDC_Phi
            PhiMax = Phi0 + FDC_Phi
C
            Call FHIT_CHK(ZV(IV),PhiMin,PhiMax,TheMin,TheMax,NH,NW,NHW,
     &                    .True.)
C
            If (NW .eq. 0) Then
              NH = 255
              RW = 1.
            Else
              NH = Min((NH*32)/NW,255)
              RW = Float(NHW)/Float(NW)
            End If
          Else
            NH  = 0
            NW  = 0
            NHW = 0
            RW  = 0
          End If
C
C  Save FDC and VTX information if FDC+VTX hit fractions maximized for this 
C  vertex.  Also get TRD hits with respect to this vertex.
C
          If (RW + R3 .ge. RW_M) Then
            NH_M  = NH
            NHW_M = NHW
            NWV_M = NWV
            NH2_M = NH2
            NH3_M = NH3
            RW_M  = RW + R3
            R2_M  = R2
            R3_M  = R3
            if (l_trd) call tcell_in_road(v1, v2, nanode, eanode,
     &          ncathode, ecathode, trd_stat)
          End If
        Else                      ! CDC analysis follows...
          If (L_CDC) Then
            TheMin = MAX(Theta - CDC_Theta,0.)
            TheMax = MIN(Theta + CDC_Theta, Pi)
C
            PhiMin = Phi - CDC_Phi
            PhiMax = Phi + CDC_Phi
C
            Call CDCHIT_INFO(ZV(IV),PhiMin,PhiMax,TheMin,TheMax,
     &                       Hits_Segm,NH,NHW,N3D,NSeg,RW)
          Else
            NH   = 0
            NHW  = 0
            N3D  = 0
            NSeg = 0
            RW   = 0
          End If
C
C  Save CDC and VTX information if CDC+VTX hit fractions maximized for this 
C  vertex.  Also get TRD hits with respect to this vertex.
C
          If (RW + R3 .ge. RW_M) Then
            NH_M   = NH
            NHW_M  = NHW
            NWV_M  = NWV
            NH2_M  = NH2
            NH3_M  = NH3
            N3D_M  = N3D
            NSeg_M = NSeg
            RW_M   = RW + R3
            R2_M   = R2
            R3_M   = R3
            if (l_trd) call tcell_in_road(v1, v2, nanode, eanode,
     &          ncathode, ecathode, trd_stat)
          End If
        End If
      End Do                         ! End of loop over verticies
C
      If (L_Hist) Then
        CALL DHDIR('CAPHEL_RCP','HBOOK_DIRECTORY3',IErr,' ')
        If (.not. L_Cent) Then
          Call HFill(IPho*100+2001,Float(NH_M),0.,1.)
          Call HFill(IPho*100+2002,Float(NHW_M),0.,1.)
          Call HFill(IPho*100+2003,RW_M-R3_M,0.,1.)
        Else
          Call HFill(IPho*100+3001,Float(NH_M),0.,1.)
          Call HFill(IPho*100+3002,Float(NHW_M),0.,1.)
          Call HFill(IPho*100+3004,Float(N3D_M),0.,1.)
          Call HFill(IPho*100+3005,Float(NSeg_M),0.,1.)
          Call HFill(IPho*100+3006,RW_M-R3_M,0.,1.)
        End If
        If ( L_VTX ) Then
          Call HFill(IPho*100+4001,Float(NWV_M),0.,1.)
          Call HFill(IPho*100+4002,Float(NH2_M),0.,1.)
          Call HFill(IPho*100+4003,R2_M,0.,1.)
          Call HFill(IPho*100+4004,Float(NH3_M),0.,1.)
          Call HFill(IPho*100+4005,R3_M,0.,1.)
        End If
        if (l_trd.and.trd_stat.eq.0) then
          nalh = 0
          nclh = 0
          do ilay=1, 3
            call hfill(ipho*100+5000+ilay,
     &         float(nanode(ilay)), 0.0, 1.0)
            call hfill(ipho*100+5003+ilay,
     &         float(ncathode(ilay)), 0.0, 1.0)
            if (nanode(ilay).gt.0) nalh = nalh + 1
            if (ncathode(ilay).gt.0) nclh = nclh + 1
          enddo
          call hfill(ipho*100+5007,
     &       float(nanode(1)+nanode(2)+nanode(3)), 0.0, 1.0)
          call hfill(ipho*100+5008,
     &       float(ncathode(1)+ncathode(2)+ncathode(3)), 0.0, 1.0)
          call hfill(ipho*100+5009, float(nalh), 0.0, 1.0)
          call hfill(ipho*100+5010, float(nclh), 0.0, 1.0)
        endif
      End If
      If (L_Cent) Then
        Packed(1) = IOR(PACKED(1),z00000001)
        If (HITS_SEGM)   Packed(1) = IOR(Packed(1),z00000002)
        Packed(1) = IOR(Packed(1),ISHFT(N3D_M,16))
        Packed(1) = IOR(Packed(1),ISHFT(NSeg_M,20))
        Packed(2) = IOR(Packed(2),NHW_M)
        Call GetCloud(NCloud)
        Packed(3) = IOR(Packed(3),ISHFT(NCloud,21))
      Else
        Packed(1) = IOR(PACKED(1),z00000010)
      End If
      Packed(1) = IOR(Packed(1),ISHFT(NH_M,8))
      Packed(1) = IOR(Packed(1),ISHFT(INT((RW_M-R3_M)*100+0.5),24))
      If ( L_VTX ) Then
        If (NWV_M .ge. 8) Packed(1) = IOR(PACKED(1),z00000008)
        Packed(2) = IOR(Packed(2),ISHFT(NH2_M,8))
        Packed(2) = IOR(Packed(2),ISHFT(NH3_M,16))
        Packed(2) = IOR(Packed(2),ISHFT(INT(R3_M*100+0.5),24))
      End If
      if (l_trd.and.(trd_stat.eq.0)) then
         packed(3) = ior(packed(3),1)
         packed(3) = ior(packed(3),ishft(nanode(1),3))
         packed(3) = ior(packed(3),ishft(nanode(2),6))
         packed(3) = ior(packed(3),ishft(nanode(3),9))
         packed(3) = ior(packed(3),ishft(ncathode(1),12))
         packed(3) = ior(packed(3),ishft(ncathode(2),15))
         packed(3) = ior(packed(3),ishft(ncathode(3),18))
      endif
C----------------------------------------------------------------------
  999 Continue
      If (L_Hist .and. (LenOcc(CurDir) .gt. 0)) Call HCDIR(CurDir,' ')
      Return
      End
