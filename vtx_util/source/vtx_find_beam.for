      LOGICAL FUNCTION VTX_FIND_BEAM(FillHists, Params, Errs, NUsed, Z0,
     &                               Chi_2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find the beam position.  Requires HBOOK
C-
C-   Returned value  : .TRUE. if fit was successful, .FALSE. otherwise
C-
C-   Inputs  : FillHists, .TRUE. if histograms should be filled.
C-             Tracks must be in COMMON block defined in VTX_ACCUM_TRACKS.INC
C-            (CALL READ_VTXT before using this routine)
C-
C-   Outputs : Params, the following four fit-to parameters:
C-
C-      X, the X-coordinate of the beam (in centimeters) at Z = 0
C-      Y, the Y-coordinate of the beam (in centimeters) at Z = 0
C-      XS, dx/dz of the beam
C-      YS, dy/dz of the beam
C-
C-             Errs, the errors on the above four parameters
C-             NUsed, the number of tracks used in the fit
C-             Z0, the central value of Z (in centimeters) used in the fit
C-             Chi_2, the (unnormalized) chi-squared of the fit
C-
C-   Controls: In VTRAKS_RCP (will try to load VTRAKS_RCPE if no bank)
C-
C-   Created  21-MAR-1994   Justin R. Bendich
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:VTX_ACCUM_TRACKS.INC'
      LOGICAL FillHists
      DOUBLE PRECISION Params(4), Errs(4)
      INTEGER NUsed
      DOUBLE PRECISION Z0, Chi_2
      CHARACTER*10 Dummy
      REAL ZCut, ImpactCut(4), BeamX(2), BeamY(2), dXdZ(2), dYdZ(2),
     &     MinZHist, MaxZHist, ZFitPar(3), Sigmas(3), Chisq, ImpErrCut,
     &     b, DelX, DelY, CosPhi, SinPhi, Delb, Xf, Yf, MinImpHist,
     &     MaxImpHist, MinXHist, MaxXHist, MinYHist, MaxYHist, W,
     &     Chi2Fact
      DOUBLE PRECISION Zero(2), X0, Y0, XSlope, YSlope, UB, LB, Chi2,
     &                 XErr, YErr, XSlpErr, YSlpErr, ZCenter
      INTEGER NIter, Iter, NZBins, NImpBins, NXBins, NYBins, ZHistID,
     &        Ptr, Err, I
      LOGICAL First, HEXIST, FH
      EXTERNAL BeamFit
      COMMON /GBNWX7/ ZCenter, X0, Y0, XSlope, YSlope, Chi2, Chi2Fact,
     &                Iter
      DATA First/.TRUE./, MinZHist/-50/, MaxZHist/50/, Zero/0, 0/,
     &     BeamX/0, 0.1/, BeamY/0, 0.1/, dXdZ/0, 0.0001/, NIter/3/,
     &     dYdZ/0, 0.0001/, NZBins/25/, ZCut/28/, ZHistID/442/,
     &     ImpactCut/0.3, 0.2, 0.2, 0.2/, ImpErrCut/0.02/,
     &     MinImpHist/-0.3/, MaxImpHist/0.3/,MinXHist/-0.2/,
     &     MaxXHist/0.2/, NImpBins/30/, NXBins/30/, NYBins/30/,
     &     MinYHist/-0.2/, MaxYHist/0.2/, Chi2Fact/110/
C
      IF(First) THEN
        First = .FALSE.
        CALL EZLOC('VTRAKS_RCP', Ptr)
        IF(Ptr .LE. 0) THEN
          CALL INRCPE('VTRAKS_RCPE', Err)
        ELSE
          Err = 0
        ENDIF
        IF(Err .EQ. 0) THEN
          CALL EZPICK('VTRAKS_RCP')
          CALL EZGET('MIN_B_HIST', MinImpHist, Err)
          CALL EZGET('MAX_B_HIST', MaxImpHist, Err)
          CALL EZGET('N_B_BINS', NImpBins, Err)
          CALL EZGET('MIN_X_HIST', MinXHist, Err)
          CALL EZGET('MAX_X_HIST', MaxXHist, Err)
          CALL EZGET('N_X_BINS', NXBins, Err)
          CALL EZGET('MIN_Y_HIST', MinYHist, Err)
          CALL EZGET('MAX_Y_HIST', MaxYHist, Err)
          CALL EZGET('N_Y_BINS', NYBins, Err)
          CALL EZGET('MIN_Z_HIST', MinZHist, Err)
          CALL EZGET('MAX_Z_HIST', MaxZHist, Err)
          CALL EZGET('N_Z_BINS', NZBins, Err)
          CALL EZGET('Z_CUT', ZCut, Err)
          CALL EZGET('CHI2_FACT', Chi2Fact, Err)
          CALL EZGET('N_ITER', NIter, Err)
          CALL EZGETA('IMPACT_CUT', 1, NIter - 1, 1, ImpactCut, Err)
          CALL EZGET('IMP_ERR_CUT', ImpErrCut, Err)
          CALL EZGET('BEAM_X', BeamX, Err)         !  Initial value, step size
          CALL EZGET('BEAM_Y', BeamY, Err)         !  Same
          CALL EZGET('DXDZ', dXdZ, Err)            !  Same
          CALL EZGET('DYDZ', dYdZ, Err)            !  Same
          CALL EZRSET
        ELSE
          CALL ERRMSG('INRCPE', 'VTX_FIND_BEAM',
     &      'Couldn''t open VTRAKS_RCPE; using defaults', 'W')
        ENDIF
      ENDIF
C
C ****  Determine the mean, sigma of the "Z of vertex" variable
C
      DO 80 WHILE(HEXIST(ZHistID))
        ZHistID = ZHistID + 1
   80 CONTINUE
      CALL HBOOK1(ZHistID,'Z of Vertex',NZBins,MinZHist,MaxZHist,0.0)
      Ptr = 0
      DO 100 I = 1, NTracks
        CALL HFF1(ZHistID, Ptr, TrackData(I).VertexZ, 1.0)
  100 CONTINUE
      ZFitPar(1) = (1.7 * NTracks) / NZBins        ! Peak height
      ZFitPar(2) = (MaxZHist + MinZHist) / 2       ! Mean
      ZFitPar(3) = 24                              ! Sigma, in centimeters
      CALL HFITHN(ZHistID, 'G', 'W', 3, ZFitPar,,,, Sigmas, Chisq)
      CALL HDELET(ZHistID)
      ZCenter = ZFitPar(2)
      FH = FillHists
      IF(FH) THEN
        CALL DHDIR(' ','//PAWC/VTX',Err,' ')
        FH = Err .EQ. 0
        IF(FH) THEN
C
C ****  Book the impact-parameter, x and y histograms
C
          IF(HEXIST(1501)) THEN                 !  I booked those histograms
            CALL HDELET(1501)
            CALL HDELET(1511)
            CALL HDELET(1502)
            CALL HDELET(1512)
            CALL HDELET(1503)
            CALL HDELET(1513)
          ENDIF
          CALL HBOOK1(1501, 'X-Y Impact Parameter, unweighted',
     &                NImpBins, MinImpHist, MaxImpHist, 0.0)
          CALL HBOOK1(1511, 'X-Y Impact Parameter, weighted',
     &                NImpBins, MinImpHist, MaxImpHist, 0.0)
          CALL HBOOK1(1502, 'X of Beam, unweighted', NXBins, MinXHist,
     &                MaxXHist, 0.0)
          CALL HBOOK1(1512, 'X of Beam, weighted', NXBins, MinXHist,
     &                MaxXHist, 0.0)
          CALL HBOOK1(1503, 'Y of Beam, unweighted', NYBins, MinYHist,
     &                MaxYHist, 0.0)
          CALL HBOOK1(1513, 'Y of Beam, weighted', NYBins, MinYHist,
     &                MaxYHist, 0.0)
        ELSE
          CALL ERRMSG('DHDIR','VTX_FIND_BEAM',
     &                'Directory not found: //PAWC/VTX','W')
        ENDIF
      ENDIF
      NUsed = NTracks
      DO 150 I = 1, NTracks
        TrackData(I).Flag = ABS(TrackData(I).VertexZ - ZCenter) .LT.
     &                      ZCut
        IF(.NOT. TrackData(I).Flag) NUsed = NUsed - 1
  150 CONTINUE
      IF(NUsed .EQ. 0) GOTO 1000
      CALL MNINIT(5, 99, 99)
      CALL MNSETI(' Beam center: X0, Y0, dXdZ, dYdZ')
      DO 300 Iter = 1, NIter
        CALL MNPARM(1,'X0',DBLE(BeamX(1)),DBLE(BeamX(2)),0.0,0.0,Err)
        Err = ABS(Err)
        CALL MNPARM(2,'Y0',DBLE(BeamY(1)),DBLE(BeamY(2)),0.0,0.0,I)
        Err = Err + ABS(I)
        CALL MNPARM(3,'dXdZ',DBLE(dXdZ(1)),DBLE(dXdZ(2)),0.0,0.0,I)
        Err = Err + ABS(I)
        CALL MNPARM(4,'dYdZ',DBLE(dYdZ(1)),DBLE(dYdZ(2)),0.0,0.0,I)
        Err = Err + ABS(I)
        IF(Err .NE. 0) THEN
          CALL ERRMSG('MNPARM', 'VTX_FIND_BEAM',
     &      'MINUIT initialization failed', 'W')
          GOTO 1000
        ENDIF
        CALL MNEXCM(BeamFit, 'MINIMIZE', Zero, 0, Err, 0)
        IF(Err .NE. 0) THEN
          CALL ERRMSG('MINIMIZE', 'VTX_FIND_BEAM', 'Fit failed', 'W')
          GOTO 1000
        ENDIF
        CALL MNPOUT(1, Dummy, X0, XErr, LB, UB, I)
        CALL MNPOUT(2, Dummy, Y0, YErr, LB, UB, I)
        CALL MNPOUT(3, Dummy, XSlope, XSlpErr, LB, UB, I)
        CALL MNPOUT(4, Dummy, YSlope, YSlpErr, LB, UB, I)
        DO 200 I = 1, NTracks
          IF((TrackData(I).Flag .AND. (Iter .LT. NIter))
     &  .OR. ((Iter .EQ. NIter) .AND. FH)) THEN
            Xf = X0 + (TrackData(I).VertexZ - ZCenter) * XSlope
            Yf = Y0 + (TrackData(I).VertexZ - ZCenter) * YSlope
            DelX = TrackData(I).Xc - Xf
            DelY = TrackData(I).Yc - Yf
            CosPhi = COS(TrackData(I).Phi)
            SinPhi = SIN(TrackData(I).Phi)
            b = DelX * SinPhi - DelY * CosPhi
            Delb = (DelX * CosPhi + DelY * SinPhi) * TrackData(I).PhiErr
            IF(TrackData(I).Flag .AND. (Iter .LT. NIter)) THEN
              IF((ABS(b) .GT. ImpactCut(Iter)) .OR.
     &        (Delb .GT. ImpErrCut)) THEN
C
C ****  Make cuts for the next iteration
C
                TrackData(I).Flag = .FALSE.
                NUsed = NUsed - 1
              ENDIF
            ELSE
C
C ****  Fill the Impact Parameter and beam-position histograms
C
              CALL HFILL(1511, b, 0.0, 1/Delb**2)
              CALL HFILL(1501, b, 0.0, 1.0)
              W = (SinPhi ** 2 / ((TrackData(I).Yc - Yf) *
     &                            TrackData(I).PhiErr)) ** 2
              CALL HFILL(1512, SNGL(Xf + b / SinPhi), 0.0, W)
              CALL HFILL(1502, SNGL(Xf + b / SinPhi), 0.0, SinPhi ** 2)
              W = (CosPhi ** 2 / ((Xf - TrackData(I).Xc) *
     &                            TrackData(I).PhiErr)) ** 2
              CALL HFILL(1513, SNGL(Yf - b / CosPhi), 0.0, W)
              CALL HFILL(1503, SNGL(Yf - b / CosPhi), 0.0, CosPhi ** 2)
            ENDIF     !  ELSE
          ENDIF       !  (TrackData(I).Flag .AND. (Iter .LT. NIter)) .OR. etc
  200   CONTINUE      !  I = 1, NTracks
        IF(NUsed .EQ. 0) GOTO 1000
  300 CONTINUE        !  Iter = 1, NIter
      Z0 = ZCenter
      Params(1) = X0 - XSlope * ZCenter
      Params(2) = Y0 - YSlope * ZCenter
      Params(3) = XSlope
      Params(4) = YSlope
      Errs(1) = XErr
      Errs(2) = YErr
      Errs(3) = XSlpErr
      Errs(4) = YSlpErr
      Chi_2 = Chi2
      VTX_FIND_BEAM = .TRUE.
      RETURN
 1000 VTX_FIND_BEAM = .FALSE.
      END
