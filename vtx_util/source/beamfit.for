      SUBROUTINE BeamFit(N, Gradient, RetVal, Params, Flag)
C----------------------------------------------------------------------
C-
C-  The stuff involving FirstFit may seem strange, but it's there for a
C-  reason: you shouldn't have fit parameters in the weights, because
C-  MINUIT will then attempt to minimize the weights, which is wrong.
C-  So we allow it for the first iteration, but use the previous iter-
C-  ation's results from then on.  The name of the COMMON block is to
C-  insure that it doesn't conflict with anything else.
C-
C-   Created  21-MAR-1994   Justin R. Bendich, Ed Oltman 
C-
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:VTX_ACCUM_TRACKS.INC'
      INTEGER N, Flag
      DOUBLE PRECISION Gradient(*), RetVal, Params(*)
      INTEGER I, Iter
      REAL Chi2Fact
      DOUBLE PRECISION Xf, Yf, DelX, DelY, Numr, Chi2, XN, YN, DX, DY,
     &                 ZCenter, XSlope, YSlope, X0, Y0
      LOGICAL FirstFit
      COMMON /GBNWX7/ ZCenter, X0, Y0, XSlope, YSlope, Chi2, Chi2Fact,
     &                Iter
C
      FirstFit = Iter .EQ. 1
      Chi2 = 0D0
      DO 10 I = 1, NTracks
        IF(TrackData(I).Flag) THEN
          Xf = Params(1) + (TrackData(I).VertexZ - ZCenter) * Params(3)
          Yf = Params(2) + (TrackData(I).VertexZ - ZCenter) * Params(4)
          IF(FirstFit) THEN
            XN = Xf
            YN = Yf
          ELSE
            XN = X0 + (TrackData(I).VertexZ - ZCenter) * XSlope
            YN = Y0 + (TrackData(I).VertexZ - ZCenter) * YSlope
          ENDIF
          DelX = TrackData(I).Xc - Xf
          DelY = TrackData(I).Yc - Yf
          DX = TrackData(I).Xc - XN
          DY = TrackData(I).Yc - YN
          Numr = (DelX * SIN(TrackData(I).Phi) -
     &      DelY * COS(TrackData(I).Phi)) ** 2
          Chi2 = Chi2 + Numr /
     &      ((DX**2 + DY**2) * TrackData(I).PhiErr**2)
        ENDIF
   10 CONTINUE
      RetVal = Chi2 / Chi2Fact
      END
