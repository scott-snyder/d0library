      SUBROUTINE CLUSTER_MOMENTS(LCACL,MOMENTS,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to culculate some parameters related with shape
C-    of the EM cluster
C-
C-   Inputs  : LCACL
C-   Outputs : MOMENTS
C-   Controls: IER  0 - OK ,
C-              -3 no CACL bank,
C-              -2 no CASH bank,
C-              -1 no ZTRK or ZFIT bank,
C-
C-   Created  29-SEP-1993   Alexandre Peryshkin
C-
C-      MOMENTS(1) = Average PHI position
C-      MOMENTS(2) = RMS(sigma) in PHI direction ( SQRT(PHI moment power 2))
C-      MOMENTS(3)= Average ETA position
C-      MOMENTS(4)= RMS(sigma)  in ETA direction ( SQRT(ETA moment power 2))
C-      MOMENTS(5)= Average longitudinal position (in radiation lengths)
C-      MOMENTS(6)= RMS(sigma) in longitudinal direction (in radiation lengths)
C-      MOMENTS(7) = SQRT Normalized sum of squares of distances between cell
C-      and track weighted by cell energy ( SQRT(3D moment power 2))
C-      MOMENTS(8) = 1/SQRT(Phi moment power -2)
C-      MOMENTS(9) = 1/SQRT(ETA moment power -2)
C-
C----------------------------------------------------------------------
C-        FOR Central Calorimeter
C-
C-  DEPTH(25) mean depth each layer in X0 (rad.units) layers thiknesses are
C-  Layer number          I    II   III  IV   V     VI    VII      VIII
C-  Layer number          1    2    3-6  7    11    12    13    14 15
C-  Layer thiknesses CC   1.4  2.0  6.9  9.8  40.4  30.3  24.6  0  30.5
C- a half of Layer thikn. 0.7  1.0  3.45 4.9  20.2  15.15 12.3  0  15.25
C-  Layer integral before lay. 1.4  3.4 10.3  20.1  60.5  90.8  - 115.4
C-  midle layer position  0.7  2.4 6.85 15.2  40.3  75.65 103.1   130.65
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LCACL,LCASH,LZTRK,LZFIT,IER,OK,I,J
      INTEGER NCELL,ADRCELL,ILAYER,IPHI,IETA,NCLAYER
      INTEGER LVERT,GZVERH,LVERH
      CHARACTER*40 STRING
      BYTE BYTES(4)
      EQUIVALENCE (ADRCELL,BYTES)
      REAL DEPTH(25)
      REAL CGVERT(3),NVECT(3),RCENT(3),DCL,X, Y, Z,ECELL,SQRT_ECELL
      REAL CGVERTV(3),NVECTV(3),DCLV,V,ZVERTEX
      REAL ZERO_M,SZERO_M,TRMOMENT2,TRMOMENT2S,PHIMOMENT1,
     &  VXMOMENT2,VXMOMENT2S,
     &  PHIMOMENT2,PHIMOMENT3,PHIMOMENT1S,PHIMOMENT2S,PHIMOMENT3S,
     &  ETAMOMENT1,ETAMOMENT2,ETAMOMENT3,ETAMOMENT1S,ETAMOMENT2S,
     &  ETAMOMENT3S,LONGMOMENT1,LONGMOMENT2,LONGMOMENT3,LONGMOMENT1S,
     &  LONGMOMENT2S,LONGMOMENT3S
      REAL NTRMOMENT2,NTRMOMENT2S,NPHIMOMENT1,NPHIMOMENT2,NPHIMOMENT3,
     &  NVXMOMENT2,NVXMOMENT2S,
     &  NPHIMOMENT1S,NPHIMOMENT2S,NPHIMOMENT3S,NETAMOMENT1,NETAMOMENT2,
     &  NETAMOMENT3,NETAMOMENT1S,NETAMOMENT2S,NETAMOMENT3S,NLONGMOMENT1,
     &  NLONGMOMENT2,NLONGMOMENT3,NLONGMOMENT1S,NLONGMOMENT2S,
     &  NLONGMOMENT3S
      REAL L1ES,L1POS(3),L2ES,L2POS(3),L3ES,L3POS(3),L4ES,L4POS(3),
     &  L5ES,L5POS(3),DCL1,DCL2,DCL3,DCL4,DCL5,DCL_CLUSTER
      REAL AR,PHIMOMENT2M,ETAMOMENT2M,DPHI,PI/3.1415926/
      REAL XYZ(4),PHI,ETA,THETA,CCLIM,SCALE
      REAL L1E,L2E,L3E,L4E,L5E,PHI_SHIFT,PHI_SH
      REAL MOMENTS(9)
C..................................................................
      DATA DEPTH/ 0.7,2.4 ,6.85,6.85,6.85,6.85,15.2,0.,0.,0.
     &  ,40.3, 75.65,103.1,130.65,160.65 ,0.,0.,0.,0.,0.
     &  ,0.,0.,0.,0.,0./
C----------------------------------------------------------------------
      DO I =1,9
        MOMENTS(I) = 99999.
      ENDDO

      IER = 0
C pointed to the claster LCACL input
      IF (LCACL .LE. 0) THEN
        IER = -3
        GOTO 999
      ENDIF
C pointed to the claster LZTRK LZFIT
      LZTRK = LQ(LCACL -6)
      IF( LZTRK .GT. 0) LZFIT = LQ(LZTRK -1)
      IF ( LZTRK .LE. 0 .OR. LZFIT.LE. 0 ) THEN
        IER = -1
CCCC        GOTO 999
      ENDIF
C get first vertex
      LVERH=GZVERH()
      IF (LVERH.GT.0) THEN
        LVERT=LQ(LVERH-1)
        IF (LVERT.GT.0) THEN
          CGVERTV(1) = Q(LVERT+3)
          CGVERTV(2) = Q(LVERT+4)
          CGVERTV(3) = Q(LVERT+5)
          ZVERTEX    = Q(LVERT+5)
        END IF
      ELSE
        CGVERTV(1) = 0
        CGVERTV(2) = 0
        CGVERTV(3) = 0
      END IF

C
      RCENT(1) = Q(LCACL+14 )
      RCENT(2) = Q(LCACL+15 )
      RCENT(3) = Q(LCACL+16 )
      V=SQRT((RCENT(1)-CGVERTV(1))**2+(RCENT(2)-CGVERTV(2))**2+(RCENT(3)
     &  -CGVERTV(3))**2)
      IF (V.LT.0) V=1.
      NVECTV(1) = (RCENT(1)-CGVERTV(1))/V
      NVECTV(2) = (RCENT(2)-CGVERTV(2))/V
      NVECTV(3) = (RCENT(3)-CGVERTV(3))/V
C
c
      CALL CLOSE_DIST(RCENT,CGVERTV,NVECTV,DCL)
C
      IF (IER.EQ.0)THEN
C track direction NVECT(3) and CG point CGVERT(3)
        CGVERT(1) = Q(LZFIT + 11)
        CGVERT(2) = Q(LZFIT + 12)
        CGVERT(3) = Q(LZFIT + 15)
        NVECT(1) = Q(LZFIT + 20)
        NVECT(2) = Q(LZFIT + 22)
        NVECT(3) = Q(LZFIT + 24)
C
        DPHI = ABS(Q(LCACL+12)-Q(LZFIT+10))
        IF (DPHI .GT. PI) DPHI=2*PI-DPHI
      ENDIF
C find  bank CASH
      LCASH = LQ(LCACL -2)
      IF (LCASH .LE. 0) THEN
        IER = -2
        GOTO 999
      ENDIF
      NCELL = IQ(LCASH +2)
C
      PHI_SHIFT = 0
      ZERO_M= 0
      SZERO_M= 0
      TRMOMENT2= 0
      TRMOMENT2S= 0
      VXMOMENT2= 0
      VXMOMENT2S= 0
      PHIMOMENT1= 0
      PHIMOMENT2= 0
      PHIMOMENT3= 0
      PHIMOMENT1S= 0
      PHIMOMENT2S= 0
      PHIMOMENT3S= 0
      ETAMOMENT1= 0
      ETAMOMENT2= 0
      ETAMOMENT3= 0
      ETAMOMENT1S= 0
      ETAMOMENT2S= 0
      ETAMOMENT3S= 0
      LONGMOMENT1= 0
      LONGMOMENT2= 0
      LONGMOMENT3= 0
      LONGMOMENT1S= 0
      LONGMOMENT2S= 0
      LONGMOMENT3S= 0
      L1E = 0
      L2E = 0
      L3E = 0
      L4E = 0
      L5E = 0
      L1ES = 0
      L2ES = 0
      L3ES = 0
      L4ES = 0
      L5ES = 0
      DO J=1,3
        L1POS(J) = 0
        L2POS(J) = 0
        L3POS(J) = 0
        L4POS(J) = 0
        L5POS(J) = 0
      ENDDO
C
C negative ETA, PHI moments
C
      PHIMOMENT2M = 0
      ETAMOMENT2M = 0
C
C distance of the closest approch
      RCENT(1) = Q(LCACL+14 )
      RCENT(2) = Q(LCACL+15 )
      RCENT(3) = Q(LCACL+16 )
      CALL CLOSE_DIST(RCENT,CGVERT,NVECT,DCL_CLUSTER)
C
      DO I=1,NCELL
C fetch cells information
        ADRCELL = IQ(LCASH +2*I+1)
        ECELL = Q(LCASH +2*I+2)
        IF( ECELL .GT. 0) THEN
          ILAYER = BYTES(2)
          IPHI   = BYTES(3)
          IETA   = BYTES(4)
          SQRT_ECELL = SQRT(ECELL)
C
          CALL CELXYZ(IETA,IPHI,ILAYER,X,Y,Z,OK)
          IF(OK.NE.0)THEN
            CALL ERRMSG('CALORIMETER','CACLFL',
     &          ' CELXYZ ERROR ','W')
    1       FORMAT(' ','ETAI,PHII,ILYR',3I5)
            WRITE(STRING,1) ETA,PHI,ILAYER
            CALL ERRMSG('CALORIMETER','DIAGNOSTIC',STRING,'W')
          ENDIF
C
C ****  ETA, PHI for cell
C
          XYZ(1) = X
          XYZ(2) = Y
          XYZ(3) = Z
          XYZ(4) = SQRT(X*X+Y*Y+Z*Z)
          CALL ETOETA(XYZ,PHI,THETA,ETA)
C shift phi out of 0 or 2*pi:
          IF(I.EQ.1) PHI_SHIFT = PI - PHI
          PHI_SH = PHI + PHI_SHIFT
C
C calculate cell centroid
          IF (OK.EQ.0.AND.IER.GE.-1) THEN
            RCENT(1) = X
            RCENT(2) = Y
            RCENT(3) = Z
C calculate distance cell by cell to track
            CALL CLOSE_DIST(RCENT,CGVERT,NVECT,DCL)
C calculate distance cell by cell to line cluster-vertex
            CALL CLOSE_DIST(RCENT,CGVERTV,NVECTV,DCLV)
C
C accomulate layer claster energy and position
            IF (ILAYER .EQ. 1) THEN
              DO J=1,3
                L1POS(J) = L1POS(J) + SQRT_ECELL*RCENT(J)
              ENDDO
              L1E = L1E +ECELL
              L1ES = L1ES +SQRT_ECELL
            ENDIF
C
            IF (ILAYER .EQ. 2) THEN
              DO J=1,3
                L2POS(J) = L2POS(J) + SQRT_ECELL*RCENT(J)
              ENDDO
              L2E = L2E + ECELL
              L2ES = L2ES +SQRT_ECELL
            ENDIF
C
            IF (ILAYER.EQ.3.OR.ILAYER.EQ.4.OR.ILAYER.EQ.5.OR.ILAYER
     &        .EQ.6)THEN
              DO J=1,3
                L3POS(J) = L3POS(J) + SQRT_ECELL*RCENT(J)
              ENDDO
              L3E = L3E + ECELL
              L3ES = L3ES +SQRT_ECELL
            ENDIF
C
            IF (ILAYER .EQ. 7) THEN
              DO J=1,3
                L4POS(J) = L4POS(J) + SQRT_ECELL*RCENT(J)
              ENDDO
              L4E = L4E + ECELL
              L4ES = L4ES +SQRT_ECELL
            ENDIF
C
            IF (ILAYER .EQ. 11) THEN
              DO J=1,3
                L5POS(J) = L5POS(J) + SQRT_ECELL*RCENT(J)
              ENDDO
              L5E = L5E + ECELL
              L5ES = L5ES +SQRT_ECELL
            ENDIF
          ENDIF
C
C    momenta culculation
          ZERO_M = ZERO_M + ECELL
          SZERO_M = SZERO_M + SQRT_ECELL
C
          IF (IER.EQ.0)THEN
            TRMOMENT2 = TRMOMENT2 + ECELL*DCL**2
            TRMOMENT2S = TRMOMENT2S + SQRT_ECELL*DCL**2
          ENDIF
C
          VXMOMENT2 = VXMOMENT2 + ECELL*DCLV**2
          VXMOMENT2S = VXMOMENT2S + SQRT_ECELL*DCLV**2
C
          PHIMOMENT1 = PHIMOMENT1 + ECELL*PHI_SH
          PHIMOMENT2 = PHIMOMENT2 + ECELL*PHI_SH**2
          PHIMOMENT3 = PHIMOMENT3 + ECELL*PHI_SH**3
          PHIMOMENT1S = PHIMOMENT1S + SQRT_ECELL*PHI_SH
          PHIMOMENT2S = PHIMOMENT2S + SQRT_ECELL*PHI_SH**2
          PHIMOMENT3S = PHIMOMENT3S + SQRT_ECELL*PHI_SH**3
C
          ETAMOMENT1 = ETAMOMENT1 + ECELL*ETA
          ETAMOMENT2 = ETAMOMENT2 + ECELL*ETA**2
          ETAMOMENT3 = ETAMOMENT3 + ECELL*ETA**3
          ETAMOMENT1S = ETAMOMENT1S + SQRT_ECELL*ETA
          ETAMOMENT2S = ETAMOMENT2S + SQRT_ECELL*ETA**2
          ETAMOMENT3S = ETAMOMENT3S + SQRT_ECELL*ETA**3
C
          LONGMOMENT1 = LONGMOMENT1 + ECELL*DEPTH(ILAYER)
          LONGMOMENT2 = LONGMOMENT2 + ECELL*DEPTH(ILAYER)**2
          LONGMOMENT3 = LONGMOMENT3 + ECELL*DEPTH(ILAYER)**3
          LONGMOMENT1S = LONGMOMENT1S + SQRT_ECELL*DEPTH(ILAYER)
          LONGMOMENT2S = LONGMOMENT2S + SQRT_ECELL*DEPTH(ILAYER)**2
          LONGMOMENT3S = LONGMOMENT3S + SQRT_ECELL*DEPTH(ILAYER)**3
C
        ENDIF
      ENDDO
C
      IF (IER.EQ.0)THEN
        NTRMOMENT2 = TRMOMENT2 /ZERO_M
        NTRMOMENT2S = TRMOMENT2S /SZERO_M
      ENDIF
C
      NVXMOMENT2 = VXMOMENT2 /ZERO_M
      NVXMOMENT2S = VXMOMENT2S /SZERO_M
C
      NPHIMOMENT1 = PHIMOMENT1 /ZERO_M
      NPHIMOMENT2 = PHIMOMENT2 /ZERO_M
      NPHIMOMENT3 = PHIMOMENT3 /ZERO_M
      NPHIMOMENT1S = PHIMOMENT1S /SZERO_M
      NPHIMOMENT2S = PHIMOMENT2S /SZERO_M
      NPHIMOMENT3S = PHIMOMENT3S /SZERO_M
C
      NETAMOMENT1 = ETAMOMENT1 /ZERO_M
      NETAMOMENT2 = ETAMOMENT2 /ZERO_M
      NETAMOMENT3 = ETAMOMENT3 /ZERO_M
      NETAMOMENT1S = ETAMOMENT1S /SZERO_M
      NETAMOMENT2S = ETAMOMENT2S /SZERO_M
      NETAMOMENT3S = ETAMOMENT3S /SZERO_M
C
      THETA = 2.0*ATAN(EXP(-NETAMOMENT1))
      CCLIM = 1.3
      IF(ABS(NETAMOMENT1).LT.CCLIM) THEN
        SCALE = SIN(THETA)
      ELSE
        SCALE = COS(THETA)
      END IF
      IF(SCALE .LE.1.0E-5) SCALE=1.
C
      NLONGMOMENT1 = LONGMOMENT1 /(ZERO_M*SCALE)
      NLONGMOMENT2 = LONGMOMENT2 /(ZERO_M*SCALE*SCALE)
      NLONGMOMENT3 = LONGMOMENT3 /(ZERO_M*SCALE*SCALE*SCALE)
      NLONGMOMENT1S = LONGMOMENT1S /SZERO_M
      NLONGMOMENT2S = LONGMOMENT2S /SZERO_M
      NLONGMOMENT3S = LONGMOMENT3S /SZERO_M
C
      IF( NTRMOMENT2 .LT.0) NTRMOMENT2 = 0
      MOMENTS(7) = SQRT(NTRMOMENT2)
C
      MOMENTS(1) = NPHIMOMENT1 - PHI_SHIFT
      IF ( MOMENTS(1) .LT. 0) MOMENTS(1) = MOMENTS(1)+PI
      MOMENTS(2) = NPHIMOMENT2 - NPHIMOMENT1**2
      IF( MOMENTS(2).LT.0) MOMENTS(2)=0
      MOMENTS(2)= SQRT(MOMENTS(2))
C
      MOMENTS(3)= NETAMOMENT1
      MOMENTS(4)= NETAMOMENT2 - NETAMOMENT1**2
      IF( MOMENTS(4).LT.0) MOMENTS(4)=0
      MOMENTS(4)= SQRT(MOMENTS(4))
C
      MOMENTS(5)= NLONGMOMENT1
      MOMENTS(6)= NLONGMOMENT2 - NLONGMOMENT1**2
      IF( MOMENTS(6).LT.0) MOMENTS(6)=0
      MOMENTS(6)= SQRT(MOMENTS(6))
C
      DO I=1,NCELL
C fetch cells information
        ADRCELL = IQ(LCASH +2*I+1)
        ECELL = Q(LCASH +2*I+2)
        IF( ECELL .GT. 0) THEN
          ILAYER = BYTES(2)
          IPHI   = BYTES(3)
          IETA   = BYTES(4)
C
          CALL CELXYZ(IETA,IPHI,ILAYER,X,Y,Z,OK)
          XYZ(1) = X
          XYZ(2) = Y
          XYZ(3) = Z
          XYZ(4) = SQRT(X*X+Y*Y+Z*Z)
          CALL ETOETA(XYZ,PHI,THETA,ETA)
C shift phi out of 0 or 2*pi:
          PHI_SH = IPHI + PHI_SHIFT
C
          AR = (PHI_SH-NPHIMOMENT1 )**2
          IF (AR .LT. 1.0E-6) AR = 1.0E-6
          PHIMOMENT2M = PHIMOMENT2M + ECELL/AR
          AR = (ETA-NETAMOMENT1)**2
          IF (AR .LT. 1.0E-6) AR = 1.0E-6
          ETAMOMENT2M = ETAMOMENT2M + ECELL/AR
        ENDIF
      ENDDO
C
      IF( ZERO_M .LT.0) ZERO_M =0
      IF( PHIMOMENT2M  .LE.1.0-6) PHIMOMENT2M=1.0E-6
      IF( ETAMOMENT2M  .LE.1.0-6) ETAMOMENT2M=1.0E-6
C

      MOMENTS(8) = SQRT(ZERO_M/PHIMOMENT2M )
      MOMENTS(9) = SQRT(ZERO_M/ETAMOMENT2M )
C
  999 RETURN
      END
