      SUBROUTINE CLUSTRK_MATCH(CLUS_PAR,TRK_PAR,DTRK,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Computes the match between Cluster centroid
C-                         and CD tracks.
C-
C-   Inputs  :  CLUS_PAR(1) [R] = X of cluster centroid
C-              CLUS_PAR(2) [R] = Y of cluster centroid
C-              CLUS_PAR(3) [R] = Z of cluster centroid
C-              TRK_PAR(1)  [R] = phi of CD track 
C-              TRK_PAR(2)  [R] = theta of CD track
C-              TRK_PAR(3)  [R] = X of point on a track
C-              TRK_PAR(4)  [R] = Y of point on a track
C-              TRK_PAR(5)  [R] = Z of point on a track
C-              
C-   Outputs :  DTRK(1)  [R]  = RdPHI (cm) between shower center and track
C-              DTRK(2)  [R]  = dZ    (cm) between shower center and track CC
C-                            = dR    (cm) between shower center and track EC
C-              DTRK(3)  [R]  = dPHI  radians between shower center and track 
C-              DTRK(4)  [R]  = dETA  between shower center and track
C-              IER      [I]  = error code = 0 if all OK
C-   Controls:
C-
C-   Created   8-DEC-1992   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      REAL    CLUS_PAR(3),TRK_PAR(5),DTRK(4)
      INTEGER IER
      REAL    XCLUS, YCLUS, ZCLUS, XPOINT, YPOINT, ZPOINT, RPOINT
      REAL    PHITRK, THETATRK, PHICEL, THETA, PHI
      REAL    A, B, R, X, Y, Z, DISC
      REAL    REXT, DELTAR, DPHI, RDPHI, DZ, DR, DETA, DELTAY,DELTAX
C----------------------------------------------------------------------
      XCLUS = CLUS_PAR(1)
      YCLUS = CLUS_PAR(2)
      ZCLUS = CLUS_PAR(3)
      PHITRK = TRK_PAR(1)
      THETATRK = TRK_PAR(2)
      XPOINT = TRK_PAR(3)
      YPOINT = TRK_PAR(4)
      ZPOINT = TRK_PAR(5)
      DTRK(1) = 999.
      DTRK(2) = 999.
      IF (ABS(ZCLUS).LT.150) THEN
C
C ****  Central  Calorimeter and CD Track Match parameters
C
        B = TAN(PHITRK)
        A = YPOINT-XPOINT*B
        R = SQRT(XCLUS**2+YCLUS**2)
        DISC = R**2*(1.+B**2)-A**2
        IF(DISC.GT.0)THEN
          IF (ABS(PHITRK-PI).LT.PI/2.) THEN
            X = (-A*B-SQRT(DISC))/(1+B**2)
          ELSE
            X = (-A*B+SQRT(DISC))/(1+B**2)
          ENDIF
          Y = A+B*X
          PHI = ATAN2(Y,X)
          PHICEL = ATAN2(YCLUS,XCLUS)
          DPHI   = PHICEL-PHI
          IF(DPHI.GT.PI) DPHI = DPHI-TWOPI
          IF(DPHI.LT.-PI)DPHI = DPHI+TWOPI
          RDPHI = R*DPHI
          IF (TAN(THETATRK).NE.0) THEN
            DZ = SQRT((X-XPOINT)**2+(Y-YPOINT)**2)/TAN(THETATRK)
            Z  = ZPOINT+DZ
            DZ = ZCLUS-Z
            IF (Z.EQ.0) Z = Z + 0.0000001
            IF (ZCLUS.EQ.0) ZCLUS = ZCLUS + 0.0000001
            DETA = - ALOG(TAN(ATAN(ABS(R/ZCLUS))/2.)) + 
     &               ALOG(TAN(ATAN(ABS(R/Z))/2.))
          ENDIF
          DTRK(1) = RDPHI
          DTRK(2) = DZ
          DTRK(3) = DPHI
          DTRK(4) = DETA
        ELSE
          IER = -1
          GOTO 999
        ENDIF
      ELSE
C
C ****  EndCap Calorimeter and CD Track Match parameters
C
        r=sqrt(xclus**2+yclus**2)
        deltaR=abs((zclus-zpoint)*tan(thetatrk))
        deltax=deltaR*cos(phitrk)
        deltay=deltaR*sin(phitrk)
        x=xpoint+deltax
        y=ypoint+deltay
        rext=sqrt(x**2+y**2)
        dr=r-rext
        dphi=atan2(yclus,xclus)-atan2(y,x)
        if(dphi.lt.-pi)dphi=dphi+twopi
        if(dphi.gt.pi)dphi=dphi-twopi
        rdphi=r*dphi
        IF (ZCLUS.EQ.0) ZCLUS = ZCLUS + 0.0000001
        DETA   = - ALOG(TAN(ATAN(ABS(R/ZCLUS))/2.)) + 
     &             ALOG(TAN(ATAN(ABS(REXT/ZCLUS))/2.))
        DTRK(1) = RDPHI
        DTRK(2) = DR
        DTRK(3) = DPHI
        DTRK(4) = DETA
      ENDIF
      IER=0
  999 RETURN
      END
