      SUBROUTINE ZFLZFT(LZFIT,HITX,HITY,HITZ,WR,WZ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : make a fit with all the hits on the ZTRK
C-                         and fill the bank ZFIT
C-
C-   Inputs  : LZFIT: the address of the bank ZFIT
C-             HITX, HITY and HITZ: arrays containning the (X,Y,Z) of
C-                                  each hit
C-             WR and WZ: the weight on the coordinates of each hit     
C-   Outputs : BANK ZFIT is filled
C-             If all WR(i) are zero, no fitting will be done
C-
C-   Created  17-MAR-1990   Qizhong Li-Demarteau
C-   Updated  16-AUG-1991   Qizhong Li-Demarteau  added protection when no 
C-                                                hits information available
C-   Updated   2-SEP-1991   Susan K. Blessing  Change the parameter NHIT
C-    to 85 (from 65) to allow for VTX-CDC-FDC tracks.  
C-    Change WIRE1 to WIREF, make it an integer, and use it in loops over 
C-    NHIT to cut down on the number of executions.  Introduce WIREL.
C-    WIREF is the first and WIREL the last entry in the HITX, HITY, HITZ, 
C-    WR and WZ arrays.
C-   Updated   2-APR-1992   Qizhong Li-Demarteau   added degree of freedom
C-   Updated  20-SEP-1992   Qizhong Li-Demarteau   recalculate the errors
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER LZFIT, I, NZ, NXY, NHIT
      PARAMETER( NHIT = 85 )
      INTEGER WIREF,WIREL
C
      REAL    HITX(NHIT), HITY(NHIT), HITZ(NHIT), WR(NHIT), WZ(NHIT)
      REAL    CHISQ(2), THETA, PHI, CPHI, SPHI, RESID(NHIT,2)
      REAL    S0, SX, SY, SX2, SY2, S1, SZ, SMS, SXY, NUM, DEN, SLEN
      REAL    TANT, RR, RRR
      REAL    STHETA, CTHETA, SINPHI, COSPHI
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (LZFIT .LE. 0) GOTO 999
C
C ****  First, fit in the X-Y plane
C
      S0 = 0.
      SX = 0.
      SY = 0.
      WIREF = 0
      NXY = 0
      DO 10 I = 1, NHIT
        IF (WR(I) .GT. 0.0) THEN
          IF (WIREF .EQ. 0) WIREF = I
          NXY = NXY + 1
          S0 = S0 + WR(I)
          SX = SX + HITX(I) * WR(I)
          SY = SY + HITY(I) * WR(I)
          WIREL = I
        ENDIF
   10 CONTINUE
      IF (S0 .LE. 0) GOTO 999
      IQ(LZFIT+6) = NXY
      IF (NXY .GT. 2) IQ(LZFIT+28) = NXY - 2
      Q(LZFIT+11) = SX/S0
      Q(LZFIT+12) = SY/S0
      Q(LZFIT+17) = 1./SQRT(S0)
      SX2 = 0.
      SXY = 0.
      SY2 = 0.
      DO 20 I = WIREF, WIREL
        IF (WR(I) .LE. 0.0) GOTO 20
        HITX(I) = HITX(I) - Q(LZFIT+11)      ! NOW, RELATIVE X's and y'S
        HITY(I) = HITY(I) - Q(LZFIT+12) 
        SX2 = SX2 + HITX(I) * HITX(I) * WR(I)
        SXY = SXY + HITX(I) * HITY(I) * WR(I)
        SY2 = SY2 + HITY(I) * HITY(I) * WR(I)
   20 CONTINUE
      PHI = .5 * ATAN2(2.*SXY, SX2-SY2)
C
C ****  Phi has to be between 0 and 2*pi. The previous value is between
C ****  -pi/2 and +pi/2. We choose the direction of the track by reference
C ****  to the first point, and choose X or Y as most significative:
C
C ****  If X, then: if x1>x0, then: phi = phi+pi.
C ****                        else:  if phi<0, phi = phi + pi
C ****        else: if phi<0 : phi = phi + pi
C ****              if y1>y0 : phi = phi + pi
C
C ****  Like this, phi is always between 0 and 2*pi, in the track direction.
C
      IF (SX2 .GT. SY2) THEN
        IF (HITX(WIREF) .GT. Q(LZFIT+11)) THEN
          PHI = PHI + PI
        ELSE
          IF (PHI .LT. 0) PHI = PHI + 2.*PI
        ENDIF
      ELSE
        IF (PHI .LT. 0 ) PHI = PHI + PI
        IF (HITY(WIREF) .GT. Q(LZFIT+12)) PHI = PHI + PI
      ENDIF
      Q(LZFIT+10) = PHI
      Q(LZFIT+16) = 1./SQRT(SX2+SY2)
      CPHI = COS(PHI)
      SPHI = SIN(PHI)
      CHISQ(1) = 0.
      DO 30 I = WIREF, WIREL
        IF (WR(I) .GT. 0.) THEN
          RESID(I,1) = HITX(I)*SPHI - HITY(I)*CPHI
          CHISQ(1)   = CHISQ(1) + WR(I) * RESID(I,1)**2
        ELSE
          RESID(I,1) = 0.
        ENDIF
   30 CONTINUE
      IF (CHISQ(1) .GT. 9999.0) CHISQ(1) = 9999.0
      Q(LZFIT+8) = CHISQ(1)
C
C ****  Then compute Z0
C
      S1 = 0.
      SZ = 0.
      SMS = 0.
      NZ = 0
      Q(LZFIT+18) = 99.0
      Q(LZFIT+34) = 99.0
      DO 110 I= WIREF, WIREL
        IF (WZ(I) .LE. 0.0) GOTO 110
        NZ = NZ + 1
        S1 = S1 + WZ(I)
        SZ = SZ + HITZ(I) * WZ(I)
        SMS = SMS + (HITX(I)*CPHI + HITY(I)*SPHI) * WZ(I)
  110 CONTINUE
      IQ(LZFIT+7) = NZ
      IF (NZ .GT. 2) IQ(LZFIT+29) = NZ - 2
      IF ( S1 .LE. 0.) THEN
        Q(LZFIT+15) = 0.
        Q(LZFIT+19) = 99.
      ELSE
        Q(LZFIT+15) = SZ/S1
        Q(LZFIT+19) = 1./SQRT( S1 )
      ENDIF
C
C ****  Now, compute theta, we have to compute various sums
C
      NUM = 0.
      DEN = 0.
      DO 130 I = WIREF, WIREL
        IF (WZ(I) .LE. 0.0) GOTO 130
        SLEN = HITX(I) * CPHI + HITY(I) * SPHI - SMS/S1
        NUM = NUM + SLEN * SLEN * WZ(I)
        DEN = DEN + SLEN * (HITZ(I)-Q(LZFIT+15)) * WZ(I)
  130 CONTINUE
C
C ****  protections, will never happen ??
C
      IF (NUM .LE. 0. .AND. DEN .EQ. 0.0) THEN
        THETA = 0.
        TANT      = 0.
        Q(LZFIT+18) = 99.
      ELSE
C ---------------- NUM is >0 => theta is between 0 and pi
        THETA = ATAN2(NUM, DEN) 
        TANT      = DEN / NUM
        IF(DEN .NE. 0.) Q(LZFIT+18) = COS(THETA)**2 / SQRT(ABS(DEN))
      ENDIF
      Q(LZFIT+13) = THETA
C
C ****  Compute Z0 at the (X0, Y0) point and its errors
C
      IF (S1 .GT. 0) Q(LZFIT+15) = (SZ - SMS * TANT) / S1
      STHETA = SIN(THETA)
      IF (STHETA .NE. 0.0)
     & Q(LZFIT+34) = Q(LZFIT+18) * SMS / (S1 * STHETA**2)
      IF (STHETA .NE. 0.0 .AND. Q(LZFIT+19) .LT. 99.0)
     & Q(LZFIT+19) = SQRT(Q(LZFIT+19)**2 + Q(LZFIT+34)**2)
C
      CHISQ(2) = 0.
      DO 40 I = WIREF, WIREL
        RESID(I,2) = 999.
        IF (WR(I) .LE. 0.) GOTO 40
        IF (WZ(I) .GT. 0.) THEN
          RESID(I,2) = HITZ(I) - Q(LZFIT+15) - 
     &                 TANT * (HITX(I) * CPHI + HITY(I) * SPHI)
          CHISQ(2) = CHISQ(2) + RESID(I,2)**2 * WZ(I)
        ENDIF
   40 CONTINUE
      IF (CHISQ(2) .GT. 9999.0) CHISQ(2) = 9999.0
      Q(LZFIT+9) = CHISQ(2)
C
C  now calculate the direction cosines
C
      RR = SQRT(Q(LZFIT+11)**2 + Q(LZFIT+12)**2)
      Q(LZFIT+14) = RR
      CTHETA = COS(THETA)
      SINPHI = SIN(PHI)
      COSPHI = COS(PHI)
      Q(LZFIT+20) = STHETA * COSPHI 
      Q(LZFIT+22) = STHETA * SINPHI 
      Q(LZFIT+24) = CTHETA
      Q(LZFIT+21) = SQRT( COSPHI**2 * CTHETA**2 * Q(LZFIT+18)**2 +
     &                    SINPHI**2 * STHETA**2 * Q(LZFIT+16)**2)
      Q(LZFIT+23) = SQRT( SINPHI**2 * CTHETA**2 * Q(LZFIT+18)**2 +
     &                    COSPHI**2 * STHETA**2 * Q(LZFIT+16)**2)
      Q(LZFIT+25) = STHETA * Q(LZFIT+18)
C
  999 RETURN
      END
