C----------------------------------------------------------------------
      SUBROUTINE T0HITC(X0,Y0,Z0,PHI,THET,XP,YP,ZP,RP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the number of fired fiber using MC
C-                         tracks
C-
C-   Inputs       : XR,YR,PhiR        - fiber plane coords (common block)
C-                  X0,Y0,Z0,Phi,Thet - track coords
C-   Outputs      : XP,YP,ZP          - Intersection point coords
C-                  RP                - Distance to the reference point
C-   Controls     : None
C-
C-   Created  11-APR-1992   Gregory L. Landsberg
C-
C
C-------------------------------------------------------------
C    Algorithm is based on the following mathematics.
C
C    Fiber plane equations: X'(s) = XR + s*cos(PhiR)
C                           Y'(s) = YR + s*sin(PhiR)
C
C    Track equations:       X"(t) = X0 + t*cos(Phi)*sin(Thet)
C                           Y"(t) = Y0 + t*sin(Phi)*sin(Thet)
C                           Z"(t) = Z0 + t*cos(Thet)
C
C    Here is the formula for intersection point:
C
C    _        (Y0-YR) - Tg(PhiR)*(X0-XR)
C    t = ------------------------------------
C        (Tg(PhiR)-Tg(Phi)*cos(Phi)*sin(Thet)
C
C    _   (Y0-YR) - (X0*Tg(Phi)-XR*Tg(PhiR))
C    X = ----------------------------------
C                Tg(PhiR) - Tg(Phi)
C
C    _   (X0-XR) - (Y0/Tg(Phi)-YR/Tg(PhiR))
C    Y = ----------------------------------
C               1/Tg(PhiR) - 1/Tg(Phi)
C
C    _              (Y0-YR) - Tg(PhiR)*(X0-XR)
C    Z = Z0 + --------------------------------------
C             (Tg(PhiR) - Tg(PhI))*cos(Phi)*Tg(Thet)
C
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$INC:T0DPOS.INC'
      REAL          X0, Y0, Z0, PHI, THET, XP, YP, ZP, RP, TG0, TG, CTG
C
      XP = 0.
      YP = 0.
      ZP = 0.
      RP = 0.
C
      IF (COS(PHI).LE.0)  RETURN   ! Track should have positive cosine
      TG0   = TAN(PHI)
      IF ( TG0 .GE. 0. )  RETURN   ! Track should be in the IV quadrant
      IF ( THET .EQ. 0. ) RETURN   ! Track should not be parallel to Z
C
      TG  = TGR - TG0
      IF ( TG  .EQ. 0. )  RETURN   ! Track should not be || to fiber plane
      CTG = 1./TGR - 1./TG0
C
      XP = ((Y0 - YR) - (X0*TG0 - XR*TGR))/TG
      YP = ((X0 - XR) - (Y0/TG0 - YR/TGR))/(1/TGR - 1/TG0)
      ZP = Z0 + ((Y0 - YR) - TGR*(X0 - XR))/TG/COS(PHI)/TAN(THET)
C
      RP = DSQRT(DBLE(XP - XR)**2 + DBLE(YP - YR)**2)
      IF (YP .LT. YR) RP = -RP
C
      RETURN
      END
