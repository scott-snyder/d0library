      SUBROUTINE TRD_CYLIND(R,XR,YR,ZR,XRP,YRP,ZRP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Intersection de la trace de cos. directeurs
C-                         UX,UY,UZ, avec le cylindre de rayon R 
C-                         (Longueur =+-90 cm)  dans le  sens positif 
C-
C-   Inputs  : UX,UY,UZ,X,Y,Z,R
C-   Outputs : XR,YR,ZR ( Coordonnees de l'intersection de la droite avec 
C-                        le cylindre cote centre de gravite de la trace)
C-             XRP,YRP,ZRP Coordonnees du deuxieme point d'intersection 
C-   Created  21-DEC-1990   Y.DUCROS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:isacan.INC'
      REAL R,DELTA,DIS,UDX,AK,K0,K1,UNORM,UPX,UPY,K
      REAL XR,YR,ZR,XRP,YRP,ZRP,UXY,XA,YA,ZA
C
      XR=1000.
      YR=1000.
      ZR=1000.
      XRP=1000.
      YRP=1000.
      ZRP=1000.
      UNORM=sqrt(UX1**2+UY1**2+UZ1**2)
C      WRITE(6,*)'CYLIND,PHITEL,TETTE, UX,UY,UZ=',PHITEL,TETTEL,UX,UY,UZ
      UX1=UX1/UNORM
      UY1=UY1/UNORM
      UZ1=UZ1/UNORM
      UXY=UX1*X1+UY1*Y1
      DELTA=(UXY)**2-(UX1**2+UY1**2)*((X1**2+Y1**2)-R**2)
      IF(DELTA.LT.0..OR.ABS(UZ1).GT.0.98) THEN
        GO TO 999
      END IF
      DELTA=SQRT(DELTA)
      AK=(-UXY+SIGN(DELTA,UXY))/(UX1**2+UY1**2)
      XR=UX1*AK+X1
      YR=UY1*AK+Y1
      ZR=UZ1*AK+Z1
      AK=( -UXY-SIGN(DELTA,UXY))/(UX1**2+UY1**2)
      XRP=UX1*AK+X1
      YRP=UY1*AK+Y1
      ZRP=UZ1*AK+Z1
      IF((XR*UX1+YR*UY1).LT.0.) THEN
        XA=XR
        YA=YR
        ZA=ZR
        XR=XRP
        YR=YRP
        ZR=ZRP
        XRP=XA
        YRP=YA
        ZRP=ZA
      END IF
C
C----------------------------------------------------------------------
  999 RETURN
      END
