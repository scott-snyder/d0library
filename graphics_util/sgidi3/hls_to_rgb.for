      SUBROUTINE HLS_TO_RGB(HUE,LIGHT,SAT,R,G,B)
      REAL LIGHT
C  CONVERT HUE-LIGHTNESS-SATURATION TO RED-GREEN-BLUE
C  0 DEG=RED, 120 DEG=GREEN, 240 DEG=BLUE
      DIMENSION ANG(3),V(3),REF(3)
      DATA REF/0.,120.,240./
C
      AL2=2.*LIGHT
      ALU=AL2-1.
      DO 10 I=1,3
        V(I)=0.001
C  PUT EACH COLOR INTO IT'S OWN REFERENCE FRAME
        ANG(I)=HUE-REF(I)
        IF(ANG(I).LT.0.) ANG(I)=ANG(I)+360.
C  FOLD INTO 180 DEGREES
        IF(ANG(I).GT.180.) ANG(I)=360.-ANG(I)
C  SET UP VECTORS FOR LIGHTNESS=.5, SAT=1.
        IF(ANG(I).LE.60.) THEN
          V(I)=1.
        ELSEIF (ANG(I).GT.120.) THEN
          V(I)=0.
        ELSE
          V(I)=(120.-ANG(I))/60.        ! INTERPOLATE 60-120
        ENDIF
C  INTERPOLATE THE LIGHTNESS ON THE SAT=1. SURFACE
        IF(LIGHT.LE..5) THEN
          V(I)=AL2*V(I)
        ELSE
          V(I)=(1.-V(I))*ALU+V(I)
        ENDIF
C  INTERPOLATE THE SATURATION TO V=LIGHT AT SAT=0.
        V(I)=LIGHT+(V(I)-LIGHT)*SAT
   10 CONTINUE
      R=V(1)
      G=V(2)
      B=V(3)
      RETURN
      END
