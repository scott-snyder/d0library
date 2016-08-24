      SUBROUTINE J_TR2XY(X,Y,XP,YP)
C  DO THE VIEWING TRANSFORMATION ON COORDINATES
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
C  DO MODELLING TRANSFORMATION IF CALLED FOR
      IF(IMODEL.EQ.0) THEN
        XP=X
        YP=Y
      ELSE
C!!!ORDER???
        XP=X*MMODEL(1,1)+Y*MMODEL(2,1)+MMODEL(4,1)
        YP=X*MMODEL(1,2)+Y*MMODEL(2,2)+MMODEL(4,2)
      ENDIF
C  TRANSLATE TO ORIGIN OF UVN SYSTEM
      XTR=X-XVW
      YTR=Y-YVW
C  ROTATE TO UVN AXES
      XP=XTR*UVECX+YTR*UVECY
      YP=XTR*VVECX+YTR*VVECY
C  TRANSFORM TO VIEWPORT COORDINATES
      XP=(XP-UMIN)*XSCALE+VXMIN
      YP=(YP-VMIN)*YSCALE+VYMIN
C  IMAGE TRANSFORMATIONS
      IF(ISEGNM.GT.0) THEN
        IF(ITYCUR.GT.0) THEN
          IF(ITYCUR.LT.1) THEN
            DX=XP-PXC
            DY=YP-PYC
            XP=(DX*CSC-DY*SNC)+PXC
            YP=(DX*SNC+DY*CSC)+PYC
          ELSE
C!!!STICK IN ASSOCIATION STUFF HERE  (JLOCAT 7)
            XP=XP+TXC
            YP=YP+TYC
          ENDIF
        ENDIF
      ENDIF
C  TRANSFORM TO DEVICE COORDINATES LATER (SEE J_TRDEV)
      RETURN
      END