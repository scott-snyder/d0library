      SUBROUTINE JCONVW(VX,VY,X,Y,Z)
C  CONVERT VIRTUAL WINDOW TO WORLD COORDINATES
      COMMON/SCRTRN/XSCALE,YSCALE,XSCALS,YSCALS
      INCLUDE 'D0$INC:DI3INC.INC'
C  FROM VIEWPORT TO VIEWSPACE
      XP=(VX-VXMIN)/XSCALE+UMIN
      YP=(VY-VYMIN)/YSCALE+VMIN
C  VIEWSPACE TO WORLD COORDS
C      X=XVW+XP*UVECX+YP*VVECX
C      Y=YVW+XP*UVECY+YP*VVECY
C      Z=ZVW+XP*UVECZ+YP*VVECZ
      X=XVW+XP
      Y=YVW+YP
      Z=0.
      END
