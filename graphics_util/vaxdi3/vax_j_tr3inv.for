      SUBROUTINE J_TR3INV(XS,YS,XW,YW,ZW)
C  DO THE VIEWING TRANSFORMATION ON COORDINATES
      COMMON/SCRTRN/XSCALE,YSCALE,XSCALS,YSCALS
      REAL*4 XS,YS,XP,YP,ZP,XW,YW,ZW
      INCLUDE 'D0$INC:DI3INC.INC'
C
C  FROM SCREEN TO VIEWPORT
      XP=(XS-XMARG)/XSCALS+VCXMN
      YP=(XS-YMARG)/YSCALS+VCYMN
C  FROM VIEWPORT TO VIEWSPACE
      XP=(XP-VXMIN)/XSCALE+UMIN
      YP=(YP-VYMIN)/YSCALE+VMIN
C  VIEWSPACE TO WORLD COORDS
      XW=XVW+XP*UVECX+YP*VVECX
      YW=YVW+XP*UVECY+YP*VVECY
      ZW=ZVW+XP*UVECZ+YP*VVECZ
      END