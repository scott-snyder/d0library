      SUBROUTINE DUMP
C PRINTS THE DI-3000 EMULATOR COMMON BLOCKS
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      JUNIT=61
      WRITE(JUNIT,*)
      WRITE(JUNIT,*)'View Space:',VCXMN,VCXMX,VCYMN,VCYMX
      WRITE(JUNIT,*)'Viewport  :',VXMIN,VXMAX,VYMIN,VYMAX
      WRITE(JUNIT,*)'Window    :',UMIN,UMAX,VMIN,VMAX
      WRITE(JUNIT,*)'View Refer Point:',XVW,YVW,ZVW
      WRITE(JUNIT,*)'Viewplane Normal:',VNORMX,VNORMY,VNORMZ
      WRITE(JUNIT,*)'View  UP  Vector:',VUPX,VUPY,VUPZ
      WRITE(JUNIT,*)'VDIST,HDIST,YDIS:',VDIST,HDIST,YDIST
      WRITE(JUNIT,*)'     U    Vector:',UVECX,UVECY,UVECZ
      WRITE(JUNIT,*)'     V    Vector:',VVECX,VVECY,VVECZ
      WRITE(JUNIT,*)'XEYE,YEYE,ZEYE  :',XPROJ,YPROJ,ZPROJ
      WRITE(junit,*)'Modelling flag  :',imodel
      WRITE(JUNIT,'(a/(4f13.3))')' MODELLING Matrix:',MMODEL
      WRITE(JUNIT,*)'SCALE FACTORS :',XSCALE,YSCALE,XSCALS,YSCALS
      WRITE(JUNIT,*)'ISEGNM,ITYCUR :',ISEGNM,ITYCUR
      WRITE(JUNIT,*)'PXC,PYC,TXC,TYC :',PXC,PYC,TXC,TYC
      WRITE(JUNIT,*)'CSC,SNC,XMARG,YMARG :',CSC,SNC,XMARG,YMARG
      WRITE(JUNIT,*)'  Cur. Pos:',XPOSN,YPOSN,ZPOSN
      WRITE(JUNIT,*)'Character Baseline:',XBASE,YBASE,ZBASE
      WRITE(JUNIT,*)'      (Normalized):',XBASEN,YBASEN,ZBASEN
      WRITE(JUNIT,*)'Character    Plane:',XPLANE,YPLANE,ZPLANE
      WRITE(JUNIT,*)'      (Normalized):',XPLANN,YPLANN,ZPLANN
      WRITE(JUNIT,*)'XSIZE,YSIZE,GAPD  :',XSIZE,YSIZE,GAPD
      WRITE(JUNIT,10)((JJ,IJ1(JJ)),JJ=1,46)
   10 FORMAT('  J1IGET ARRAY:',/,12('  ',4('(',I2,')',I),/))
      WRITE(JUNIT,*)
      END
