      SUBROUTINE PRPXPA(PRUNIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Printout the entire PXPARA.INC common and 
C-                         a few other DI-3000 values 
C-
C-   Inputs  : PRUNIT = output device number
C-
C-   Created  20-APR-1989   Jeffrey Bantly
C-   Updated   1-MAR-1993   Susan K. Blessing  Remove PRODUC. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PXPARA.INC'
      INTEGER PRUNIT
      REAL    XMN,XMX,YMN,YMX,RATIO
C----------------------------------------------------------------------
      WRITE(PRUNIT,*) ' '
      WRITE(PRUNIT,*) ' PXPARA.INC   DUMP'
      WRITE(PRUNIT,*) '   XDVMAG,YDVMAG-DEVICE X/Y MAG',XDVMAG,YDVMAG
      WRITE(PRUNIT,*) '   XCVPRT,YCVPRT,XMVPRT,YMVPRT=',
     &              XCVPRT,YCVPRT,XMVPRT,YMVPRT
      WRITE(PRUNIT,*) '   Viewport Sizes=',XVPRT1,XVPRT2,YVPRT1,YVPRT2
      WRITE(PRUNIT,*) '   Window Sizes  =',XWIND1,XWIND2,YWIND1,YWIND2
      WRITE(PRUNIT,*) '   Last retained segment number=',MAXSEG
C
      WRITE(PRUNIT,*) ' DI-3000 THINKS'   
      CALL JASPEK(1,RATIO)
      WRITE(PRUNIT,*) '   Aspect Ratio=',RATIO
      CALL J4RGET(2,XMN,XMX,YMN,YMX)
      WRITE(PRUNIT,*) '   Viewport Sizes=',XMN,XMX,YMN,YMX
      CALL J4RGET(1,XMN,XMX,YMN,YMX)
      WRITE(PRUNIT,*) '   Window Sizes  =',XMN,XMX,YMN,YMX
      WRITE(PRUNIT,*) ' '
C----------------------------------------------------------------------
  999 RETURN
      END
