      SUBROUTINE JBEGIN
C  INITIALIZE TO DEFAULT SETTINGS
      INCLUDE 'D0$INC:DI3INC.INC'
C   IJ1(46)   whose contents correspond to the call arguments of J1IGET
C   RJ3(3,11) whose contents correspond to the call arguments of J3RGET
C   RJ4(4,3)  whose contents correspond to the call arguments of J4RGET
C   RJ16(4,4)  whose contents correspond to the call arguments of J16GET
C
C BLOCK DATA
      DATA XSIZE,YSIZE,GAP      /0.,0.,0./
      DATA XBASEN,YBASEN,ZBASEN /0.,0.,0./
      DATA XPLANN,YPLANN,ZPLANN /0.,0.,0./
      DATA XSIZED,YSIZED,  GAPD /.02,.02,0./
      DATA XBASND,YBASND,ZBASND /1.,0.,0./
      DATA XPLNND,YPLNND,ZPLNND /0.,1.,0./
      DATA    XVW,   YVW,   ZVW /0.,0.,0./
      DATA VNORMX,VNORMY,VNORMZ /0.,0.,1./
      DATA   VUPX,  VUPY,  VUPZ /0.,1.,0./
      DATA  VDIST, HDIST, YDIST /1.,-99999.,99999./
      DATA  XPROJ, YPROJ, ZPROJ /0.,0.,-1./
C  INITIAL U,V VECTORS OF UVN SYSTEM.  N IS VNORM ABOVE
      DATA UVECX,UVECY,UVECZ /1.,0.,0./
      DATA VVECX,VVECY,VVECZ /0.,1.,0./
C
      DATA  UMIN, UMAX, VMIN, VMAX /-1.,1.,-1.,1./      ! (JWINDO)
      DATA VXMIN,VXMAX,VYMIN,VYMAX /-1.,1.,-1.,1./      ! (JVPORT)
      DATA VCXMN,VCXMX,VCYMN,VCYMX /-1.,1.,-1.,1./      ! (JVSPAC)
C
      DATA ISEGNM,INSIDE,IERROR,IDEBUG        /-1,1,0,0/
C  CURRENT LINE, POLY, AND MARK
      DATA ICOLOR,IINTEN,ILSTYL,ILWID ,IPEN   /0,0,0,0,0/
      DATA IPEDGE,IPSTYL,IPCOLO,IPINT ,IMARK  /0,0,0,0,0/
C  CURRENT PICK ID
      DATA IPICID                             /0/
C  LINE, POLY, AND MARK DEFAULTS
      DATA ICLRDF,IINTDF,ILSTDF,ILWDDF,IPENDF /0,16383,0,16383,0/
      DATA IPEDDF,IPINDF,IPCLDF,IPNTDF,IMRKDF /0,0,0,0,1/
C  DEFAULT PICK ID
      DATA IPIKDF                             /0/
C  CHARACTER DEFAULTS
      DATA IPATH ,IFONT ,IHJUST,IVJUST        /0,0,0,0/
      DATA IPTHDF,IFONDF,IHJUDF,IVJUDF        /1,1,1,1/
C  SEGMENT DEFAULTS
      DATA IVISDF,IHIDF ,IDETDF,IPRIDF,ITYPDF /1,0,0,0,0/
C  CLIPPING
      DATA IWCLIP,IHCLIP,IYCLIP               /0,0,0/
C  HANDEDNESS, PROJECTION TYPE, MODELLING, BATCH
      DATA IRIGHT,IPROJ ,IMODEL,IBATCH        /0,0,0,-1/
C  INITIAL MODELLING TRANSFORMATION MATRIX
      DATA MMODEL/1.,0.,0.,0., 0.,1.,0.,0., 0.,0.,1.,0., 0.,0.,0.,1./
C  THE FOLLOWING HAS BEEN MOVED TO DEVICE-SPECIFIC JIQDEV
C      DATA IQD/256,1,38,10,0,5,14,32767,0,60,
C     &         445,1,1,2,0,2,0,1,2,0,
C     &         0,0,0,0,1,0,0,0,0,0,
C     &         0,0,0,0,0,0,0,0,0,0,
C     &         1,1,1,2,1,1,1/
C
      DATA DI3INI/0/                 !init. flag (1=JBEGIN called)
      DATA DI3ADV/-1/                !no currently active device
      DATA JUNIT/6/                  !UNIT NUMBER FOR PRINTING
      DATA NSMXX,NBLOCX/100,400000/  !SEGMENT LIST LIMITS
      DATA ARL/1./                   !HANDEDNESS (DEFAULT=LEFT!)
C
      DI3INI = 1
C  MAKE ALL SEGMENTS VISIBLE
      DO 10 I=1,NSMXX
        IVIS(I)=1
   10 CONTINUE
C  SET UP THE PICK LIMIT ARRAYS
      DO 30 II=1,100
        PKXMN(II)=-99999.
        PKYMN(II)=-99999.
        PKXMX(II)=+99999.
        PKYMX(II)=+99999.
   30 CONTINUE
      RETURN
      END
