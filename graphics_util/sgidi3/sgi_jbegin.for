      SUBROUTINE JBEGIN
C  INITIALIZE TO DEFAULT SETTINGS
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
C   IJ1(46)   whose contents correspond to the CALL arguments of J1IGET
C   RJ3(3,11) whose contents correspond to the CALL arguments of J3RGET
C   RJ4(4,3)  whose contents correspond to the CALL arguments of J4RGET
C   RJ16(4,4)  whose contents correspond to the CALL arguments of J16GET
C
C BLOCK DATA
      DATA XSIZE,YSIZE,GAP      /0.,0.,0./
      DATA XBASEN,YBASEN,ZBASEN /0.,0.,0./
      DATA XPLANN,YPLANN,ZPLANN /0.,0.,0./
      DATA XSIZED,YSIZED,  GAPD /.02,.02,0./
      DATA XBASND,YBASND,ZBASND /1.,0.,0./
      DATA XPLNND,YPLNND,ZPLNND /0.,1.,0./
      DATA    XVW,   YVW,   ZVW /0.,0.,0./
C      DATA VNORMX,VNORMY,VNORMZ /0.,0.,1./
      DATA VNORMX,VNORMY,VNORMZ /0.,0.,-1./
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
      DATA IPEDGE,IPSTYL,IPCOLO,IPINT ,IMARK  /0,0,0,0,1/
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
      DATA IRIGHT,IPROJ ,IMODEL,IBATCH        /1,0,0,-1/
C      DATA IRIGHT,IPROJ ,IMODEL,IBATCH        /0,0,0,-1/
C  INITIAL MODELLING TRANSFORMATION MATRIX
      DATA MMODEL/1.,0.,0.,0., 0.,1.,0.,0., 0.,0.,1.,0., 0.,0.,0.,1./
C
      DATA IQD/12,1,1,1,1,1,1,1,0,1, 1,0,2,2,0,2,0,1,0,0, 27*0/
C
      DATA DI3INI/0/                 !init. flag (1=JBEGIN CALLed)
      DATA DI3ADV/-1/                !no currently active device
      DATA DI3DIN/0/                 !JDEVON NOT YET CALLED
      DATA JUNIT/6/                  !UNIT NUMBER FOR PRINTING
C
C  Following used for hardcopy RGB tables
C                 BLACK, RED, GREEN, YELLOW, BLUE
C                 MAGENTA, CYAN, WHITE, BLACK, ORANGE, LIME
C                 INDIGO, DARK-GRAY, WHITE
C                 LIGHT-RED, LIGHT-GREEN, LIGHT-BLUE
      DATA RVEC/
     &            0,255,31,255,31,
     &            255,31,255,0,255,127,
     &            127,127,255,
     &            255,127,127,33*0/
      DATA GVEC/
     &            0,31,255,255,31,
     &            31,255,255,0,127,255,
     &            31,127,255,
     &            127,255,127,33*0/
      DATA BVEC/
     &            0,31,31,31,255,
     &            255,255,255,0,31,31,
     &            255,127,255,
     &            127,127,255,33*0/
C
      SAVE
C-------------------
      DI3INI=1
C  MAKE ALL SEGMENTS VISIBLE
      DO I=1,NSMXX
        IVIS(I)=1
      ENDDO
      IF(IDEBUG.GT.0) TYPE *,' JBEGIN CALLED'
c      type *,' Written debug unit number?:'
c      accept 8009,idebwr
c 8009 format(i)
      if(idebwr.gt.0) then
        write(idebwr,8008)
 8008   format(' JBEGIN')
      endif
      RETURN
      END
