      SUBROUTINE DEV_WINDOW_CORNERS(UMN,UMX,VMN,VMX)
C  For an open window, specify the corners in world coordinates.
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      DATA VXMINH,VXMAXH,VYMINH,VYMAXH/-1.,1.,-1.,1./
      SAVE
      IF(IGSYNC.NE.1) THEN
        TYPE *,' DEV_OPEN_WINDOW should be called'
        TYPE *,'        before DEV_WINDOW_CORNERS'
        RETURN
      ENDIF
C
C  Window corners in world coordinates
      UMINH=UMN
      UMAXH=UMX
      VMINH=VMN
      VMAXH=VMX
C  Default character size
      XSIZEH=.02*(UMAXH-UMINH)
      YSIZEH=.02*(VMAXH-VMINH)
      IWORLD=0
C
C  Pick up the device corners (screen, page, etc)
      IF(IDV.EQ.1) THEN
        CALL PS_CRNS(DVXMN,DVYMN,DVXMX,DVYMX)
      ELSEIF(IDV.EQ.2) THEN
        CALL TK_CRNS(DVXMN,DVYMN,DVXMX,DVYMX,1)  ! HARDCOPY TEK
      ELSEIF(IDV.EQ.3) THEN
        CALL TK_CRNS(DVXMN,DVYMN,DVXMX,DVYMX,2)  ! SCREEN TEK
      ENDIF
C
C  Apply the screen fractions given in OPEN_WINDOW to find the corners
C  of the window in screen (or hardcopy device) coordinates.
   20 WIDX=DVXMX-DVXMN
      WIDY=DVYMX-DVYMN
C  Device window coordinates.
      DWXMN=DVXMN+WNX*WIDX
      DWYMN=DVYMN+WNY*WIDY
      DELX=(WNX+WNDX)*WIDX
      DELY=(WNY+WNDY)*WIDY
      DWXMX=DVXMN+DELX
      DWYMX=DVYMN+DELY
      RASPH=WIDY/WIDX
C
C  Now find the transformation from window world coordinates
C  to window screen coordinates.
   30 XSCL=(DWXMX-DWXMN)/(UMAXH-UMINH)
      YSCL=(DWYMX-DWYMN)/(VMAXH-VMINH)
C
C  Viewport transformation factors
      XSCAL=XSCL*(VXMAXH-VXMINH)/2.
      YSCAL=YSCL*(VYMAXH-VYMINH)/(2.*RASPH)
      DXOFF=DWXMN+(DWXMX-DWXMN)*(VXMINH+1.)/2.
      DYOFF=DWYMN+(DWYMX-DWYMN)*(VYMINH/RASPH+1.)/2.
C  Transformation will be XDEV = (X-UMINH)*XSCAL+DXOFF, etc., in
C  DEVICE
C      XOUT=(XP-UMINH)*XSCAL+DXOFF
C      YOUT=(YP-VMINH)*YSCAL+DYOFF
C
C  Setup the device transformation matrix
      CALL D_MATUNI(TDEVIC)
      TDEVIC(1,1)=XSCAL
      TDEVIC(1,4)=DXOFF-UMINH*XSCAL
      TDEVIC(2,2)=YSCAL
      TDEVIC(2,4)=DYOFF-VMINH*YSCAL
      call d_dmpmat('DEV_WINDOW_CORNERS-TDEVIC:',TDEVIC)
C
      RETURN
C
      ENTRY DEV_VPORT(VXMN,VXMX,VYMN,VYMX)
C  Set a viewport.  Ranges run from -1. to 1.
      VXMINH=VXMN
      VXMAXH=VXMX
      VYMINH=VYMN
      VYMAXH=VYMX
      GO TO 30
C
      END
