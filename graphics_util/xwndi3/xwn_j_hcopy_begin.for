      SUBROUTINE J_HCOPY_BEGIN(TITLE,ICOL,IERR)
C  When doing hardcopy, this routine is called first, then all drawing
C  primitives are gone through to recreate the image.
C  TITLE   = (DOESN'T APPEAR ON HARDCOPY AT THE MOMENT)
C  ICOL    = 0 BLACK AND WHITE
C          = 1 COLOR
C  IERR    = 0 FOR NO ERROR
C----------------------------------------------------------------------
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      CHARACTER*(*) TITLE
      CHARACTER*12 HCFILNAM
      CHARACTER*1 SNAME(12)
      EQUIVALENCE (HCFILNAM,SNAME)
      DATA HCFILNAM/'HCPY0000.DAT'/
      SAVE
C----------------------------------------------------------------------
C
C  If no driver has been selected, ask for one.
C!!!!hardwire hardcopy driver for now
      IHCDEV=1
      IF(IHCDEV.EQ.0) THEN
        TYPE *,' PLEASE SELECT HARDCOPY DRIVER AND TRY AGAIN!'
        IERR=-1
        RETURN
      ENDIF
C
C  Make a unique name for each file this session (HCPYXXXX.DAT)
      IHCNUM=IHCNUM+1
      ITEM2=IHCNUM/1000
      SNAME(5)=CHAR(ITEM2+48)
      ITEM3=(IHCNUM-ITEM2*1000)/100
      SNAME(6)=CHAR(ITEM3+48)
      ITEM4=(IHCNUM-ITEM2*1000-ITEM3*100)/10
      SNAME(7)=CHAR(ITEM4+48)
      ITEM5=IHCNUM-ITEM2*1000-ITEM3*100-ITEM4*10
      SNAME(8)=CHAR(ITEM5+48)
C
C  Open the hardcopy device.  (This sets IDV=IHCDEV.)
c      type *,' Calling DEV_OPEN_WINDOW'
      CALL DEV_OPEN_WINDOW(IHCDEV,TITLE,0.,0.,1.,1.,ICOL,0,IERR)
C
C  Pick up current window.
C!!!THIS MAY BE UNNECESSARY!!!
c      type *,' Calling DEV_WINDOW CORNERS'
      CALL DEV_WINDOW_CORNERS(UMIN,UMAX,VMINH,VMAX)
C
      HCPY=.TRUE.
      RETURN
      END
