      SUBROUTINE PXLINE(ILINE,NPTS,XPTS,YPTS,ZPTS)
C====================================================================
C
C  Description:  Connects a series of NPTS points with a line of
C  ============  DI-3000 linestyle ILINE
C
C  Author:
C  ========
C  Tami Kramer
C
C  Conditions necessary before call:
C  =================================
C  Graphics initialized
C
C  Revision History:
C  =================
C  Original Creation - December 5,1986
C
C=====================================================================
C
      IMPLICIT NONE
C
C  Local Declarations:
C  ===================
C
      INTEGER I,ILINE,NPTS
      REAL XPTS(*),YPTS(*),ZPTS(*)
C
C  Executable Code:
C  ================
C 
      IF (NPTS .EQ. 1) THEN
          PRINT*,'ERROR - INSUFFICIENT POINTS IN ROUTINE PXLINE'
      ENDIF
C
      CALL JLSTYL(ILINE)
      CALL J3MOVE(XPTS(1),YPTS(1),ZPTS(1))
C
      DO 78 I = 1,NPTS-1
C
         CALL J3DRAW(XPTS(I+1),YPTS(I+1),ZPTS(I+1))
C
   78 CONTINUE
C
      RETURN
      END
