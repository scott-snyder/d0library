C=============================================================================
      SUBROUTINE PMAXES(IVIEW)
C=============================================================================
C
C  Description:  Outputs a set of axes with X,Y Z labels.
C  ============
C
C  Author:
C  =========
C  Tami Kramer
C
C  Revision History:
C  ==================
C  Original Creation - March 19,1988
C  Modified by V.Bhatnagar 2-MAR-1994 Added IVIEW = 6 (Z_X view)
C     Changed Initial X and Y values (800 -> 1000 & -350 -> -750)
C==============================================================================
C
      IMPLICIT NONE
C
C  Local Declarations:
C  ====================
C
      INTEGER IVIEW
      REAL AXSIZ,AXDEL,X,Y
C
      LOGICAL COMBINED_MODE,FLGVAL
      DATA AXSIZ/100./
      DATA AXDEL/15./
      DATA X/1000./
      DATA Y/-750./
C
C  Executable Code:
C  =================
C
      CALL PUOPEN
C
C   Draw lines for axes...
C  ===============================
C
      CALL JMOVE(-X,AXSIZ+Y)
      CALL JDRAW(-X,Y)
      CALL JDRAW(-X+AXSIZ,Y)
C
C  Label the axes....
C  =====================
C
      CALL JSIZE(50.,50.)
      CALL JMOVE(-X+AXSIZ+AXDEL,Y)
      IF (IVIEW .EQ. 1) CALL J3STRG('Z')
      IF (IVIEW .EQ. 2) CALL J3STRG('X')
      IF (IVIEW .EQ. 3) CALL J3STRG('X')
      IF (IVIEW .EQ. 6) CALL J3STRG('Z')
      CALL JMOVE(-X,Y+AXSIZ + AXDEL)
      IF (IVIEW.EQ.1.OR.IVIEW.EQ.2) CALL J3STRG('Y')
      IF (IVIEW.EQ.3) CALL J3STRG('Z')
      IF (IVIEW.EQ.6) CALL J3STRG('X')
CCC   MARK VERTEX
      COMBINED_MODE = FLGVAL('COMBINED_MODE')
      IF (.NOT.COMBINED_MODE) THEN
        CALL JMOVE(0.,0.)
        CALL J3STRG('O')
      ENDIF
      CALL JRCLOS
      RETURN
      END
