      SUBROUTINE PFQUADL(XTOP,YTOP,QUAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw Quadrant labels for FDC end view.
C-
C-   Inputs  : XTOP,YTOP Position of top of Quadrant.
C-             QUAD
C-   Outputs : 
C-
C-   Created   6-AUG-1991   Robert E. Avery
C-   Updated   8-NOV-1991   Robert E. Avery  reset base and plane. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  INPUT:
      REAL XTOP, YTOP 
      INTEGER QUAD
C  LOCAL:
      REAL XPOS, YPOS 
      REAL XBASE, YBASE 
      REAL XSIZE, YSIZE 
      CHARACTER*3 LAB(0:7)
      DATA LAB /'Q 0','Q 1','Q 2','Q 3','Q 4','Q 5','Q 6','Q 7'/
      DATA XSIZE, YSIZE /1.5,2.5/
C----------------------------------------------------------------------
      XPOS = 1.1 * XTOP
      YPOS = 1.1 * YTOP
C
      CALL JJUST(2,2)
      CALL JBASE(YPOS,-XPOS,0.)
      CALL JPLANE(XPOS,YPOS,0.)
      CALL JSIZE(XSIZE,YSIZE)
      CALL JMOVE( XPOS, YPOS )
      CALL J3STRG( LAB(QUAD) )
      CALL JBASE(1.0,0.0,0.)
      CALL JPLANE(0.0,1.0,0.)
C      
  999 RETURN
      END
