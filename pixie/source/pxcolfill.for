      SUBROUTINE PXCOLFILL(COLOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This is an interface that will call the color table
C-                      (PXCOLN) and perform the color and filling operations
C-                      JPIDEX, JPINTR.   Assumes that a segment is open
C-
C-   Inputs  : COLOR [C*3] - Colro desired to use.  First three letters of the
C-                           color.
C-
C-   Created   5-JAN-1990   LUPE ROSAS
C-   Updated   4-APR-1990   Lupe Howell  Evans & Suterland compatible 
C-   Updated   9-JUN-1992   Lupe Howell   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*3 COLOR, IDET
C----------------------------------------------------------------------
      INTEGER     ITYP, INDX, KCOLOR, KINTEN, KFILL, KSTYL,
     &  ENSCOL(17) 
      REAL RLEVEL
      LOGICAL     CALFLG
      DATA        CALFLG /.FALSE./
      DATA        ITYP /1/
      DATA        IDET /'CDC'/
      DATA        ENSCOL/0, 0, 13, 31, 4, 15, 6, 2, 17, 11,
     &            5, 1, 14, 20, 16, 3, 0/
C----------------------------------------------------------------------
      IF (COLOR .EQ. 'WHI') THEN
        INDX = 1
      ELSEIF (COLOR .EQ. 'BLA') THEN
        INDX = 2
      ELSEIF (COLOR .EQ. 'DPU') THEN   ! Dark purple
        INDX = 3
      ELSEIF (COLOR .EQ. 'PUR') THEN   ! Purple
        INDX = 4
      ELSEIF (COLOR .EQ. 'DBL') THEN   ! Dark Blue
        INDX = 5
      ELSEIF (COLOR .EQ. 'BLU') THEN   ! Blue
        INDX = 6
      ELSEIF (COLOR .EQ. 'CYA') THEN   ! Cyan
        INDX = 7
      ELSEIF (COLOR .EQ. 'DGR') THEN   ! Dark Green
        INDX = 8
      ELSEIF (COLOR .EQ. 'GRE') THEN   ! Kelly Green
        INDX = 9
      ELSEIF (COLOR .EQ. 'YGR') THEN   ! Yellow Green
        INDX = 10
      ELSEIF (COLOR .EQ. 'BRO') THEN   ! Brown
        INDX = 11
      ELSEIF (COLOR .EQ. 'DRE') THEN   ! Dark Red
        INDX = 12
      ELSEIF (COLOR .EQ. 'RED') THEN   ! Red
        INDX = 13
      ELSEIF (COLOR .EQ. 'MAG') THEN   ! Magenta
        INDX = 14
      ELSEIF (COLOR .EQ. 'ORA') THEN   ! Orange
        INDX = 15
      ELSEIF (COLOR .EQ. 'YEL') THEN   ! Yellow
        INDX = 16
      ELSEIF (COLOR .EQ. 'FOR') THEN   ! Foreground
        INDX = 17
      ELSE                             !  Bad input parameter
        GO TO 999
      ENDIF
      CALL JIQDIL(RLEVEL)
      IF ( RLEVEL.LT.0.AND.RLEVEL.GT.-20)  THEN           ! Evens & Sutherland machine
        CALL JCOLOR(ENSCOL(INDX))
      ELSE
        CALL PXCOLN(IDET,INDX,ITYP,CALFLG,KCOLOR,KINTEN,KFILL,
     &    KSTYL)
        CALL JPIDEX( KCOLOR,KINTEN)
        CALL JPINTR( KFILL )
      ENDIF
  999 RETURN
      END
