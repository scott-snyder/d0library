C----------------------------------------------------------------------
      INTEGER FUNCTION D3UTOP(PATH,TOPD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will return the top directory from a path
C-    name, in the case where no path structure (//topd/subd ...), it
C-    will return PATH as the top directory
C-
C-   Returned value  : Length of name of top directtory
C-   Inputs  :  PATH  (C)  Name of path
C-   Outputs :  TOPD  (C)  Top directory
C-   Controls: 
C-
C-   Created   9-OCT-1992   Lars Rasmussen
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ICFIND,ICLOC
C
      CHARACTER *(*) PATH,TOPD
      CHARACTER      CHLP*80
      INTEGER C1,C2,LC
C----------------------------------------------------------------------
      CHLP = PATH
      CALL UPCASE (CHLP,CHLP)
      CALL DBSBLC(CHLP,CHLP,LC)
      IF (LC .LE. 0) THEN
         TOPD = ' '
         D3UTOP = 0
         RETURN
      END IF
C
      C1 = ICLOC('//',2,CHLP,1,LC)
      IF (C1 .GE. 1 .AND. C1 .LE. LC) THEN
         C1 = C1 + 2
      ELSE
         C1 = 1
      END IF
      C2 = ICFIND('/',CHLP,C1,LC)
      IF (C2 .GE. 1 .AND. C2 .LE. LC) THEN
         C2 = C2 - 1
      ELSE
         C2 = MIN(LC,8)
      END IF
      IF (C1 .GE. 1 .AND. C2 .GE. C1) THEN
         D3UTOP = C2-C1+1
         TOPD = CHLP(C1:C2)
      ELSE
         D3UTOP = 0
         TOPD = ' '
      END IF
C
999   RETURN
      END
