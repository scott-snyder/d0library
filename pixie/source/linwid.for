      SUBROUTINE LINWID( DRVNAM, ILTAB )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sets seventeen different kinds of line width 
C-                         on the table.
C-
C-   Inputs  : DRVNAM - Driver's name
C-   Outputs : ILTAB  - Line width's table with the 17 styles.
C-
C-   Created  28-NOV-1989   LUPE ROSAS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*3 DRVNAM
      INTEGER ILTAB(17), LWIDOR(17),LINE, I
      REAL    NUM
C----------------------------------------------------------------------
      LINE = 16383
      NUM = (32767.- 16384.)/170
      DO 100 I = 1, 17
        LWIDOR(I) = LINE
        ILTAB(I)  = LINE
        LINE = IFIX( (LINE + (NUM*3.5) ) )
  100 CONTINUE
      ILTAB(9)  = LWIDOR(1) 
      ILTAB(16) = LWIDOR(3) 
      ILTAB(6)  = LWIDOR(5) 
      ILTAB(14) = LWIDOR(7) 
      ILTAB(7)  = LWIDOR(9) 
  999 RETURN
      END
