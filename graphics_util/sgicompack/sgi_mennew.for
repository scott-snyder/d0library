      SUBROUTINE MENNEW(MNUNAM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Add a menu of name MNUNAM (fill with MENADD).
C-
C-   Inputs:  MNUNAM [C*]:
C-
C-   Outputs: None
C-
C-   Created XX-XXX-XXXX Mike Shupe
C-   Updated 09-FEB-1993 Lupe Howell Clean up
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) MNUNAM
C
      INCLUDE 'D0$GRAPHICS_UTIL$SGICOMPACK:MAINPACK.INC'
C-----------------------------------------------------------------
C
C *** Increement the number of menus
C *** If the maximum nuber of menus allowed is reach exit
C
      NMENU=NMENU+1
      IF(NMENU.GT.NMENMX) THEN
        TYPE *,' NUMBER OF MENUS EXCEEDED'
        GO TO 999
      ENDIF
C
C *** Store the name of the menu
C
      FNAME(NMENU)=MNUNAM
  999 RETURN
      END
