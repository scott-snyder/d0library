      SUBROUTINE MENUOP(NUMOPT,MENOPT,SELECTED)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display a list of menu options, and return
C-   the number corresponding to the option selected by the user.
C-
C-   Inputs  : NUMOPT   [I]     Number of menu options
C-             MENOPT   [C*]    Menu options
C-
C-   Outputs : SELECTED [I]     Number of the option selected.
C-
C-   Created  15-MAR-1991   Jan S. Hoftun
C-                          LUPE HOWELL, Harrison B. Prosper
C-      Based on GETOPT.
C-   Updated   5-JUL-1991   Harrison B. Prosper   
C-      Now uses GETOPT
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NUMOPT,SELECTED
      CHARACTER*(*) MENOPT(*)
C----------------------------------------------------------------------
      SELECTED = 0
      CALL GETOPT(1,' ',NUMOPT,MENOPT,SELECTED)
  999 RETURN
      END
