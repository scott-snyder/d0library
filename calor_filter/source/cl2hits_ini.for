      FUNCTION CL2HITS_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-              INITIALIZATION FOR CL2HITS package (LEVEL 2 fast CAHITS);
C-              set up as package USRINI hook
C-   Inputs  : NONE
C-   Outputs :
C-   Controls:
C-
C-   Created   3-MAY-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CL2HITS_INI
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL FILTER_STP_HISTORY
        CALL CL2_MAKE_TABLES
        FIRST = .FALSE.
      ENDIF
      CL2HITS_INI = .TRUE.
      RETURN
      END
