      SUBROUTINE TYPAGE (LIST,NLIST,BACK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display list of strings with paging.
C-
C-   Inputs:   LIST(*)     List of strings to display
C-             NLIST       Number of items
C-             BACK        If TRUE go back to upper menu level.
C-
C-   Outputs : None
C-
C-   Created  18-MAR-1988   Harrison B. Prosper
C-   Modified 18-MAR-1988
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) LIST(*)
      INTEGER       NLIST
      LOGICAL       QUIT,BACK
      INTEGER       N,I,J,L
      LOGICAL       PAGE
      CHARACTER*4   INUM
      CHARACTER*80  STRING
C
C----------------------------------------------------------------------
C
C ****  Display item list
C
      L = LEN (LIST(1))
      DO 10 I = 1,NLIST
        CALL INTMSG (LIST(I)(1:L))
        IF ( PAGE (QUIT,BACK) ) CALL ERASE
        IF ( QUIT .OR. BACK ) GOTO 999
   10 CONTINUE
C
  999 RETURN
      END
