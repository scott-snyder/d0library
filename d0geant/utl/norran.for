      SUBROUTINE NORRAN(HASARD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SUBSTITUTE FOR V101 IN ORDER TO USE RNDM
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  13-MAR-1989   A. Zylberstejn
C-   Updated  22-MAY-1990   Alan M. Jonckheere  Call RANNOR *each* entry -
C-                              calling every other results in different random
C-                              number sequences depending on even/odd calling
C-                              order
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FIRST
      REAL HASARD,A,B
      DATA FIRST/.TRUE./
c      IF(FIRST)THEN
        CALL RANNOR(HASARD,B)
c        FIRST=.FALSE.
c      ELSE
c        HASARD=B
c        FIRST=.TRUE.
c      END IF
C----------------------------------------------------------------------
  999 RETURN
      END
