      SUBROUTINE TRISRC(ITR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Finds the number of reconstructed tracks
C-                        in a cylinder defined by RIN,ZIN,
C-                        ROUT,ZOUT. Adapted from TRISTR.
C-
C-   Inputs  : ITR      =     0    : All tracks (in the sense of
C-                                     option OPT)
C-                      >     0    : Pointer to selected track
C-   Outputs :
C-
C-   Created   4-OCT-1989   J.Fr. Glicenstein
C-                                          track
C-   Updated  19-DEC-1990   A. Zylberstejn  Split the routine to call 
C-      different routines according to the value of ITR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ITR
      IF(ITR.EQ.0)THEN
        CALL TRISRC_FULL(ITR)
      ELSE
        CALL TRISRC_TRACK(ITR)
      END IF
  999 RETURN
      END
