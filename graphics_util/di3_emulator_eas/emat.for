      SUBROUTINE EMAT(JSEG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  To flag that rotation is connected and
C-                          extracted rotation matrices should be
C-                          considered and used in JCONVW.  Also to set
C-                          the segment for which the matrices should
C-                          be considered (jseg).
C-
C-   Inputs  : JSEG   Segment for which the 3D matrices should
C-                    be extracted for.
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created   10-JUL-1990   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER JSEG
      INCLUDE 'D0$INC:NEWDI3.INC'
      INCLUDE 'D0$INC:GRFPAR.INC'
C
      CSEG = JSEG
C
      IF(.NOT. NUDI3) THEN
        EAS_CONN = .TRUE.
      ELSE
        EAS_CONN = .FALSE.
      ENDIF
C
  999 RETURN
      END
