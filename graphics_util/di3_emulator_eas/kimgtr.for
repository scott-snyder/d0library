      SUBROUTINE KIMGTR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  To include the image tranfformations.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   7-JUN-1989   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I
      REAL TTRV(3), TSCV(3)
      EXTERNAL ERRHND
C
      DO 1 I=1,3
        TTRV(I) = 0.0
        TSCV(I) = 1.0
    1 CONTINUE
C
      CALL PTRANS('IP"', TTRV, '"', ERRHND)
      CALL PROTX('IRX"', 0.0, '"', ERRHND)
      CALL PROTY('IRY"', 0.0, '"', ERRHND)
      CALL PROTZ('IRZ"', 0.0, '"', ERRHND)
      CALL PSCALE('IS"', TSCV, '"', ERRHND)
      CALL PTRANS('IN"', TTRV, '"', ERRHND)
      CALL PTRANS('IT"', TTRV, '"', ERRHND)
C
  999 RETURN
      END
