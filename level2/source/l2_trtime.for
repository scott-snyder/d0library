      SUBROUTINE L2_TRTIME (TRTIM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To mark the overall trigger time (Event T0)
C-                         for each event.  Will this come from a crossing
C-                         number or a LEVEL1 calculation?
C-               DUMMY ROUTINE FOR NOW (MC doesn't need trigger time)
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   8-NOV-1991   Daniel R. Claes
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:L2TRAK.INC'
C----------------------------------------------------------------------
      REAL TRTIME, TRTIM
      COMMON /L2TIME/ TRTIME  ! TRTIME from DTRAKS offline analysis
C
      IF (COS_FLAG) THEN
        TRTIM = TRTIME
      ELSE
        TRTIM = 0.
      ENDIF
C
  999 RETURN
      END
