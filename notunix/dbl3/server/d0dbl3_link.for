      SUBROUTINE D0DBL3_LINK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Protect link area for d0dbl3 routines
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   17-JUN-1992   S. Abachi
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:D0DBL3_SRVR.INC'
      INCLUDE 'D0$INC:D0DBL3_LNK.INC'
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZLINK (IXSTP,'/D0DBL3_LNK/',D0DBLNK,D0DBLNK,D0DBLNK)
      ENDIF
C
  999 RETURN
      END
