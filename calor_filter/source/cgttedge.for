      SUBROUTINE CGTTEDGE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      build a table /TTEDGE/ which shows which eta coordinate in PTCAEP2
C-      represents the "lower" edge of a trigger tower, for purposes of block
C-      zeroing.
C-      based on CGTTPH.FOR
C-   Inputs  : none
C-   Outputs : /TTEDGE/
C-   Controls: none
C-
C-   Created   9-NOV-1990   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER TTETA
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'  ! L-1 to PHysics [this routine]
      INCLUDE 'D0$INC:TTEDGE.INC'
C----------------------------------------------------------------------

      DO TTETA = -NETAL11,NETAL11
        IF (ABS(TTETA) .LT. MNCTTE) THEN
          TTEDGE(TTETA) = 2*TTETA - 1     ! Lower edge for + eta
          IF (TTETA.LT.0) TTEDGE(TTETA) = 2*TTETA ! and for - eta
        ELSE
          TTEDGE(TTETA) = SIGN((ABS(TTETA) + MNCTTE-1), TTETA)
        ENDIF
      ENDDO

  999 RETURN
      END
