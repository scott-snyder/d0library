      SUBROUTINE LSO_COPYWORDS(SOURCE, DEST, LENGTH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Copy LENGTH words from SOURCE to DEST. On VAXVMS, use
C-    the OTS$MOVE3 library routine.
C-
C-   Inputs  : SOURCE   The input array
C-   Outputs : DEST     The output array
C-   Controls: LENGTH   The number of words to move.
C-
C-   Created  20-JUN-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER SOURCE(1:*)
      INTEGER DEST(1:*)
      INTEGER LENGTH
      INTEGER COUNT
C
C&IF VAXVMS
      CALL OTS$MOVE3( %VAL(LENGTH * 4), SOURCE, DEST)
C&ELSE
C&      DO COUNT = 1, LENGTH
C&        DEST(COUNT) = SOURCE(COUNT)
C&      ENDDO
C&ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
