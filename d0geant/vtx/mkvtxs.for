      SUBROUTINE MKVTXS( ILAY )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make the 'data' and 'hits' for the two z-strip 
C-                         layers associated with wire layer LAY.  Hit 
C-                         data is stored in common blocks VZHITS and VZDATA.
C-
C-   Inputs  : ILAY [I] : wire layer - corresponds to two strip layers (inner
C-                       and outer).
C-
C-   Outputs : Fills hit bank VZLA if SVTX(2) = 1.
C-             Fills data bank VZDA if SVTX(1) = 1. or SVTX(3) = 1.
C-
C-   Created   9-NOV-1989   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$INC:VTLOCA.INC'
C
      INTEGER ILAY
C----------------------------------------------------------------------
      IF ( ILAY .EQ. 0 ) GO TO 999  ! strip layers 0, 1 not in hardware
C
      ZLAYER = 2 * ILAY                 ! inner layer number
C
C ****  Create the hits structure VTXH -- VZLA
C
      IF ( SVTX(2) .EQ. 1. ) THEN
        CALL BLVZLA
      ENDIF
C
C ****  Create the data structure VTXH -- VZLA -- VZDA
C
      IF ( SVTX(1) .EQ. 1. .OR. SVTX(3) .EQ. 1. ) THEN
        CALL BLVZDA
      ENDIF
C
  999 RETURN
      END
