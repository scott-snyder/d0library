      SUBROUTINE PX_CLOSE_OPEN_SEGMENT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check for an open segment and close it.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   5-APR-1991   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER CURSEG
C----------------------------------------------------------------------
      CALL J1IGET( 1, CURSEG )
      IF( CURSEG .EQ. 0 ) THEN
        CALL JCLOSE
        CALL STAMSG(' *** A TEMPORARY segment was left OPEN',.TRUE.)
      ELSEIF( CURSEG .GT. 0 ) THEN
        CALL JRCLOS
        CALL STAMSG(' *** A RETAINED segment was left OPEN ',.TRUE.)
      ENDIF
  999 RETURN
      END
