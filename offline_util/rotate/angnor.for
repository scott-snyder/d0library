      FUNCTION ANGNOR(ANG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       normalize angle to the standard D0 range
C-   Returned value  : angle between 0 to 2 * pi
C-   Inputs  :unnormalized angle in radians
C-   Outputs : none
C-   Controls: none
C-
C-   Entry Points--ANORM, which is a synonym for ANGNOR
C-
C-   Created  16-MAR-1989   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL ANGNOR,ANG,ANORM
      INCLUDE 'D0$INC:PI.DEF'
C----------------------------------------------------------------------

      ENTRY ANORM (ANG)

      IF ( ANG.LT.0 ) THEN
        ANGNOR = MOD(DBLE(ANG),TWOPI)+ TWOPI
      ELSE
        ANGNOR = MOD(DBLE(ANG),TWOPI)
      ENDIF

      ANORM = ANGNOR
  999 RETURN
      END
