      SUBROUTINE WAIBIT(ISEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Hibernate for a number of seconds
C-
C-   Inputs  : ISEC: Number of seconds to hibernate (INTEGER)
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ISEC
      INTEGER WAITIT,ISTAT
C----------------------------------------------------------------------
      ISTAT=WAITIT(FLOAT(ISEC))                ! Hibernate for SEC seconds
      RETURN
      END
