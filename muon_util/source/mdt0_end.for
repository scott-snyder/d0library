      SUBROUTINE MDT0_END
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: 
C-
C-   Created   6-FEB-1992   David Hedin
C-   Updated  16-FEB-1992   Eric James   added calls to dt0 end routines 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
CC    call routine to build new dt0 zebra banks
C
      CALL DT0ZEB
C
CC    call routine to write out new vs old dt0 values
C
      CALL DT0OUT
C
  999 RETURN
      END
