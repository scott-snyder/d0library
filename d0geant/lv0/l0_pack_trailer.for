      SUBROUTINE L0_PACK_TRAILER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Pack CRATE TRAILER for LEVEL 0
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created   9-DEC-1992   Freedy Nang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:LV0PARAM.INC'
      INTEGER HEAD
C----------------------------------------------------------------------
      HEAD= 103        ! Saved for HEAD, ADC, and VERTEX words
      CALL MVBITS(107,0,32,DATA_WORD(HEAD+1),0)  ! total number of long words
      CALL MVBITS(0,0,16,DATA_WORD(HEAD+2),16)   ! trig id seen by VBD
      CALL MVBITS(1,0,16,DATA_WORD(HEAD+2),0)    ! crate id seen by VBD
C----------------------------------------------------------------------
  999 RETURN
      END
