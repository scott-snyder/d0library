      SUBROUTINE L0_PACK_HEADER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Pack CRATE HEADER for LEVEL 0
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
C----------------------------------------------------------------------
      CALL MVBITS(4,0,32,DATA_WORD(1),0) ! 4: Header length count
      CALL MVBITS(0,0,32,DATA_WORD(2),0) ! 0: Beam Crossing Number
      CALL MVBITS(1,0,8,DATA_WORD(3),24) ! 1: LEVEL 0 Crate id
      CALL MVBITS(3,0,8,DATA_WORD(3),16) ! 3: # of LEVEL 0 data blocks
      CALL MVBITS(1,0,16,DATA_WORD(3),0) ! 1: DATA
      CALL MVBITS(0,0,32,DATA_WORD(4),0) ! 0: Version Number
      CALL MVBITS(1,0,6,DATA_WORD(5),8)  ! 1: Valid bunch mask
      CALL MVBITS(0,0,8,DATA_WORD(5),0)  ! 0: LV0 bunch id of present crossing
C----------------------------------------------------------------------
  999 RETURN
      END
