      SUBROUTINE BKLBLS(LLBLS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book an LBLS Zebra bank in the ZEBCOM common.
C-
C-   Inputs  : none
C-   Outputs : LLBLS    Link to the new LBLS bank
C-   Controls: none
C-
C-   Created  20-SEP-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   Updated   3-JAN-1992   Philippe Laurens, Steven Klocek  
C-                      The LBLS bank is booked in the ZEBCOM common block. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:LSM_ZEB.PARAMS'
C
      INTEGER LLBLS, IOH, DUMMY
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /
C
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('LBLS','4I -F',DUMMY)
      ENDIF
C
      DUMMY = BANK_NULL
      CALL MZBOOK(IXMAIN, LLBLS, DUMMY, STAND_ALONE, 'LBLS', 
     &  13, 10, 
     &  (2 * NETAL + 1) * NPHIL * NLYRL +10, IDH_IOFORM, ZERO_BANK)
C----------------------------------------------------------------------
  999 RETURN
      END
