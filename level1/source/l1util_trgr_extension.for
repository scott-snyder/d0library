      SUBROUTINE L1UTIL_TRGR_EXTENSION(LTRGR, DELTA_WORDS, 
     &  SUBSYSTEM_OFFSET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Increase the size of the TRGR bank, and give the
C-    offset for the first word in the newly allocated section.
C-
C-   Inputs  : LTRGR  The address of the TRGR bank. NOTE: This may be modified
C-                    if the TRGR bank is relocated. On return, this argument
C-                    contains the address of the (possibly relocated) TRGR
C-                    bank.
C-
C-             DELTA_WORDS  The number of words to add to the TRGR bank.
C-
C-   Outputs : SUBSYSTEM_OFFSET The offset into the TRGR bank of the first
C-                              newly allocated word. 
C-
C-   Controls: none
C-
C-  NOTE: On return, the first newly allocated word may be accessed by:
C-        IQ(LTRGR + SUBSYSTEM_OFFSET + 1)
C-
C-  NOTE: The TRGR bank must have already been booked.
C-
C-   Created  14-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LTRGR, DELTA_WORDS, SUBSYSTEM_OFFSET
C
C       Save the original length of the TRGR bank
      SUBSYSTEM_OFFSET = IQ(LTRGR-1)
C
C       Extend the bank
      CALL MZPUSH(IXMAIN, LTRGR, 0, DELTA_WORDS, ' ')
C----------------------------------------------------------------------
  999 RETURN
      END
