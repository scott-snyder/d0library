      SUBROUTINE PRTRGR_L15_DEBUG_TYPE3(LUN, NLWF, LDBG3)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print out the information from the
C-                         L1.5 Datablock Frame Section.
C-
C-   Inputs  : LUN    (I)   The unit number to write the information to
C-             NLWF   (I)   Number of longwords to follow for this entry
C-             LDBG3  (I)   The array of Type 3 entry of DBG Sect. words
C-   Outputs : none
C-   Controls: none
C-
C-   Created   2-MAY-1994   Johannes V. (Djoko) Wirjawan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*8 PRTRGR_INT_TO_HEX, NROW
      EXTERNAL PRTRGR_INT_TO_HEX
C
      INTEGER LUN, NLWF, LDBG3(NLWF)
      INTEGER  I, ID_ENT
C
C----------------------------------------------------------------------
C      CALL L15CALDBB_DATA_BLOCK_BUILDER()
  104 FORMAT(' ', A, T50, ': ', A)
      WRITE (LUN, 104) 'Term Number using these Derived Constants',
     &                  PRTRGR_INT_TO_HEX(LDBG3(1))
      WRITE (LUN, 104) 'Tool Number generating these Derive Constants',
     &                  PRTRGR_INT_TO_HEX(LDBG3(2))
      DO I = 3, NLWF
         NROW   = PRTRGR_INT_TO_HEX(LDBG3(I))
  109    FORMAT(' Derived Constant #', I3,27X, ': ', A)
         WRITE (LUN, 109) I-2, NROW
      ENDDO
      WRITE (LUN,*)
C----------------------------------------------------------------------
  999 RETURN
      END
