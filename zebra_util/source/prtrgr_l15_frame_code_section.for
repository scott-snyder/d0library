      SUBROUTINE PRTRGR_L15_FRAME_CODE_SECTION(LUN, L15_FCS_BLOCK, NLWF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print out the information from the
C-                         L1.5 Datablock Frame Code Section.
C-
C-   Inputs  : LUN          The unit number to write the information to
C-             L15_FCS_BLOCK    The array of L1.5 Frame Code Section words
C-   Outputs : NLWF         Number of Longwords to follow
C-   Controls: none
C-
C-   Created  30-NOV-1993   Johannes V. (Djoko) Wirjawan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L15CALDBB_DATA_BLOCK.PARAMS'
C
      CHARACTER*8 PRTRGR_INT_TO_HEX
      EXTERNAL PRTRGR_INT_TO_HEX
C
      INTEGER LUN, L15_FCS_BLOCK(L15CAL_FRAME_LENGTH)
      INTEGER FRAME_WORD, NLWF
C
C----------------------------------------------------------------------
C
      CHARACTER*40 FRAME_LABELS(L15CAL_FRAME_LENGTH)
      DATA FRAME_LABELS / 'Frame Code Longwords to follow',
     &                     'L15 CT Engine Control Starting Status',
     &                     'L15 CT Engine Control Finishing Status',
     &                     'L15 CT Readout Control Finishing Status',
     &                     'List of L1 Sp Trig Fired for this Event',
     &                     'Mask of Terms that Passed'/
C
  100 FORMAT(' ', A, T42, ': ', I2)
  109 FORMAT(' ', A, T42, ': ', A)
      DO FRAME_WORD = 1, L15CAL_FRAME_LENGTH
         IF (FRAME_WORD.EQ.1) THEN
            WRITE (LUN, 100) FRAME_LABELS(1),
     &             L15_FCS_BLOCK(1)
         ELSE
            WRITE (LUN, 109) FRAME_LABELS(FRAME_WORD),
     &             PRTRGR_INT_TO_HEX(L15_FCS_BLOCK(FRAME_WORD))
         ENDIF
      END DO
      NLWF = L15_FCS_BLOCK(1)
C----------------------------------------------------------------------
  999 RETURN
      END
