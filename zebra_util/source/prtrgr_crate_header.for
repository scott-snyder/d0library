      SUBROUTINE PRTRGR_CRATE_HEADER(LUN, CRATE_HEADER, WORD_COUNT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print out the information from the Level 1 Datablock
C-      Crate Header.
C-
C-   Inputs  : LUN          The unit number to write the information to
C-             CRATE_HEADER The array of Crate Header words
C-             WORD_COUNT   The Word Count from the Level 1 Datablock
C-   Outputs : none
C-   Controls: none
C-
C-   Created  24-JAN-1992   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
C
      CHARACTER*8 PRTRGR_INT_TO_HEX
      EXTERNAL PRTRGR_INT_TO_HEX
C
      INTEGER LUN
      INTEGER CRATE_HEADER(TRGR_HEADER_LENGTH)
      INTEGER WORD_COUNT
C
      INTEGER HEADER_WORD
C
      CHARACTER*19 HEADER_LABELS(TRGR_HEADER_LENGTH)
      DATA HEADER_LABELS / 'Header Length Count',
     &                     'SYNC Word',
     &                     'Controller Word',
     &                     'Version Number',
     &                     'Revision Number',
     &                     'Trigger Mask' /
C
  100 FORMAT(' ', A, T21, ':  ', A)
      DO HEADER_WORD = 1, TRGR_HEADER_LENGTH
        WRITE (LUN, 100) HEADER_LABELS(HEADER_WORD),
     &                   PRTRGR_INT_TO_HEX(CRATE_HEADER(HEADER_WORD))
      END DO
      WRITE (LUN,100) 'Word Count', PRTRGR_INT_TO_HEX(WORD_COUNT)
C
C----------------------------------------------------------------------
  999 RETURN
      END
