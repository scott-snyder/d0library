      SUBROUTINE PRTRGR_L15_CRATE_HEADER(LUN, L15_CRATE_HEADER, NLWF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print out the information from the
C-                         L1.5 Datablock Crate Header.
C-
C-   Inputs  : LUN          The unit number to write the information to
C-             L15CAL_CRATE_HEADER The array of L1.5 Crate Header words
C-
C-   Output  : NLWF         Number of longwords to follow
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
      INTEGER LUN, L15_CRATE_HEADER(L15CAL_HEADER_LENGTH)
      INTEGER HEADER_WORD, NLWF
C
C----------------------------------------------------------------------
C
      CHARACTER*24 HEADER_LABELS(L15CAL_HEADER_LENGTH)
      DATA HEADER_LABELS / 'Header Length Count',
     &                     'SYNC Word',
     &                     'Controller Word',
     &                     'Version Number',
     &                     'Revision Number',
     &                     'Data Validity Word',
     &                     'Mark & Force Pass Mask'/

C
  100 FORMAT(' ', A, T26, ': ', I2)
  109 FORMAT(' ', A, T26, ': ', A)
      DO HEADER_WORD = 1, L15CAL_HEADER_LENGTH
         IF (HEADER_WORD.EQ.1) THEN
            WRITE (LUN, 100) HEADER_LABELS(1),
     &             L15_CRATE_HEADER(1)
         ELSE
            WRITE (LUN, 109) HEADER_LABELS(HEADER_WORD),
     &             PRTRGR_INT_TO_HEX(L15_CRATE_HEADER(HEADER_WORD))
         ENDIF
      END DO
      NLWF = L15_CRATE_HEADER(1)
C----------------------------------------------------------------------
  999 RETURN
      END
