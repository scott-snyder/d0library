      SUBROUTINE ANDOR_TERM_PACK(ANDOR_WORDS,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : EXTRACT AND/OR TERMS FROM TRGR BANK, REPACKAGE
C-                         FOR DST SUMMARY
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls:
C-
C-   Created  18-MAY-1993   Amber S. Boehnlein
C-   All 256 AND/OR terms are copied, see TRIGGER.RES for decoding
C-   IER = -1 for no TRGR bank
C-
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER L1WORDS
      PARAMETER (L1WORDS=8)
      INTEGER ANDOR_WORDS(L1WORDS)
      INTEGER LTRGR_LEVEL1,GZTRGR
      INTEGER GZFIND_CRATE
      INTEGER ANDOR_TERM_INDEX
      LOGICAL ACTUAL_STATE
      INTEGER IWORD,IBIT
      INTEGER IER
C----------------------------------------------------------------------
      CALL VZERO(ANDOR_WORDS,L1WORDS)
      LTRGR_LEVEL1 = GZFIND_CRATE( 'TRGR', GZTRGR(), 11 )
      IF (LTRGR_LEVEL1.GT.0) THEN
        DO IWORD= 1,8
          DO IBIT = 0,31
            ANDOR_TERM_INDEX = 32*(IWORD-1)+IBIT
            CALL L1EXTRACT_ANDOR_TERM(IQ(LTRGR_LEVEL1),
     &                                ANDOR_TERM_INDEX,
     &                                ACTUAL_STATE)
C<<
            IF (ACTUAL_STATE) ANDOR_WORDS(IWORD) =
     &        IBSET(ANDOR_WORDS(IWORD),IBIT)
          ENDDO
        ENDDO
      IER = 0
      ELSE
      IER = -1
      ENDIF
  999 RETURN
      END
