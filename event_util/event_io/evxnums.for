      SUBROUTINE EVXNUMS(IEVN1,IEVN2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return INPUT_EVENT Number ( beam crossing
C-                         number, from Level_1. )
C-
C-   Outputs : IEVN1 - lower 8 digits( I8 )
C-             IEVN2 - upper 8 digits( I8 )
C-   Controls:
C-
C-   Created  19-APR-1991   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IEVN1,IEVN2
C----------------------------------------------------------------------
      IEVN1=0
      IEVN2=0
C-
      IF(LHEAD.NE.0) THEN
        IEVN1=IQ(LHEAD+7)
        IEVN2=IQ(LHEAD+8)
      ENDIF
C-
  999 RETURN
      END
