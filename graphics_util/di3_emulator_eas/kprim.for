      SUBROUTINE KPRIM(PRINAM)
C
C    This subroutine generates a primitive name 4 characters long based 
C    on the base 37 numbering system and type of segment open.
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      CHARACTER*(*) PRINAM
      CHARACTER*3 SEQ

      NPRIM = NPRIM + 1
      CALL KBLDN(NPRIM, SEQ)
      IF (SEGNUM .GT. 0) THEN
         PRINAM = 'K'//SEQ
      ELSE
         PRINAM = 'L'//SEQ
      ENDIF
      RETURN
      END
