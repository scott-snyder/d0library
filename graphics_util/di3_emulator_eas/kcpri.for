      SUBROUTINE KCPRI(PRINAM)
C
C    This subroutine generates a primitive name 4 characters long based
C    on the base 37 numbering system and type of segment open. This
C    routine uses the current primitive number instead of incrementing
C    to the next primitive.
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      CHARACTER*(*) PRINAM
      CHARACTER*3 SEQ

      CALL KBLDN(NPRIM, SEQ)
      IF (SEGNUM .GT. 0) THEN
         PRINAM = 'K'//SEQ
      ELSE
         PRINAM = 'L'//SEQ
      ENDIF
      RETURN
      END
