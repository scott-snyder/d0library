      SUBROUTINE KFRAM
C
C   Routine to erase all non-retained segments and their associated 
C   primitives.
C
      IMPLICIT NONE
      EXTERNAL ERRHND
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      CHARACTER*4  TSEG
      INTEGER I

      CALL KCLAS
      IF (NTSEG .GT. 0) THEN
         DO 10 I=1,NTSEG
            CALL KBLDN(I, TSEG)
            TSEG = 'T'//TSEG(1:3)
C
C   Remove the temporary segment from the display list.
C
            CALL PREMFR(TSEG,EMDISP,ERRHND)
   10    CONTINUE
C
C   Delete the primitives associated with the temporary segments.
C
         CALL PDELW('L', ERRHND)
C
C   Delete the temporary segments.
C
         CALL PDELW('T',ERRHND)
C
C   Remove all entries of temporary segments from internal structure.
C
         NTSEG = 0
      ENDIF
      RETURN
      END
