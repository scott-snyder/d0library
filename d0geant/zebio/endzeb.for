      SUBROUTINE ENDZEB
C-----------------------------------------------------------------------
C-    Close input and output files and write EOF mark on the output.
C-
C-    INPUT(A):  (none)
C-    INPUT(C):  input and output logical number from /ZEBIO/
C-
C-    OUTPUT:    (none)
C-
C-    S.Kunori    Mar.,1986
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBIO.INC'
C
      IF(IRDUNI.NE.0) THEN
         CALL FZENDI(IRDUNI,'T')
      ENDIF
      IF(IWRUNI.NE.0) THEN
         CALL FZENDO(IWRUNI,'T')
      ENDIF
C
      RETURN
      END
