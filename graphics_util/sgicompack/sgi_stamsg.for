      SUBROUTINE STAMSG(STRNG,CENTER)
C--------------------------------------------------------------
C- Purpose and Methods : Output string to the current window
C-
C- Input  : STRING [C*]: Character to be output
C-
C- Output : None
C-
C-  Created XX-XXX-XXXX Mike Shupe
C-  Updated 27-JAN-1993 Lupe Howell Removed the bell
C-
C---------------------------------------------------------------
      CHARACTER*(*) STRNG
C
      LOGICAL CENTER
      INTEGER M
C---------------------------------------------------------------
      M=MYL(STRNG)
      IF(M.EQ.0) RETURN
      IF(CENTER) THEN
        TYPE 100,STRNG(1:M)
  100   FORMAT('  <<<<< ',A,' >>>>>')
      ELSE
        TYPE 200,STRNG(1:M)
  200   FORMAT(' ',A)
      ENDIF
      RETURN
      END
