      SUBROUTINE L1UTIL_REPLACE_RS_KEYWORD(LINE, ORIG_KEY, NEW_KEY, 
     &  WORK, OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Replace the first occurance of ORIG_KEY in LINE with
C-      NEW_KEY. Returns whether then new string can fit within the input
C-      variable.
C-
C-   Inputs  : LINE     The input string. 
C-             ORIG_KEY The string to replace
C-             NEW_KEY  The string to replace ORIG_KEY with
C-
C-   Outputs :  WORK     The modified string.
C-              OK       .FALSE. on error
C-
C-   Controls: none
C-
C-   Created  19-NOV-1991   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
C       This routine is only used with online code, but is distributed as 
C       part of an offline library. To be able to do this, we only generate 
C       useful code on VMS machines.
C&IF VAXVMS
      IMPLICIT NONE
C
      INTEGER  TRULEN
      EXTERNAL TRULEN
C
      CHARACTER*(*) LINE, ORIG_KEY, NEW_KEY, WORK
      LOGICAL OK
C
      INTEGER ORIG_KEY_POS
C
      OK = .TRUE.
      ORIG_KEY_POS = INDEX(LINE, ORIG_KEY)
      IF (ORIG_KEY_POS .EQ. 0) GOTO 999
      IF (TRULEN(LINE) - LEN(ORIG_KEY) + LEN(NEW_KEY) 
     &  .GT. LEN(LINE)) THEN
        OK = .FALSE.
        GOTO 999
      ENDIF
      CALL STR$REPLACE(WORK, LINE, ORIG_KEY_POS, 
     &  ORIG_KEY_POS + LEN(ORIG_KEY) -1, NEW_KEY)
C
C&ELSE
C&      CHARACTER*(*) LINE, ORIG_KEY, NEW_KEY, WORK
C&      LOGICAL OK
C&ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
