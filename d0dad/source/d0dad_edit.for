      SUBROUTINE D0DAD_EDIT(FILENAME,COMMAND,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: EDIT (modify) d0dad system data files...
C-
C-   Inputs  : FILENAME - system file to change
C-             COMMAND  - edit command (or file of commands)
C-   Outputs : IERR     - 0 ==> All OK
C-   Controls:
C-
C-   Created  12-JAN-1996   John Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:d0dadcom.inc'
      CHARACTER*(*) FILENAME,COMMAND
      INTEGER  IERR,ITYPE
C-----------------------------------------------------------------------
C
      CALL D0DAD_FTYPE(FILENAME,ITYPE)
      IF( ITYPE.EQ.JFFC ) THEN
        IERR = 0
        CALL FCEDIT(FILENAME,COMMAND,IERR)
      ELSEIF( ITYPE.EQ.JFEC ) THEN
        IERR = 0
        CALL ECEDIT(FILENAME,COMMAND,IERR)
      ELSE
        IERR = -1
      ENDIF
C
  999 RETURN
      END
