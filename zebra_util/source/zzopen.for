      SUBROUTINE ZZOPEN (LUN,FILNAM,ERROR,OPTION)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Open a ZEBRA file for either READ or WRITE
C-                         access and perform a FORTRAN open. 
C-                         
C-
C-   Inputs  : LUN         Logical Unit Number of input stream
C-             FILNAM      File name
C-   Outputs : ERROR       Non-zero if error opening file
C-   Controls: OPTION      'READ' or 'INPUT' 
C-                         'WRITE' or 'OUTPUT'
C-
C-   Created  21-NOV-1988   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL OK
      INTEGER LUN,L,ERROR
      CHARACTER*1   TYPE
      CHARACTER*(*) FILNAM,OPTION
      INCLUDE 'D0$INC:QUEST.INC'
C----------------------------------------------------------------------
      L = LEN(FILNAM)
      TYPE = OPTION(1:1)
      CALL UPCASE (TYPE,TYPE)
C
      IF ( (TYPE .EQ. 'R') .OR. (TYPE .EQ. 'I') ) THEN
        TYPE = 'I'
        CALL D0OPEN (LUN,FILNAM(1:L),'IU',OK)
        IF ( .NOT. OK ) GOTO 900
      ELSEIF ( (TYPE .EQ. 'W') .OR. (TYPE .EQ. 'O') ) THEN
        TYPE = 'O'
        CALL D0OPEN (LUN,FILNAM(1:L),'OU',OK)
        IF ( .NOT. OK ) GOTO 900
      ENDIF
C
      CALL FZFILE (LUN,0,TYPE)
      ERROR = IQUEST(1)
      GOTO 999
C
  900 CONTINUE
      ERROR =-1
  999 RETURN
      END
