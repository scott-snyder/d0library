      SUBROUTINE NTUPLE_SAVE(TOP_DIRECTORY,NTUPLE_ID,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Save the current ntuple buffer to the file 
C-   identified by the "top_directory".
C-
C-   Inputs  : TOP_DIRECTORY [C*]  Top directory in RZ file
C-             NTUPLE_ID     [I]   Ntuple identifier
C-             
C-   Outputs : STATUS       [I]   0 -- OK.
C-   Controls: 
C-
C-   Created  21-NOV-1991   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TOP_DIRECTORY
      INTEGER NTUPLE_ID,STATUS
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:NT_HOUSEKEEP.INC'
C----------------------------------------------------------------------
      INTEGER ICYCLE
      INTEGER IDX
C----------------------------------------------------------------------
C
C ****  Select correct ntuple file
C
      STATUS = 0
      IF ( NTUPLE_ID .LT. 0 ) GOTO 999
      CALL NTUPLE_FILE_SELECT1(TOP_DIRECTORY(1:LEN(TOP_DIRECTORY)),IDX)
      IF ( IDX .LE. 0 ) THEN
        STATUS =-1
        GOTO 999
      ENDIF
C
      CALL HROUT(NTUPLE_ID,ICYCLE,' ')     ! write out the current buffer
C
  999 RETURN
      END
