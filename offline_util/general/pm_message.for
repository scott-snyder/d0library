      SUBROUTINE PM_MESSAGE(MESSAGE,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Used for creating a file for communication
C-       with the Production Manager. The method consists of opening,
C-       writing to the file and then closing it as follows:
C-
C-             CALL PM_OPEN(OK)
C-             CALL PM_MESSAGE('Message up to 80 characters long')
C-                :
C-                :
C-             CALL PM_CLOSE
C-
C-   Inputs  : MESSAGE up to 80 characters long.
C-   Outputs : OK is .TRUE. if there are no problems
C-   Controls: none
C-
C-   Created   7-NOV-1991   Lee Lueking
C-   Modified  5-JUN-1992   Lee Lueking. Only one output file PM_STATUS_1
C-                          Changed bug CALL CLOSE(LUN) to CLOSE(LUN)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER LUN,ERR
      CHARACTER FILENAME*11,MESSAGE*(*)
      LOGICAL FIRST_FILE,OK
      SAVE LUN
C----------------------------------------------------------------------
C
C Write a message
C
      OK=.TRUE.
      WRITE(LUN,'( A80)')MESSAGE
      RETURN
C
C Open a status file
C
      ENTRY PM_OPEN(OK)
C
C Select one the output file
C
        FILENAME='PM_STATUS_1'
C
C Open the selected file
C
      OK=.TRUE.
      CALL GTUNIT(200,LUN,ERR)
      IF(ERR.NE.0)THEN
        OK=.FALSE.
        RETURN
      ENDIF
      CALL D0OPEN (LUN,FILENAME,'OF',OK)
      RETURN
C
C Close the selected file
C
      ENTRY PM_CLOSE
C
      CLOSE(LUN)
      CALL RLUNIT(200,LUN,ERR)
      RETURN
      END
