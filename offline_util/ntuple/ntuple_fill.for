      SUBROUTINE NTUPLE_FILL(TOP_DIRECTORY,NTUPLE_ID,XTUPLE,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill an Ntuple of given ID
C-
C-   Inputs  : TOP_DIRECTORY  [C*]
C-             NTUPLE_ID      [I]   ID of the Ntuple 
C-             XTUPLE(*)      [R]   Data array
C-
C-   Outputs : STATUS         [I]   Error status; 0 means no error
C-   Controls: none
C-
C-   Created  19-MAY-1991   B.S.Acharya
C-   Updated   5-NOV-1991   Harrison B. Prosper  
C-    Use NTUPLE_FILE_SELECT1
C-   Updated   2-DEC-1991   Harrison B. Prosper  
C-    Make compatible with DHDIR 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) TOP_DIRECTORY
C
      INTEGER NTUPLE_ID
      INTEGER STATUS
      INTEGER IDX
C
      REAL XTUPLE(*)
C----------------------------------------------------------------------
      STATUS = 0
      IF ( NTUPLE_ID .LE. 0 ) GOTO 999
      CALL NTUPLE_FILE_SELECT1(TOP_DIRECTORY(1:LEN(TOP_DIRECTORY)),IDX)
      IF ( IDX .LE. 0 ) THEN
        STATUS =-1
        GOTO 999
      ENDIF
C
C...Fill the NTUPLE ------------>>>>
C
      CALL HFN(NTUPLE_ID,XTUPLE)              
C
  999 RETURN
      END
