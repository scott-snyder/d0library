      SUBROUTINE NTUPLE_GET(TOP_DIRECTORY,NTUPLE_ID,EVENT,XX,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the ntuple vector for the event EVENT.
C-   If STATUS is non-zero then the previous event was the last.
C-
C-   Inputs  : TOP_DIRECTORY  [C*]
C-             NTUPLE_ID      [I]   Ntuple identifier
C-             EVENT          [I]   Event counter
C-   Outputs : XX(*)          [R]   Event vector
C-             STATUS         [I]   0 ---- OK
C-   Controls:
C-
C-   Created  27-SEP-1991   Harrison B. Prosper
C-   Updated   5-NOV-1991   Harrison B. Prosper  
C-      Use NTUPLE_FILE_SELECT1
C-   Updated   2-DEC-1991   Harrison B. Prosper  
C-    Make compatible with DHDIR 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TOP_DIRECTORY
      INTEGER NTUPLE_ID,EVENT
      REAL    XX(*)
      INTEGER STATUS,NN,IDX
C----------------------------------------------------------------------
      STATUS = 0
      IF ( NTUPLE_ID .LE. 0 ) GOTO 999
      CALL NTUPLE_FILE_SELECT1(TOP_DIRECTORY(1:LEN(TOP_DIRECTORY)),IDX)
      IF ( IDX .LE. 0 ) THEN
        STATUS =-1
        GOTO 999
      ENDIF
      CALL HGN(NTUPLE_ID,NN,EVENT,XX,STATUS)
C
  999 RETURN
      END
