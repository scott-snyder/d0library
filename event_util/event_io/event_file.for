      SUBROUTINE EVENT_FILE(FILNAM,N)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-          construct name of file for ONE event 
C-          extension should be added by user
C-
C-   Outputs:
C-      FILNAM = name of file (E_run#_id#)
C-           N = number of characters in name
C-
C-   Created  22-JAN-1991   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) FILNAM
      CHARACTER*20 TEMP
      INTEGER RUN,ID,N
C----------------------------------------------------------------------
C
      CALL EVNTID(RUN,ID)
      CALL STRINT('E_',RUN,TEMP,N)
      CALL STRINT(TEMP(1:N)//'_',ID,TEMP,N)
      FILNAM=TEMP(1:N)
C
  999 RETURN
      END
