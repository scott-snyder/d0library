      SUBROUTINE TRD_CHGHBK(CHAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reselects histograms
C-                         Changes the limits
C-   Inputs  : CHAR: 'ON' online histograms
C-                 : 'OFF' offline histograms
C-   Outputs : 
C-   Controls: 
C-
C-   Created  21-SEP-1990   JFG
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) CHAR
       IF (CHAR.EQ.'ON') THEN
        CALL HCDIR('//PAWC/TRH',' ')
       ELSE IF (CHAR.EQ.'OFF') THEN
        CALL HCDIR('//PAWC/TRD',' ')
       ELSE
        GOTO 999
       ENDIF
      CALL HDELET(0)
      IF (CHAR.EQ.'ON') THEN
       CALL TRDBOK_ON
      ELSE 
       CALL TRDBOK
      ENDIF
  999 RETURN
      END
