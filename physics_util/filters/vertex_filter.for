      LOGICAL FUNCTION VERTEX_FILTER()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Test if object and track vertices are the same
C-
C-   Returned value  : True if Object and Track vertex are *NOT* same
C-
C-   Created  20-NOV-1995   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER VERSION
      INTEGER LVERH,GZVERH
C----------------------------------------------------------------------
      VERTEX_FILTER = .FALSE.
C
      LVERH = GZVERH()
      IF (LVERH.LE.0) THEN
        CALL ERRMSG('No VERH bank','VERTEX_FILTER',
     &              'VERH bank not found','W')
        GO TO 999
      ENDIF
C
      VERSION = IQ(LVERH+1)
      IF (VERSION.LT.3) THEN
        CALL ERRMSG('Bad VERH version','VERTEX_FILTER',
     &              'Old VERH version number detected','W')
        GO TO 999
      ENDIF
C
      IF (IBITS(IQ(LVERH),0,1).EQ.1) VERTEX_FILTER = .TRUE.
C
  999 RETURN
      END
