      LOGICAL FUNCTION MGEH_FLG( IFLG )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : check and modify WAMUS geoemetry 
C-                         smearing word. 
C-
C-   Returned value  : Smeared .TRUE. except when it cannot find MGEH.
C-                     not smeared .FALSE.
C-   Inputs  : IFLG  0 - check only
C-                   1 - change to smeared
C-                   2 - change to not-smeared
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  10-MAR-1993   Atsushi Taketani
C-   Modified 13-MAY-1993   HTD
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  IFLG
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER  LM, GZMGEH
C----------------------------------------------------------------------
      LM = GZMGEH(1)

C
      IF(LM.EQ.0) THEN                !Cannot find MGEH
        MGEH_FLG = .TRUE.
        GOTO 999
      ELSE
        MGEH_FLG = .FALSE.
      ENDIF

      IF ( IFLG.EQ.0 ) THEN
        MGEH_FLG= IC(LM+2).EQ.1
      ELSE IF ( IFLG.EQ.1 ) THEN
        IC(LM+2) = 1
      ELSE IF ( IFLG.EQ.2 ) THEN
        IC(LM+2) = 0
      END IF
C
  999 RETURN
      END
