      PROGRAM D0USER
C---------------------------------------------------------------------
C-                                                                   -
C-      General User Program                                         -
C-                                                                   -
C-                       SDP Apr.,1987                               -
C-                                                                   -
C---------------------------------------------------------------------
C
      IMPLICIT NONE
      INTEGER INUNIT,LISUNI
      LOGICAL NOIN,YES
      CHARACTER*8 COMMAND
      CHARACTER*8 STATE
      DATA INUNIT/0/
      DATA NOIN/.TRUE./
C
      CALL INIGEN        ! general initialization
      STATE='SETUP'
C
C        program state subroutines
C
    1 CONTINUE
C
C
        IF(STATE(1:5).EQ.'SETUP') THEN  !  initialize job
          CALL INIJOB(INUNIT,NOIN) 
          STATE='PROCESS'
C
C   
        ELSE IF(STATE(1:7).EQ.'PROCESS') THEN
C
C             process runs and events
C
          CALL PROCES(INUNIT,STATE,NOIN)
C
C
        ELSE IF(STATE(1:6).EQ.'FINISH') THEN
          CALL FINISH
C
        ENDIF
C
      GOTO 1
C
      END                                    
