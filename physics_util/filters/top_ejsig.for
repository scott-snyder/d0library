      LOGICAL FUNCTION TOP_EJSIG
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : stream e+jets events
C-
C-   Returned value  : TRUE or FALSE
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  31-OCT-1995   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      LOGICAL  STREAM_EJETS,SELECT_EJETS,SELECT_GJETS
      EXTERNAL STREAM_EJETS,SELECT_EJETS,SELECT_GJETS
      LOGICAL OK,PASSED
C----------------------------------------------------------------------
      TOP_EJSIG = .FALSE.
      PASSED = .FALSE.
C
C ****  ejets filter, first setup by calling stream_ejets
C
      OK = STREAM_EJETS()
      IF (.NOT.OK) GOTO 999
      IF( SELECT_EJETS() .OR. SELECT_GJETS() )THEN
        PASSED = .TRUE.
      ENDIF

      TOP_EJSIG = PASSED
  999 RETURN
      END
