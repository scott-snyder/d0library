      LOGICAL FUNCTION TOP_EJBKG
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : stream e+jets background events
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  31-OCT-1995   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL  STREAM_EJETS,SELECT_EJETS_BKG,SELECT_GJETS_BKG
      EXTERNAL STREAM_EJETS,SELECT_EJETS_BKG,SELECT_GJETS_BKG
      LOGICAL OK,PASSED
C----------------------------------------------------------------------
      TOP_EJBKG = .FALSE.
      PASSED = .FALSE.
C
C ****  ejets filter, first setup by calling stream_ejets
C
      OK = STREAM_EJETS()
      IF (.NOT.OK) GOTO 999
      IF(SELECT_EJETS_BKG() .OR. SELECT_GJETS_BKG() )THEN
        PASSED = .TRUE.
      ENDIF

      TOP_EJBKG = PASSED
  999 RETURN
      END
