      LOGICAL FUNCTION TOP_TOP_SGL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : filter JET_3_MULTI events
C-
C-
C-   Controls: TOP_TOP_SGL_RCP
C-   Returned value  : true for events that pass
C-
C-   Created  16-MAY-1994   Krzysztof L. Genser
C-   Updated   6-SEP-1994   Krzysztof L. Genser   
C-     add all the new and old filter names
C-     
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL MATCH_WILD
      INTEGER IER
      INTEGER J
      LOGICAL FIRST,PASSED
      INTEGER NTRIGON,NFILTON,LEN1,TBIT_ON(32),FBIT_ON(128)
      CHARACTER*32 TNAME_ON(32),FNAME_ON(128)
      CHARACTER*32 SEARCH_STRING1,SEARCH_STRING2,SEARCH_STRING3
      SAVE FIRST
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL INRCP('TOP_TOP_SGL_RCP',IER)
        IF(IER.EQ.0) THEN
          CALL EZPICK('TOP_TOP_SGL_RCP')
        ELSE
          CALL ERRMSG('TOP_TOP_SGL','TOP_TOP_SGL_RCP',
     &      'ERROR READING TOP_TOP_SGL RCP FILE','F')
        ENDIF
      ENDIF
C
C ***   intializations
C
      PASSED    = .FALSE.
C
C ***   get names of triggers/filters fired for this event
C
      CALL GTTSUM(NTRIGON,TBIT_ON,TNAME_ON,NFILTON,FBIT_ON,FNAME_ON)
      SEARCH_STRING1 = 'JET_3_MULTI'
      SEARCH_STRING2 = 'JET_3_HIGH'
      SEARCH_STRING3 = 'JET_3_MAX'
C
      DO 1 J = 1,NFILTON
        PASSED = MATCH_WILD(FNAME_ON(J),SEARCH_STRING1).OR.PASSED
        PASSED = MATCH_WILD(FNAME_ON(J),SEARCH_STRING2).OR.PASSED
        PASSED = MATCH_WILD(FNAME_ON(J),SEARCH_STRING3).OR.PASSED
        IF (PASSED) GOTO 1994
    1 CONTINUE
C
 1994 CONTINUE
      TOP_TOP_SGL = PASSED
C
      RETURN
      END
