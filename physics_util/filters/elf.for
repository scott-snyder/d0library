      LOGICAL FUNCTION ELF
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : use for ELF streaming
C-
C-   Returned value  : TRUE if any ELF L2 filter passed
C-   Inputs  :
C-   Outputs :
C-   Controls:  ELF.RCP
C-
C-   Created  22-FEB-1994   Meenakshi Narain
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,J,IER
      INTEGER NTRIGON,NFILTON,LEN1,TBIT_ON(32),FBIT_ON(128)
      INTEGER NFSTRING_REQ,STATUS
      CHARACTER*32 TNAME_ON(32),FNAME_ON(128)
      CHARACTER*32 TSTRING_REQ(32),FSTRING_REQ(128)
      CHARACTER*32 SEARCH_STRING
      LOGICAL OK, PASS_L2, MATCH_WILD,FIRST
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL INRCP('ELF_RCP',IER)
        IF(IER.EQ.0) THEN
          CALL EZ_GET_CHARS('ELF_FILTNAMES',NFSTRING_REQ,FSTRING_REQ,
     &      IER)
          IF (IER.NE.0) THEN
            CALL ERRMSG('ELF','ELF_RCP','ERROR GETTING ELF RCP VALUES',
     &        'W')
          ENDIF
        ELSE
          CALL ERRMSG('ELF','ELF_RCP','ERROR READING ELF RCP FILE','W')
        ENDIF
      ENDIF
C
C ****  SET DEFAULTS
C
      ELF = .FALSE.
      PASS_L2 = .FALSE.
C
C   get names of triggers/filters fired for this event
C
      CALL GTTSUM(NTRIGON,TBIT_ON,TNAME_ON,NFILTON,FBIT_ON,FNAME_ON)
C   check whether the event passes an EM filter
      DO I = 1,NFSTRING_REQ
        SEARCH_STRING = FSTRING_REQ(I)
        DO J = 1,NFILTON
          OK = MATCH_WILD(FNAME_ON(J),SEARCH_STRING)
          PASS_L2 = PASS_L2 .OR. OK
        ENDDO
      ENDDO
C
      IF (.NOT.PASS_L2) GO TO 999
      ELF = PASS_L2
C
  999 RETURN
      END
