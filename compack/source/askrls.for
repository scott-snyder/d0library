C DEC/CMS REPLACEMENT HISTORY, Element ASKRLS.FOR
C *2    11-MAY-1988 10:41:39 HARRY "COMPACK routine to prompt for range of reals"
C *1    22-APR-1988 09:52:26 HARRY "COMPACK routine to prompt for range of REALS"
C DEC/CMS REPLACEMENT HISTORY, Element ASKRLS.FOR
      SUBROUTINE ASKRLS (PRT,FLAG,LIMITS,DEFAUL,STR,NUMBER,BACK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prompt user for first and last value of a 
C-                         set of REALS. If user hits return set
C-                         values to default. Check also that numbers
C-                         are within given limits.
C-
C-   Inputs:   PRT         Prompt
C-             FLAG        For FLAG = "EQUAL" set NUMBER(1) = NUMBER(2)
C-                         if only ONE number has been entered; otherwise
C-                         set NUMBER(1) =-ABS(NUMBER(1)) 
C-                         and NUMBER(2) = ABS(NUMBER(1))
C-             LIMITS(2)   Minimum & maximum bounds (REAL)
C-             DEFAUL(2)   Default minimum & maximum bounds (REAL)
C-
C-   Outputs : STR         Selected number or range packed as a string
C-             NUMBER(2)   First & last value entered (REAL)
C-             BACK        If true go back to upper menu level
C-
C-   Created   4-MAR-1988   Harrison B. Prosper
C-   Modified 15-APR-1988
C-   Updated  18-MAR-2004   sss - compile with g77
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL       OK,BACK,ACTIVE,EQUATE
      REAL          LIMITS(2),DEFAUL(2),NUMBER(2)
      INTEGER       I,J,ERR,N,N1,N2,NP,LEN
      CHARACTER*(*) STR,FLAG
      REAL          VALUE
      CHARACTER*16  RANGE,LIM
      CHARACTER*64  STRING
      CHARACTER*(*) PRT,ERRMSG
      PARAMETER(    ERRMSG = ' %ASKRLS-ERROR-')
C&IF LINUX
C&      character*1024 tmp
C&ENDIF
C----------------------------------------------------------------------
C
C
C ****  Initialize some variables and strings
C
      NP = LEN (PRT)
      EQUATE = (FLAG(1:1) .EQ. 'E') .OR. (FLAG(1:1) .EQ. 'e')
C
      CALL VNUMR (2,LIMITS,'(',',',')',LIM,N)
      IF ( DEFAUL(2) .EQ. DEFAUL(1) ) THEN
        CALL VNUMR (1,DEFAUL(1),'[',' ',']',RANGE,N)
      ELSE
        CALL VNUMR (2,DEFAUL,'[',',',']',RANGE,N)
      ENDIF
C
C ****  Ask for range
C
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )
C&IF LINUX
C&        tmp = prt
C&        CALL GETSTR (tmp(1:NP)//' '//RANGE(1:N)//'> ',STRING,OK,BACK)
C&ELSE
        CALL GETSTR (PRT(1:NP)//' '//RANGE(1:N)//'> ',STRING,OK,BACK)
C&ENDIF
        IF ( BACK ) THEN
          RETURN
        ENDIF
        
        IF ( OK  ) THEN
C ****  Extract first value
          NUMBER(1) = VALUE (STRING,I,J,ERR)
C
C ****  If ERR .EQ. 0 then string contains non-numeric characters
          IF ( ERR .GT. 0 ) THEN
            STRING  = STRING(J+1:)
C
C ****  Extract last value
            NUMBER(2) = VALUE (STRING,I,J,ERR)
            IF ( ERR .EQ. 0 ) THEN
              IF ( EQUATE ) THEN
                NUMBER(2) = NUMBER(1)
              ELSE
                NUMBER(2) = ABS(NUMBER(1))
                NUMBER(1) =-NUMBER(2)
              ENDIF
            ENDIF
C
C ****  Ensure that values are within range
C
            OK = NUMBER(1) .LE. NUMBER(2)
            IF ( OK ) THEN
              OK = (LIMITS(1) .LE. NUMBER(1)) .AND. 
     &             (LIMITS(2) .GE. NUMBER(1)) .AND.
     &             (LIMITS(1) .LE. NUMBER(2)) .AND. 
     &             (LIMITS(2) .GE. NUMBER(2))
              IF ( OK ) THEN
                ACTIVE = .FALSE.
              ELSE
              CALL RELMSG
     &        (0,ERRMSG//'INVALID Value(s); MUST be in range: '//LIM)
              ENDIF
            ELSE
              CALL RELMSG 
     &        (0,ERRMSG//'INVALID Lower bound; MUST be <= upper bound')
            ENDIF
          ELSE
            GOTO 999 ! String contains non-numeric characters
          ENDIF
        ELSE
          NUMBER(1) = DEFAUL(1)
          NUMBER(2) = DEFAUL(2)
          ACTIVE = .FALSE.
        ENDIF
      ENDDO
C
C ****  Create string from NUMBER(1) and NUMBER(2)
C
      NP = LEN(STR)
      IF ( NUMBER(2) .EQ. NUMBER(1) ) THEN
        CALL VNUMR (1,NUMBER(1),'(',' ',')',STR(1:NP),N)
      ELSE
        CALL VNUMR (2,NUMBER,'(',',',')',STR(1:NP),N)
      ENDIF
C
  999 RETURN
      END
