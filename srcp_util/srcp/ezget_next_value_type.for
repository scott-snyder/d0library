      SUBROUTINE EZGET_NEXT_VALUE_TYPE(PARAM,VAL,CVAL,TYPE,LVAL,IER,PTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the next value and its type in array
C-   PARAM. Set PTR to 1 to get FIRST value. PTR will be updated upon
C-   exit.
C-
C-   Inputs  : PARAM    [C*]    Name of parameter (32-characters maximum).
C-
C-   Outputs : VAL              Value; can be returned as INTEGER, REAL
C-                              or LOGICAL. Use equivalences to convert
C-                              between INTEGER, REAL and LOGICAL.
C-
C-             CVAL     [C*]    Character value.
C-
C-             TYPE     [I]     Value type.
C-
C-                              Type            Type id
C-
C-                              INTEGER         1
C-                              REAL            2
C-                              REAL (E-format) 3
C-                              LOGICAL         4
C-                              CHARACTER       10 + n (n=string length)
C-
C-             LVAL     [I]     1 for INTEGER, REAL or LOGICAL; Length
C-                              of string for CHARACTER value.
C-
C-             IER      [I]     Status code
C-                               0 --> OK
C-                               1 --> LAST value
C-                               
C-   Controls: PTR      [I]     1 -- Return 1st value. Updated upon
C-                              exit.
C-
C-   Created  29-MAY-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) PARAM
      INTEGER       VAL
      CHARACTER*(*) CVAL
      INTEGER       TYPE
      INTEGER       LVAL
      INTEGER       IER
      INTEGER       PTR
C
      INTEGER MAXWORDS
      PARAMETER( MAXWORDS = 21 )
      INTEGER IVAL(MAXWORDS),ITYPE(MAXWORDS)
      INTEGER I,J,K,L,ID,NVAL,TOTAL,NWORD
C
      CHARACTER*32 LAST_PARAM
      LOGICAL END_OF_ARRAY
C
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      SAVE ID,LAST_PARAM,FIRST
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        LAST_PARAM = ' '
      ENDIF
C
C ****  Get parameter ID
C
      L = LEN(PARAM)
      I = INDEX(PARAM(1:L),' ')
      IF ( I .GT. 0 ) THEN
        L = I - 1
      ENDIF
C
      IF ( PARAM(1:L) .NE. LAST_PARAM(1:L) .OR. PTR .LT. 2 ) THEN
        CALL EZGETI (PARAM(1:L),ID,IER)
        IF ( IER .NE. EZS_SUCCESS ) GOTO 999
        LAST_PARAM = PARAM(1:L)         ! Remember parameter name
        TOTAL = 0
        PTR   = 1                       ! Get first value
      ENDIF
C
      END_OF_ARRAY = (TOTAL .GT. 0) .AND. (PTR .GT. TOTAL)
      IF ( END_OF_ARRAY ) THEN
        IER = 1
        GOTO 999
      ENDIF
C
      CALL EZGET2 (ID,PTR,PTR,1,VAL,TYPE,NVAL,TOTAL,IER)
      IF ( TYPE .GT. VTCHR ) THEN       ! Check for character type
        L = TYPE - VTCHR                ! Get string length
        NWORD = (L+3)/4                 ! NUMBER OF WORDS
        CALL EZGET2 (ID,PTR,PTR+NWORD-1,1,IVAL,ITYPE,NVAL,TOTAL,IER)
        CALL DHTOC (L,IVAL,CVAL(1:LEN(CVAL)))
        PTR = PTR + NWORD               ! Point to next value
        LVAL= L
      ELSE
        PTR = PTR + 1
        LVAL= 1
      ENDIF
C
C ****  Check for end-of-array
C
      END_OF_ARRAY = PTR .GT. TOTAL
      IF ( END_OF_ARRAY ) IER = 1
  999 RETURN
      END
