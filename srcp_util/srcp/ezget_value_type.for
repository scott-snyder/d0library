      SUBROUTINE EZGET_VALUE_TYPE (PARAM,VAL,TYPE,NVAL,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return values associated with identifier
C-   PARAM in array VAL(*). For each value return the type in array
C-   TYPE(*). Use EZSET_VALUE_TYPE to set the values and types.
C-
C-   Inputs  : PARAM    [C*]    Name of parameter (32-characters maximum).
C-
C-   Outputs : VAL(*)           Values; can be returned as INTEGER, REAL
C-                              or LOGICAL. If the values are of mixed
C-                              type use equivalences between INTEGER,
C-                              REAL and LOGICAL to get the correct
C-                              data conversion and use DHTOC (or UHTOC)
C-                              to convert to characters.
C-
C-             TYPE(*)  [I]     Value type.
C-
C-                              Type            Type id
C-
C-                              INTEGER         1
C-                              REAL            2
C-                              REAL (E-format) 3
C-                              LOGICAL         4
C-                              CHARACTER       10 + n (n=string length)
C-
C-             NVAL     [I]     Number of values returned
C-
C-             IER      [I]     Status code
C-                               0 --> OK
C-                               See EZGET_ERROR_TEXT(IER,STRING)
C-   Controls:
C-
C-   Created   9-NOV-1989   Harrison B. Prosper
C-   Updated  23-FEB-1990   Harrison B. Prosper
C-      Corrected error handling
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) PARAM
      INTEGER       VAL(*)
      INTEGER       TYPE(*)
      INTEGER       NVAL
      INTEGER       IER
C
      LOGICAL GET
      CHARACTER*32  PARAM1
      INTEGER KSTART,KEND,KSTEP,LENGIN,TOTAL
      INTEGER I,J,K,L,ID
C
      INCLUDE 'D0$PARAMS:SRCP.DEF'
C
C----------------------------------------------------------------------
      GET = .TRUE.
      GOTO 5
C
C ****  Entry point for setting routine
C
      ENTRY EZSET_VALUE_TYPE (PARAM,VAL,TYPE,NVAL,IER)
      GET = .FALSE.
    5 CONTINUE
C
C ****  Decode parameter name
C
      L = LEN(PARAM)
      CALL EZZDCD (PARAM(1:L),PARAM1,LENGIN,KSTART,KEND,KSTEP)
C
C ****  Get parameter ID
C
      CALL EZGETI (PARAM1,ID,IER)
      IF ( IER .EQ. EZS_SUCCESS ) THEN
C
C ****  Get/Set parameter
C
        IF ( GET ) THEN
          CALL EZGET2 (ID,KSTART,KEND,KSTEP,VAL,TYPE,NVAL,TOTAL,IER)
        ELSE
          CALL EZSET2 (ID,KSTART,KEND,KSTEP,VAL,TYPE,NVAL,TOTAL,IER)
        ENDIF
      ENDIF
C
  999 RETURN
      END
