      SUBROUTINE PX_GET_ACTION(ARRAY_NAME,OCURRENCE,PACKAGE,ACTION,
     &  ACTNUM,PARNAMES,PARVAL,PARTY,TOTAL_ELEMENTS,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the nth elements in a combined array.
C-
C-   Inputs  : ARRAY_NAME [C*]: Combine array name
C-             OCURRENCE  [I ]: The nth element of the combined array
C-
C-   Outputs : PACKAGE    [C*]: Name of the package
C-             ACTION  [C*(*)]: Array with action name(s)
C-             ACTNUM     [I ]: number of actions
C-             PARNAMES[C*(*)]: Array containing the element names of the
C-                              requested view
C-             PARVAL   [I(*)]: Array of values of the elements
C-             PARTY    [I(*)]: Array of the types
C-             TOTAL_ELEMENTS : Total number of elements
C-             IER         [I]: 0 If the nth element was found
C-
C-   Created  31-MAY-1991   LUPE HOWELL
C-   Updated  22-OCT-1992   Lupe Howell  Updated  cal to PX_GET_NEXT_ACTION
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) ARRAY_NAME
      CHARACTER*(*) PACKAGE
      CHARACTER*(*) PARNAMES(*)
      CHARACTER*(*) ACTION(*)
      REAL    PARVAL(*)
      INTEGER OCURRENCE,ACTNUM,PARTY(*),TOTAL_ELEMENTS,IER
C
      CHARACTER*32 OLD_ARRAY
      CHARACTER*40 REM
      INTEGER PTR,I,J,AL,COUNT
      LOGICAL GET_NEXT_ACTION
C----------------------------------------------------------------------
      COUNT = 0
      PTR = 1
      GET_NEXT_ACTION = .TRUE.
      CALL WORD(ARRAY_NAME,I,J,AL)
      DO WHILE ( GET_NEXT_ACTION )
        COUNT = COUNT + 1
        CALL PX_GET_NEXT_ACTION(ARRAY_NAME(1:AL),
     &                            PACKAGE,
     &                            ACTION,
     &                            ACTNUM,
     &                            PARNAMES,
     &                            PARVAL,
     &                            PARTY,
     &                            TOTAL_ELEMENTS,
     &                            REM,
     &                            PTR,IER)
C
C ****  Stop the search if the end of the array is reached or
C ****  if the nth view is found
C
        GET_NEXT_ACTION = (IER .EQ. 0 ) .AND. (COUNT .NE. OCURRENCE)
      ENDDO
      IF ( COUNT .EQ. OCURRENCE ) THEN
C
C ****  Incrementing the counter to inclue %PACKAGE and %ACTION(s)
C
        TOTAL_ELEMENTS = TOTAL_ELEMENTS + ACTNUM + 1
        IER = 0
      ELSE
        CALL INTMSG(' PX_GET_ACTION')
        CALL INTMSG(' Could not find the requested ocurrence ')
        IER = -1
      ENDIF
  999 RETURN
      END
