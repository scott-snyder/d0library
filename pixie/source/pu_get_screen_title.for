      SUBROUTINE PU_GET_SCREEN_TITLE(IDX,TITLE,LENGTH,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the title for the screen specified
C-   by the index IDX.
C-
C-   Inputs  : IDX      [I]     Screen index
C-   Outputs : TITLE    [C*]    Screen title
C-             LENGTH   [I]     Length of title
C-             IER      [I]     0 -- ok
C-   Controls:
C-
C-   Created  21-JUN-1991   Harrison B. Prosper
C-   Modified 29-JUN-1991   Nobuaki Oshima ( fix bugs on LEN and VAL )
C-   Updated   2-DEC-1991   Lupe Howell  Adding Entry PU_GET_SCREEN_ACTION 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDX
      CHARACTER*(*) TITLE
      INTEGER LENGTH
      INTEGER IER
C----------------------------------------------------------------------
      INTEGER VAL(20),TYP,I,J,L,NLEN
      CHARACTER*80 NAME
      LOGICAL ACTION
C----------------------------------------------------------------------
      ACTION = .FALSE.
      GOTO 10
C
C
      ENTRY PU_GET_SCREEN_ACTION(IDX,TITLE,LENGTH,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the action routine name for the screen
C-   specified by the index IDX.
C-
C-   Inputs  : IDX      [I]     Screen index
C-   Outputs : TITLE   [C*]     Screen Action routine name 
C-             LENGTH   [I]     Length of routine name
C-             IER      [I]     0 -- ok
C-
C-   Created   2-DEC-1991   Lupe Howell
C-
C----------------------------------------------------------------------
      ACTION = .TRUE. 

   10 CONTINUE
C
C ****  Setting the name of the element to be search 
C ****  according what was requested
C
      IF ( ACTION ) THEN
        NAME = 'ACTION'
        NLEN = 6
      ELSE
        NAME = 'NAME'
        NLEN = 4
      ENDIF
      CALL EZ_GET_ELEMENT('PXSCREEN',NAME(1:NLEN),IDX,1,VAL,TYP,IER)
      IF ( ACTION ) THEN
        CALL EZ_GET_ELEMENT_CHARACTER( TITLE )
      ELSE
        CALL EZ_GET_ELEMENT_REMARK( TITLE )
      ENDIF
        L = LEN( TITLE )
        CALL SWORDS(TITLE(1:L),I,LENGTH,J)
  999 RETURN
      END
