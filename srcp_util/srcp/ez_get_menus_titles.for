      SUBROUTINE EZ_GET_MENUS_TITLES(MENU,TITLE,NUMBER_MENUS,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the menu names (plus titles) from the
C-   array COMPACK_MENUS.
C-
C-   Inputs  : None
C-   Outputs : MENU(*)          [C*]    Array of menu names
C-             TITLE(*)         [C*]    Array of menu titles
C-             NUMBER_MENUS     [I]     Number of menu names
C-             IER              [I]     0 --- OK
C-   Controls:
C-
C-   Created  25-JUN-1991   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) MENU(*), TITLE(*)
      INTEGER NUMBER_MENUS, IER
C----------------------------------------------------------------------
      INTEGER NUMBER, I, J
      INTEGER MAXREC
      PARAMETER( MAXREC = 60 )
      LOGICAL EZERROR, GET_TITLE
      CHARACTER*132 RECORD(MAXREC)
C----------------------------------------------------------------------
C
C ****  Get Menu list
C
      CALL EZ_GET_CHARS ('COMPACK_MENUS',NUMBER,RECORD,IER)
      IF ( EZERROR(IER) ) THEN
        GOTO 999
      ENDIF
C
C ****  Decode into menu names and titles
C
      NUMBER_MENUS = 0
      J = 1
      MENU(J) = RECORD(1)
      TITLE(J)= MENU(J)
      GET_TITLE = .TRUE.
C
      DO I =  2,NUMBER
        IF ( GET_TITLE ) THEN
          GET_TITLE= .FALSE.
          IF ( RECORD(I)(1:1) .EQ. '%' ) THEN
            TITLE(J) = RECORD(I)(2:)
          ELSE
            J = J + 1
            MENU(J) = RECORD(I)
            TITLE(J)= MENU(J)
          ENDIF
        ELSE
          GET_TITLE = .TRUE.
          J = J + 1
          MENU(J) = RECORD(I)
          TITLE(J)= MENU(J)
        ENDIF
      ENDDO
      NUMBER_MENUS = J
  999 RETURN
      END
