      SUBROUTINE PU_CHECKMENU(COMMAND,MENU)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check if the command entered is not in the
C-   screen array, that is, is not associated with a view. The command
C-   entered could be, for example, a submenu; in this case MENU
C-   will be set TRUE.
C-
C-   Inputs  : COMMAND [C*]: Command entered
C-
C-   Outputs : MENU    [L ]: flag that indicates menu item type
C-   Controls:
C-
C-   Created  16-NOV-1990   Lupe Howell
C-   Updated  18-JAN-1991   Lupe Howell  The bank_name is use to check if
C-                          the calls to get array information should be done.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) COMMAND
      LOGICAL MENU
C
      INTEGER II,I,J,LCOMM
C----------------------------------------------------------------------
      CALL SWORDS(COMMAND,II,J,LCOMM)
C
C ****  Check if the command has the submenu marker "$" at 
c ****  the end otherwise it is not a sub menu
C
      IF ( COMMAND(J:J) .EQ. '$' ) THEN
        MENU = .TRUE.
      ELSE
        MENU = .FALSE.
      ENDIF
  999 RETURN
      END
