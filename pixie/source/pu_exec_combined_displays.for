      SUBROUTINE PU_EXEC_COMBINED_DISPLAYS(TITLE,PACKAGE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display a menu of combined-view or multi-view
C-   displays and execute the display selected by the user.
C-
C-   Inputs  : TITLE    [C*]    Title of menu
C-             PACKAGE  [C*]    Name of package (the associated RCP file
C-                              must have the name PX_"package"_RCP)
C-   Outputs : None
C-   Controls: None
C-
C-   Created   6-NOV-1990   Lupe Howell, Harrison B. Prosper
C-
C----------------------------------------------------------------------
      INTEGER IER
      CHARACTER*(*) TITLE
      CHARACTER*(*) PACKAGE
C
      INTEGER LTIT,LPAC
      CHARACTER*40 COMMAND
C----------------------------------------------------------------------
      LTIT = LEN(TITLE)
      LPAC = LEN(PACKAGE)
C
      COMMAND = ' '
      DO WHILE ( COMMAND(1:4) .NE. 'EXIT' )
        CALL PUMENUDO(TITLE(1:LTIT),PACKAGE(1:LPAC),COMMAND)
      ENDDO
C
  999 RETURN
      END
