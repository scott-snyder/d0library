      SUBROUTINE PXBUILD_EDIT_MENU(MENU)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build the edit screen menu
C-
C-   Inputs  : MENU [C*]: Name of the menu 
C-   Outputs : None
C-
C-   Created  16-MAY-1991   Lupe Howell
C-   Updated   6-NOV-1991   Lupe Howell  Menu updated 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) MENU
C----------------------------------------------------------------------
      CALL MENNEW(MENU)

      CALL MENADD(MENU,.TRUE.,
     & 'Edit Menu',
     & 'EDIT MENU',
     & ' Lets you add remove menu items')

      CALL MENADD(MENU,.TRUE.,
     & 'Edit Screen Parameters',
     & 'EDIT SCREEN',
     & ' Gives the list of current screens for you to choose from '//
     & 'the one to be edit')

      CALL MENADD(MENU,.TRUE.,
     & 'Merge With a RCP File',
     & 'MERGE',
     & 'Merges the current RCp file with a given one')

      CALL MENADD(MENU,.TRUE.,
     & 'Change RCP File',
     & 'CHANGE',
     & 'Lets you pick a new RCP file to modify')

      CALL MENADD(MENU,.TRUE.,
     & 'Write the current RCP file',
     & 'WRITE RCP',
     & 'Writes the current RCP file to the local area.')

      CALL MENADD(MENU,.TRUE.,
     & 'Write Routines',
     & 'WRITE ROUTINES',
     & 'Writes the EXEC and INIT routines for current RCP file '//
     & 'to the local area.')
  999 RETURN
      END
