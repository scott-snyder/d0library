      SUBROUTINE PX_DELETE_ACTIONS(RCPFILE,SCREEN,ACTION,
     &  VIEWPORT,COMBINED,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Deletes and action routine name either
C-   form the combined array or a single view. A warning will be displayed
C-   if the action routine name is atempted to be deleted from a single view
C-
C-   Inputs  : RCPFILE   [C*]: RCP file name
C-             SCREEN    [C*]: Screen Name
C-             ACTION    [C*]: Action name to be deleted
C-             VIEWPORT  [I ]: Viewport number
C-             COMBINED  [L ]: Combioned view flag
C-             IER       [I ]: Error flag 0 okay
C-
C-   Outputs : None
C-
C-   Created   7-JUN-1991   Lupe Howell
C-   Updated  20-DEC-1991   Lupe Howell  Deletion of an action of a single view
C-             not permitted 
C-   Updated  24-JAN-1992   Lupe Howell  Update for SGI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) RCPFILE
      CHARACTER*(*) SCREEN
      CHARACTER*(*) ACTION
      INTEGER VIEWPORT,IER
      LOGICAL COMBINED
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:PIXIE.DEF'
      INCLUDE 'D0$INC:PXBUILDCOM.INC'
C----------------------------------------------------------------------
      INTEGER PTR,PARTY(MAXCPAR),COMBINED_ELEMENTS,IMENU,ISUBM,IACTION
      INTEGER ACTNUM
      REAL    PARVAL(MAXCPAR)
      CHARACTER*32 PACKAGE,ACTIONS(MAXCACT),PARNAMES(MAXCPAR)
      CHARACTER*80 STRING
C----------------------------------------------------------------------
      IF ( COMBINED ) THEN
C
C ****  Get the total number of elements in the picked
C ****  port.
C
        PTR = 1
        CALL PX_GET_ACTION
     &    (SCREEN,VIEWPORT,PACKAGE,ACTIONS,ACTNUM,PARNAMES,
     &    PARVAL,PARTY,COMBINED_ELEMENTS,PTR,IER)
C
C
C ****  Remove the number of elements in the combined view
C ****  in PX_*_RCP array
C
        CALL EZ_REMOVE_ELEMENT
     &       (SCREEN,'%PACKAGE',VIEWPORT,COMBINED_ELEMENTS,IER)
C
      ELSE
C
C ****  Get menu, submenu and action indexes
C
        STRING = 'This view has only one action routine'//
     &           ' You CAN NOT delete it'
        CALL ERRMSG(STRING,'W')
      ENDIF

  999 RETURN
      END
