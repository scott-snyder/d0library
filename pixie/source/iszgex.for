      LOGICAL FUNCTION ISZGEX( )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Call subroutine whose name is given in EXENAM
C-
C-   Inputs  : 
C-   Outputs : .TRUE. if routine known...
C-   Controls:
C-
C-   Created  11-JUL-1988   Olivier Callot
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PXUSER.INC'
C----------------------------------------------------------------------
      ISZGEX = .TRUE.
      IF ( EXENAM .EQ. 'ISZ2D' ) THEN
        CALL ISZ2D
      ELSEIF (EXENAM .EQ. 'ISZ3D') THEN
        CALL ISZ3D
      ELSE
        ISZGEX = .FALSE.
      ENDIF
  999 RETURN
      END
