      subroutine ez_get_string (param, val, vallen, callers_name)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Retrieve the value of PARAM from the currently selected RCP
C-     bank as a string.  CALLERS_NAME is the name of the calling
C-     routine, for error reporting.
C-
C-   Inputs  : 
C-     param : Name of the RCP parameter to retrieve.
C-     callers_name : Name of calling routine, for error reporting.
C-
C-   Outputs :
C-     val : The retrieved string.
C-     vallen : Its length.
C-
C-   Controls: 
C-   Bugs :
C-     I'm not sure that mixed arrays are handled properly in all cases.
C-
C-   Created  21-FEB-1994   scott snyder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      character*(*) param, val, callers_name
      integer vallen
C----------------------------------------------------------------------

      call ez_get_string_elt (param, 0, val, vallen, callers_name)
  999 RETURN
      END
