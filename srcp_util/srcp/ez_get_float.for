      real function ez_get_float (param, callers_name)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Retrieve the value of PARAM from the currently selected RCP
C-     bank as an float  CALLERS_NAME is the name of the calling
C-     routine, for error reporting.
C-
C-   Returned value  :
C-     Value of the parameter, as an integer.
C-
C-   Inputs  :
C-     param : Name of the RCP parameter to retrieve.
C-     callers_name : Name of calling routine, for error reporting.
C-
C-   Outputs : 
C-   Controls:
C-   Bugs :
C-     I'm not sure that mixed arrays are handled properly in all cases.
C-
C-   Created  21-FEB-1994   scott snyder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      character*(*) param, callers_name

      real     ez_get_float_elt
      external ez_get_float_elt
C----------------------------------------------------------------------

      ez_get_float = ez_get_float_elt (param, 0, callers_name)
  999 RETURN
      END
