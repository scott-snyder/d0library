      integer function ez_get_integer (param, callers_name)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Retrieve the value of PARAM from the currently selected RCP
C-     bank as an integer.  CALLERS_NAME is the name of the calling
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

      integer  ez_get_integer_elt
      external ez_get_integer_elt
C----------------------------------------------------------------------

      ez_get_integer = ez_get_integer_elt (param, 0, callers_name)

  999 RETURN
      END
