C DEC/CMS REPLACEMENT HISTORY, Element IOSERR.FOR
C *1    11-MAY-1988 10:56:40 HARRY "Function to test ZEBRA error code"
C DEC/CMS REPLACEMENT HISTORY, Element IOSERR.FOR
      LOGICAL FUNCTION IOSERR (IOS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Test error number IOS. Useful for aborting
C-                         from a dispatch loop on occurrence of an error
C-
C-   Inputs : IOS      Error number (usually IQUEST(1) )
C-
C-   Outputs: True if IOS .LT.0 .OR. IOS .GE. 4
C-
C-   Created  17-FEB-1988   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER IOS
      IOSERR = IOS .LT. 0 .OR. IOS .GE. 4
 999  RETURN
      END
