      PROGRAM CAWSTP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Run CAL_PREGEO and CAL_POSTGEO to build
C-                         CAL_STPFILE and LV0_STPFILE.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  10-FEB-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
C
C ****  Initialize ZEBRA
C
      CALL MZEBRA (0)
C
C ****  Initialize STP structure        
C
      CALL INZSTP                       ! ..and construct STP headers
C
      CALL CAL_PREGEO
C
C ****  Clear /ZEBSTP/
C
      CALL MZWIPE (IDVSTP)
      CALL CONSTP                       ! Construct STP headers
C
      CALL CAL_POSTGEO
C
      END
