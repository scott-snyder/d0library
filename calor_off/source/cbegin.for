      SUBROUTINE CBEGIN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize ZEBRA, HBOOK4 AND initialize
C-                         CALORIMETER Frame
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-   Calledby: CALOFF
C-
C-   Created  23-JAN-1989   Harrison B. Prosper, John Womersley
C-   Updated  12-JUN-1989   Rajendran Raja   
C-   Updated  21-SEP-1989   Chip Stewart  HBP 
C-   Updated  28-SEP-1989   Harrison B. Prosper  
C-   Move PBDINI into CFRAME_INI
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL OK,CFRAME_INI,CAL_BEGIN
C----------------------------------------------------------------------
C
      CALL INZBRA                       ! INITIALIZE ZEBRA
      CALL INZCOM(2)                    ! Initialize /ZEBCOM/
      CALL INZLNK                       ! Initialize ZLINKA
      CALL INZSTP                       ! Initialize /ZEBSTP/
      CALL INPAWC                       ! Initialize HBOOK4
C
C ****  INITIALIZE FRAMEWORK PARAMETERS (DATA FILE, GEOMETRY, DUMPS, PACKAGES)
C
      OK =  CFRAME_INI ()
      IF ( OK ) THEN
C
C ****  HOOK FOR PACKAGE INITIALIZATION
C
        OK = CAL_BEGIN ()
        IF (.NOT. OK) THEN
          CALL ERRMSG('CALORIMETER','CBEGIN',' ERROR IN CAL_BEGIN ','F')
        ENDIF
      ELSE
        CALL ERRMSG('CALORIMETER','CBEGIN',' ERROR IN CFRAME_INI','F')
      END IF
C    
      RETURN
      END
