      LOGICAL FUNCTION MENMUO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-            Menu 10  MUO                                          
C-                                                                 
C-            Called  by ====> LUIGET                              
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   1-May-1987   Steve Linn
C-   Updated   5-JUN-1989   Harrison B. Prosper  
C-   Made into program-builder interface function
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0COM.INC'        ! Selected MENU id and command
C----------------------------------------------------------------------
      MENMUO = .TRUE.
      IF ( NMENU .NE. IDMUO ) GOTO 999
C
  999 RETURN
      END
