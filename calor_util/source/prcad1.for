      SUBROUTINE PRCAD1(PRUNIT,LCAD1,NCAD1,CFL,IFL) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print out for CAD1 bank
C-
C-   Inputs  :   PRUNIT= unit number for printout            
C-               LCAEPI= bank address                        
C-               NCAEPI= bank number   (NOT USED)
C-   Outputs :  NONE
C-   Controls:   CFL   = flag to control printout   (NOT USED)          
C-               IFL   = flag to control printout              
C-                     = 0 RAW HEX DUMP
C-                     = 1 print full title for each NON ZERO ADC word
C-
C-   Created  12-MAY-1989   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PRUNIT,IFL,LCAD1,NCAD1
      CHARACTER CFL*(*)
C----------------------------------------------------------------------
      CALL PRCAD(PRUNIT,1,NCAD1,CFL,IFL) 
  999 RETURN
      END
