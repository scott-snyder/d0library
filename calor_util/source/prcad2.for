      SUBROUTINE PRCAD2(PRUNIT,LCAD2,NCAD2,CFL,IFL) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print out for CAD2 bank
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
      INTEGER PRUNIT,IFL,LCAD2,NCAD2
      CHARACTER CFL*(*)
C----------------------------------------------------------------------
      CALL PRCAD(PRUNIT,2,NCAD2,CFL,IFL) 
  999 RETURN
      END
