      SUBROUTINE PRMUSR(PRUNIT,LUSERI,NUSER,CFL,IFL)
C------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-  Print out for event out.  This is a routine called from 
C-  S/R MURECO_DUMP after all the standard banks are printed out,
C-  if a switch 'MUSR' is requested in MURECO.RCP.   By adding
C-  additonal write statements or calling extra PRxxxx routines
C   user can add print out into event dump files.
C-  
C-
C-  INPUT:
C-       ( All input parameters except PRUNIT are dummy)
C-  PRUNIT= unit number for printout
C-  LUSERI= bank address (dummy)
C-  NUSER = bank number  (dummy)
C-  CFL   = flag to control printout   (dummy)
C-          'ALL' for all banks, 'LINEAR' for one linear structure
C-          'ONE' for one bank only
C-          LUSERI must be provided for 'LINEAR',
C-          LUSERI or NUSER may be provided for 'ONE',
C-          LUSERI and NUSER ignored for 'ALL'          
C-  IFL   = 0  print everything   (dummy)
C-          1  print partially (not yet implemented) 
C-
C-   Created  11-JUN-1990   Shuichi Kunori 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C     -- variables in input arguments...
      INTEGER PRUNIT,LUSERI,NUSER,IFL
      CHARACTER CFL*(*)
C     -- local variables...
C----------------------------------------------------------------------
C
C     ========= following are just example ========
C
C      WRITE(PRUNIT,*)
C      WRITE(PRUNIT,*)  '======== ADDITIOANL STUFF IN S/R PRMUSR =====' 
C      WRITE(PRUNIT,*)
C
C      -- print out ISAL bank...
C
C      CALL PRISAL(PRUNIT,0,0,'ALL',0)
C
  999 RETURN
      END
