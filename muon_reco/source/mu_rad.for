C DEC/CMS REPLACEMENT HISTORY, Element MU_RAD.FOR
C *1    20-AUG-1991 11:30:23 KLATCHKO "no. of radiation lengts from PMUO"
C DEC/CMS REPLACEMENT HISTORY, Element MU_RAD.FOR
      FUNCTION MU_RAD(ITRAK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :UNPACKnumber of radiation lengths from muon bank 
C-
C-   Returned value  :number of radiation lengths from muon bank 
C-   Inputs  : ITRAK - TRAK NUMBER
C-   Outputs : MU_RAD
C-   Controls: 
C-
C-   Created   6-AUG-1991   A.Klatchko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL MU_RAD
      INTEGER ITRAK
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER GZMUON,LMUON
C----------------------------------------------------------------------
      MU_RAD =0.0
      LMUON=GZMUON(ITRAK)               
      IF(LMUON.LE.0)GOTO 999
      MU_RAD = Q(LMUON+19)  !number of radiation lengths in CAL
  999 RETURN
      END
