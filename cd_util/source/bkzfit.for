      SUBROUTINE BKZFIT(LZTRK,LZFIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : book ZFIT bank for global fitting on a 
C-                         central track ZTRK
C-
C-   Inputs  : LZTRK: bank address of the ZTRK
C-   Outputs : LZFIT: bank address of the ZFIT
C-
C-   Created  21-MAR-1990   Qizhong Li-Demarteau
C-   Updated   2-APR-1992   Qizhong Li-Demarteau  added two more words 
C-                                          for # of degree of freedom
C-   Updated   8-JUN-1992   Qizhong Li-Demarteau  added two more words 
C-                                          for VTX's dE/dx
C-   Updated  21-SEP-1992   Qizhong Li-Demarteau  added words 32-34
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZZFIT.LINK'
      INTEGER LZFIT, LZTRK, IZTRK, GZZTRK
      INTEGER MPZFIT(5)
      LOGICAL FIRST
C
      SAVE FIRST
      DATA MPZFIT/4HZFIT,0,0,34,0/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (LZTRK .LE. 0) RETURN
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL MZFORM('ZFIT','1I 1B 5I 20F 2I 5F',MPZFIT(5))
      ENDIF
C
C ****  Book ZFIT bank
C
      CALL MZLIFT(IXMAIN, LZFIT, LZTRK, -IZZFIT, MPZFIT, 0)
C        
      IQ(LZFIT + 1) = 0               ! version number
C
  999 RETURN
      END

