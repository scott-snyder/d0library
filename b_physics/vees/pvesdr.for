      SUBROUTINE PVESDR(NPVES)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : drop PVES bank if no good hypothesis
C-
C-   Inputs  :    NPVES  = Vee ID number
C-   Controls:
C-
C-   Created 12-DEC-1991   Daria Zieminska   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER NPVES,LPVES,GZPVES,LVERH,GZVERH,LVERT,LPARH,GZPARH
      LVERH=GZVERH()
      IQ(LVERH+3)=IQ(LVERH+3)-1
      LPARH=GZPARH()
      IQ(LPARH+6)=IQ(LPARH+6)-1
      LPVES = GZPVES(NPVES)
      LVERT   = LQ(LPVES-2)
      CALL MZDROP(IXCOM,LVERT,' ')
      LPVES = GZPVES(NPVES)
      CALL MZDROP(IXCOM,LPVES,' ')
  999 RETURN
      END
