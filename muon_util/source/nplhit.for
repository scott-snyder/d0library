C VAX/DEC CMS REPLACEMENT HISTORY, Element NPLHIT.FOR
C *1    21-OCT-1993 08:55:03 FORTNER "add terms for scintillator"
C VAX/DEC CMS REPLACEMENT HISTORY, Element NPLHIT.FOR
      INTEGER FUNCTION NPLHIT(NMOD)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Finds number of hits in one plane
C-
C-    Input  :  NMOD   - Module ID
C-
C-    Output :  NPLHIT   - Number of planes with processed hits
C-
C-    Created :  DH 12-17-85
C-
C-    DH 7/89 NEW MUOF DEFINITION
C-    MF 10/93 NEW MUOF DEFINITION
C-----------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMOD,NHIT,JHIT,I
      INTEGER NWR,NWP,NWM,NSR,NSP,NSM,NP,NF,NSC,NV,LPMUOF(460)
      INTEGER IMOD,MMOD,NRW,IH,NCM,ICM,IPR,IPP,LMUD1
C
      NPLHIT = 0
      CALL GTMUHT(NWR,NWP,NWM,NSR,NSP,NSM,NP,NF,NSC,NV,LPMUOF)
      IMOD = LPMUOF(NMOD)
      CALL GTMUOF(IMOD,MMOD,NRW,IH,NHIT,JHIT,NCM,ICM,IPR,IPP,LMUD1)
      DO I = 0,3
         NPLHIT = NPLHIT + IBITS(IPP,I,1)
      ENDDO
C
      RETURN
      END
