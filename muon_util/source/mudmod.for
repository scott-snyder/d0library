      SUBROUTINE MUDMOD(NMOD,NRAW,JHIT,LMUD1)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Finds number and location of hits in a module
C-
C-    Input  :  NMOD   - Module ID
C-
C-    Output :  NRAW   - Number of hits in bank MUHP
C-              JHIT   - Number of first hit in MUHP
C-              LMUD1  - Location of header information for this module
C-
C-    Created :  31-AUG-93  M. Fortner
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NMOD,NRAW,JHIT,LMUD1
      INTEGER NWR,NWP,NWM,NSR,NSP,NSM,NP,NF,NSC,NV,LPMUOF(460)
      INTEGER IMOD,MMOD,NPR,IH,NCM,ICM,IPR,IPP,IP5
C
C                Initialize
C
      NRAW = -1
      JHIT = -1
      LMUD1 = 0
      IF (NMOD.LT.0.OR.NMOD.GT.460) RETURN
      IF (NMOD.EQ.0) THEN
          CALL GTMUHT(NWR,NWP,NWM,NSR,NSP,NSM,NP,NF,NSC,NV,LPMUOF)
          NRAW = NF
          JHIT = NP
C
C                Get module information
C
      ELSE
          IMOD = LPMUOF(NMOD)
          IF (IMOD.EQ.0) THEN
              NRAW = 0
              JHIT = 0
          ELSE
              CALL GTMUOF(IMOD,MMOD,NRAW,JHIT,NPR,IH,
     &                    NCM,ICM,IPR,IPP,LMUD1)
              IF (JHIT.EQ.0) NRAW=0
          ENDIF
C
      ENDIF
C
      RETURN
      END
