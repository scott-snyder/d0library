C VAX/DEC CMS REPLACEMENT HISTORY, Element MUHMOD.FOR
C *2    21-OCT-1993 08:52:44 FORTNER "add terms for scintillator"
C *1    15-SEP-1993 17:51:59 DARIEN "New MF code for 1B MUD1"
C VAX/DEC CMS REPLACEMENT HISTORY, Element MUHMOD.FOR
      SUBROUTINE MUHMOD(NMOD,NHIT,JHIT)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Finds number and location of hits in a module
C-
C-    Input  :  NMOD   - Module ID (use 0 to initialize)
C-
C-    Output :  NHIT   - Number of hits in bank MUOH
C-              JHIT   - Number of first hit in MUOH
C-
C-    Created :  10-SEP-93  M. Fortner
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NMOD,NHIT,JHIT
      INTEGER NWR,NWP,NWM,NSR,NSP,NSM,NP,NF,NSC,NV,LPMUOF(460)
      INTEGER IMOD,MMOD,NRW,IH,NCM,ICM,IPR,IPP,IP5,LMUD1
C
C                Get global information
C
      NHIT = -1
      JHIT = -1
      IF (NMOD.LT.0.OR.NMOD.GT.310) RETURN
      IF (NMOD.EQ.0) THEN
          CALL GTMUHT(NWR,NWP,NWM,NSR,NSP,NSM,NP,NF,NSC,NV,LPMUOF)
          NHIT = NWP
          JHIT = 1
C
C                Get module information
C
      ELSE
          IMOD = LPMUOF(NMOD)
          IF (IMOD.EQ.0) THEN
              NHIT = 0
              JHIT = 0
          ELSE
              CALL GTMUOF(IMOD,MMOD,NRW,IH,NHIT,JHIT,
     &                    NCM,ICM,IPR,IPP,LMUD1)
              IF (NHIT.EQ.0) THEN
                  CALL GTMUHT(NWR,NWP,NWM,NSR,NSP,NSM,
     &                        NP,NF,NSC,NV,LPMUOF)
                  JHIT = NWP + 1
              ENDIF
          ENDIF
C
      ENDIF
C
      RETURN
      END
