      SUBROUTINE MNHMOD(NMOD,NSCN,JSCN)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Finds number and location of scint hits
C-
C-    Input  :  NMOD   - Module ID (use 0 to initialize)
C-
C-    Output :  NSCN   - Number of hits in bank MSCT
C-              JSCN   - Number of first hit in MSCT
C-
C-    Created :  20-OCT-93  M. Fortner
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NMOD,NSCN,JSCN
      INTEGER NWR,NWP,NWM,NSR,NSP,NSM,NP,NF,NSC,NV,LPMUOF(460)
      INTEGER IMOD,MMOD,NRW,IH,NH,JH,IPR,IPP,IP5,LMUD1
C
C                Get global information
C
      NSCN = -1
      JSCN = -1
      IF (NMOD.LT.0.OR.NMOD.GT.310) RETURN
      IF (NMOD.EQ.0) THEN
          CALL GTMUHT(NWR,NWP,NWM,NSR,NSP,NSM,NP,NF,NSC,NV,LPMUOF)
          NSCN = NSC
          JSCN = 1
C
C                Get module information
C
      ELSE
          IMOD = LPMUOF(NMOD)
          IF (IMOD.EQ.0) THEN
              NSCN = 0
              JSCN = 0
          ELSE
              CALL GTMUOF(IMOD,MMOD,NRW,IH,NH,JH,
     &                    NSCN,JSCN,IPR,IPP,LMUD1)
              IF (NSCN.EQ.0) THEN
                  CALL GTMUHT(NWR,NWP,NWM,NSR,NSP,NSM,
     &                        NP,NF,NSC,NV,LPMUOF)
                  JSCN = NSC + 1
              ENDIF
          ENDIF
C
      ENDIF
C
      RETURN
      END
