      SUBROUTINE CGDBITT
C----------------------------------------------------------------------
C-
C-   CGDBITT = (Calorimeter) Generate Data Block Index to Trigger Tower table.
C-
C-   Purpose and Methods : Fills in common block/table /DBITT/DBITT.
C-                         DBITT is a 1-D X 2 lookup table that 
C-                         given a DBI will give the corrosponding 
C-                         (L1ETAC,L1PHIC).
C-
C-   Inputs  : None.
C-
C-   Outputs : Fills in /DBITT/DBITT
C-
C-   Controls: None.
C-
C-   Created  12-SEP-1989   Dale A. Ross, MSU
C-
C----------------------------------------------------------------------
*
      IMPLICIT NONE!
*
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS/LIST'
*
C     Passed Variables:
*
        INCLUDE 'D0$INC:DBITT.INC/LIST'
*
C     Local Variables:
*
        INTEGER DBI,L1ETAC,L1PHIC
        LOGICAL ITSOK
*
C     ================================================================
*
      DO DBI = 0,(NETAL1*NPHIL1*2)-1
        CALL CDBITT(DBI,L1ETAC,L1PHIC,ITSOK)
        IF (ITSOK) THEN
          DBITT(DBI,1) = L1ETAC
          DBITT(DBI,2) = L1PHIC
        ELSE
          DBITT(DBI,1) = -1 ! since there is no such thing as a neg. DBI.
          DBITT(DBI,2) = -1
        ENDIF
      END DO
*
  999 RETURN
      END
