      SUBROUTINE ENDET (DETEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return detector mneumonic from top-level 
C-                         SRCP bank.
C-
C-   Inputs  : None
C-   Outputs : DETEC       Detector mneumonic (CHARACTER*4)
C-                         '/CAL', '/CDC', '/MUO' etc.
C-   Controls: 
C-
C-   Created   7-MAR-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER N
      CHARACTER*4 DETEC
C----------------------------------------------------------------------
      CALL EZPICK ('SCPH')
      CALL EZGSET ('DETECTOR',N,1)      ! Get detector name
      CALL EZRSET
      CALL UHTOC (N,4,DETEC,4)
      CALL UPCASE (DETEC,DETEC)
C
  999 RETURN
      END
