      SUBROUTINE CGPHTT
C----------------------------------------------------------------------
C-
C-   CGPHTT = (Calorimeter) Generate PHysics (IETAC,IPHIC)
C-            indices to Trigger Tower table.
C-
C-   Purpose and Methods : With this routine common block/table PHTT is
C-                         filled in. PHTT is a pair of 1-D tables that accept
C-                         (IETAC,IPHIC) and give (L1ETAC,L1PHIC).
C-
C-   Inputs  : None.
C-
C-   Outputs : Fills in /PHTT/PHTTETA(-NETAL:NETAL),PHTTPHI(NPHIL)
C-
C-   Controls: None.
C-
C-   Created  12-SEP-1989   Dale A. Ross, MSU
C-   Updated  10-NOV-1990   James T. Linnemann   
C-
C----------------------------------------------------------------------
*
      IMPLICIT NONE!
*
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS/LIST'
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS/LIST'
*
C     "Passed" Variable(s):
*
      INCLUDE 'D0$INC:PHTT.INC/LIST' ! Has parameters dependent
C                                             on L1PHP.PARAMS.
*
C     Local Variables:
*
      INTEGER IETAC,IPHIC,L1PHIC,L1ETAC
      LOGICAL ITSOK
*
C     ================================================================
*
      IPHIC = 1
      DO IETAC = -NETAL,NETAL
        CALL CPHTT(IETAC,IPHIC,L1ETAC,L1PHIC,ITSOK)
        IF (ITSOK .AND. IETAC.NE.0) THEN
          PHTTETA(IETAC) = L1ETAC
        ELSE
          PHTTETA(IETAC) = 0
        ENDIF
      END DO
      IETAC = 1
      DO IPHIC = 1,NPHIL
        CALL CPHTT(IETAC,IPHIC,L1ETAC,L1PHIC,ITSOK)
        PHTTPHI(IPHIC) = L1PHIC
      END DO
  999 RETURN
      END
