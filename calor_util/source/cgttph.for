      SUBROUTINE CGTTPH
C----------------------------------------------------------------------
C-
C-   CGTTPH = (Calorimeter) Generate Trigger Tower to 
C-            PHysics and hex conversion table.
C-
C-   Purpose and Methods : Here we fill in common block/table
C-                         /TTPH/TTPH. TTPH is a 2-D X 5 table that
C-                         given (L1ETAC,L1PHIC) will give a vector of
C-                         (IETAC,IPHIC,CRATE,IADDRL,IADDRH), where
C-                         IADDRL-H are the high and low, modulo the
C-                         lowest 6 bits, hex addresss in the TT.
C-
C-   Inputs  : None.
C-
C-   Outputs : Fills in /TTPH/TTPH
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
C     "Passed" Variables:
*
        INCLUDE 'D0$INC:TTPH.INC/LIST'
*
C     Local Variables:
*
        INTEGER L1ETAC,L1PHIC,IETAC,IPHIC,CRATE,IADDRL,IADDRH
        LOGICAL ITSOK
*
C     ================================================================
*
      DO L1ETAC = -NETAL11,NETAL11
        DO L1PHIC = 1,NPHIL1
          CALL CTTPH(L1ETAC,L1PHIC,IETAC,IPHIC,
     &               CRATE,IADDRL,IADDRH,ITSOK)
          IF (ITSOK .AND. L1ETAC.NE.0) THEN
            TTPH(L1ETAC,L1PHIC,1) = IETAC
            TTPH(L1ETAC,L1PHIC,2) = IPHIC
            TTPH(L1ETAC,L1PHIC,3) = CRATE
            TTPH(L1ETAC,L1PHIC,4) = IADDRL
            TTPH(L1ETAC,L1PHIC,5) = IADDRH
          ELSE
            TTPH(L1ETAC,L1PHIC,1) = 0
            TTPH(L1ETAC,L1PHIC,2) = 0
            TTPH(L1ETAC,L1PHIC,3) = 0
            TTPH(L1ETAC,L1PHIC,4) = 0
            TTPH(L1ETAC,L1PHIC,5) = 0
          ENDIF
        END DO
      END DO
*
  999 RETURN
      END
