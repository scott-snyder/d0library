      SUBROUTINE CGGAPAD
C----------------------------------------------------------------------
C-
C-   CGGAPAD = (Calorimeter) Generate GAP ADdresses
C-
C-   Purpose and Methods : Here we fill in PHGP, a 2 x 3 lookup table
C-                         whose inputs are (IETAC,IPHIC), which gives
C-                         the hex address of the ICD and MG's at that
C-                         location.
C-
C-   Inputs  : None.
C-
C-   Outputs : Fills in PHGP in the common block /PHGP/
C-
C-   Controls: None.
C-
C-   Created  29-AUG-1989   Dale A. Ross, MSU
C-
C----------------------------------------------------------------------
*
      IMPLICIT NONE!
*
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS/LIST'
      INCLUDE 'D0$PARAMS:CAL_LEVEL2.PARAMS/LIST'
*
C     Passed Variables:
*
        INCLUDE 'D0$INC:PHGP.INC/LIST' ! The dimensioning of PHGP
C                                           is dependent on params in
C                                           CAL_LEVEL2.PARAMS.
*
C     Local Variables:
*
        LOGICAL CCMGXST,ECMGXST,ICDXST,OR,ITSOK
        INTEGER IPHIC,IETAC,CRATE,IADDR
*
C     _________________________________________________________________
C     _________________________________________________________________
*
*
      DO IPHIC=1,NPHIL
        DO IETAC = -NETAL,NETAL
          IF (IETAC .NE. 0) THEN  !-- IETAC=0 is an illegal value.
            CALL CL2_GAPXST(IETAC,IPHIC,CCMGXST,ECMGXST,ICDXST,OR)
            IF (CCMGXST) THEN
              CALL CL2_GAP_ADR(CCGPTP,IETAC,IPHIC,CRATE,IADDR,ITSOK)
              IF (ITSOK) THEN
                PHGP(IETAC,IPHIC,1,CCGPTP) = CRATE
                PHGP(IETAC,IPHIC,2,CCGPTP) = IADDR
              ELSE
                PHGP(IETAC,IPHIC,1,CCGPTP) = 0
                PHGP(IETAC,IPHIC,2,CCGPTP) = 0
              ENDIF
            ENDIF
            IF (ICDXST) THEN
              CALL CL2_GAP_ADR(ICDGPTP,IETAC,IPHIC,CRATE,IADDR,ITSOK)
              IF (ITSOK) THEN
                PHGP(IETAC,IPHIC,1,ICDGPTP) = CRATE
                PHGP(IETAC,IPHIC,2,ICDGPTP) = IADDR
              ELSE
                PHGP(IETAC,IPHIC,1,ICDGPTP) = 0
                PHGP(IETAC,IPHIC,2,ICDGPTP) = 0
              ENDIF
            ENDIF
            IF (ECMGXST) THEN
              CALL CL2_GAP_ADR(ECGPTP,IETAC,IPHIC,CRATE,IADDR,ITSOK)
              IF (ITSOK) THEN
                PHGP(IETAC,IPHIC,1,ECGPTP) = CRATE
                PHGP(IETAC,IPHIC,2,ECGPTP) = IADDR
              ELSE
                PHGP(IETAC,IPHIC,1,ECGPTP) = 0
                PHGP(IETAC,IPHIC,2,ECGPTP) = 0
              ENDIF
            ENDIF
          ENDIF
        END DO
      END DO
  999 RETURN
      END
