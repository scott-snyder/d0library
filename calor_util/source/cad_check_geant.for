      SUBROUTINE CAD_CHECK_GEANT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books and fills histograms for checking CAD bank
C-                         filling in GEANT and unpacking in the CAHITS package
C-
C-   Inputs  :  NONE
C-   Outputs : NONE
C-   Controls: CAD_CHECK_GEANT_RCP
C-
C-   Created 14-SEP-1990   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER,GZCATE,GZPNUT,I,IETA,IPHI,ILYR
      INTEGER K,NCHT,NEMT,LDCATE,NR,NFOUND
      REAL    E(4),ET,WT,ISA_WEIGHT,X,SCALAR_ET
      LOGICAL FIRST,LMONTE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CATENM.PARAMS'
C
      LOGICAL START,DO_ZSUPR
      INTEGER BITS,ICHAN,NCH_GEAN,NCH_RECO,NV,GZCAEP,CRATE,IWORD,PH
      REAL ENERGY_GEAN, ENERGY_RECO,ENERGY_CUT
      CHARACTER*50 MSG
      INCLUDE 'D0$INC:ZLINKC.INC'
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST)THEN
        CALL CZLINI
        FIRST = .FALSE.
        CALL EZPICK('CAHITS_RCP')
        CALL EZGET_l('MONTE_CARLO_DATA',LMONTE,IER)
        CALL EZGET_l('DO_ZERO_SUPRESS',DO_ZSUPR,IER)
        CALL EZGET('ENERGY_CUTOFF',ENERGY_CUT,IER)
        CALL EZRSET
      ENDIF
C
C ****   CHECK CAEPFL/CADFL
C
      CALL PATHST('GEAN')
      CALL GTCAEP_HEADER(NV,NR,NCH_GEAN,IER)
      CALL PATHST('RECO')
      CALL GTCAEP_HEADER(NV,NR,NCH_RECO,IER)
      START = .TRUE.
      NFOUND = 0
      DO 5,  I = 1, NCH_GEAN
        CALL PATHST('GEAN')
        LCAEP=GZCAEP()
        CALL GTCAEP(START,IETA,IPHI,ILYR,BITS,ENERGY_GEAN,ICHAN,IER)
        IF (IER.NE.0 ) THEN
          WRITE(MSG,3)IER
    3     FORMAT(' GEAN CAEP IER ',I5)
          CALL ERRMSG('CAEP GEAN BAD','CAD_CHECK_GEANT', MSG, 'W')
        END IF
        START = .FALSE.
        CALL PATHST('RECO')
        LCAEP=GZCAEP()
        CALL GTCAEP_ADDR(IETA,IPHI,ILYR,ENERGY_RECO,IER)
        IF (IER.NE.0 ) THEN
          IF( DO_ZSUPR .AND. (ENERGY_GEAN.GT.ENERGY_CUT)) THEN
            WRITE(MSG,4)IETA,IPHI,ILYR,ENERGY_GEAN,ENERGY_RECO
            CALL ERRMSG('MISSING RECO CELL','CAD_CHECK_GEANT', MSG, 'W')
          END IF
    4     FORMAT('EPL',3I5,' IN RECO ',F8.2,' GEAN',F8.2)
          CALL HF1(501, ENERGY_GEAN,1.0)
          GOTO 5
        END IF
        NFOUND = NFOUND + 1
        CALL HF1(500, ENERGY_GEAN-ENERGY_RECO, 1.0)
    5 CONTINUE
C
C ****  EXTRA RECO CHANNEL CHECK
C
      IF ( NFOUND.NE.NCH_RECO) THEN
        WRITE(MSG,13)NFOUND,NCH_RECO
   13   FORMAT(' NFOUND ',I5, ' NCH_RECO ',I5)
        CALL ERRMSG('CAEP RECO BAD','CAD_CHECK_GEANT', MSG, 'W')
      END IF
C
  999 RETURN
      END
