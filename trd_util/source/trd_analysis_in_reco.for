      SUBROUTINE TRD_ANALYSIS_IN_RECO(LTRDT,ACCEPTANCE,EFFICIENCY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TRD analysis routine for RECO jobs only !
C-   Inputs  :
C-      LTRDT        integer   link to TRDT bank
C-   Outputs :
C-     ACCEPTANCE    logical   overall accptance (geometry, bad runs, ...)
C-     EFFICIENCY    real      electron efficiency
C-                             pion     : efficiency close to 1.
C-                             electron : efficiency in [0.,1.]
C-
C-   Controls: TRD_ANALYSIS.RCP,TRD.RCP
C-
C-   Created   15-JUN-1994   Alain PLUQUET from TRD_ANALYSIS.FOR
C-   Updated  26-OCT-1994  Jean-Francois LEBRAT
C-
C-                          call to    ENERGY_MAX_CELLS
C-                                     TRD_TO_CDC_DEDX
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TRD_NWORD.INC'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:THIT_INFO.INC'
      INCLUDE 'D0$INC:TRD_DST_ENERGIES.INC'
      INCLUDE 'D0$INC:WORD_IN_TPRL.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL     EFFICIENCY,EPICOR(3),EFFICIENCY_LIK,ECDC
      REAL     RW(3,NWORD),VERSION,ENERGY_MAX_CELLS(5),LIK1,LIK2
      INTEGER  IW(3,NWORD),DIAGNOSTIC_COR,DIAGNOSTIC_ENV
      INTEGER  LTRDT,MULTIPLICITY(3),HITS(3),CD_ACTIVITY,FIRED_CELLS(3)
      INTEGER GZTHIT,EVTI
      EQUIVALENCE (IW,I_IN_TPRL),(RW,R_IN_TPRL)
      LOGICAL  ACCEPTANCE,CORRECTION(10),GEOMETRY(3)
      DATA EVTI/0/

      IF(IQ(LHEAD+9).NE.EVTI)THEN
        EVTI=IQ(LHEAD+9)
        IF(GZTHIT().NE.0)THEN ! Look FOR THIT BANK
          IF(FIRST_INFO(1).NE.1)CALL THIT_GET
        END IF
      END IF

      CALL TRD_DST_COR_IN_RECO
     &  (LTRDT,VERSION,CORRECTION,ENERGY,DIAGNOSTIC_COR,RW,IW,EPICOR)

      CALL TRD_ENERGY_FIRED_CELLS
     &  (LTRDT,RW,IW,FIRED_CELLS,ENERGY_FIRED_CELLS)

      CALL TRD_ENVIRONMENT
     &  (LTRDT,VERSION,RW,IW,
     &  GEOMETRY,MULTIPLICITY,HITS,CD_ACTIVITY,DIAGNOSTIC_ENV)

      CALL TRD_ELECTRON_PION
     &  (ENERGY,ENERGY_FIRED_CELLS,ENERGY_MAX_CELLS,ECDC,EPICOR,
     &   GEOMETRY,MULTIPLICITY,HITS,CD_ACTIVITY,DIAGNOSTIC_COR,
     &   DIAGNOSTIC_ENV,ACCEPTANCE,EFFICIENCY,EFFICIENCY_LIK,LIK1,LIK2)

      END
