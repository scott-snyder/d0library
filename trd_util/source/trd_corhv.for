      SUBROUTINE TRD_CORHV(CORRECTION,LAYER,HTANO,HTPOT,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : HV Corrections
C-
C-   Inputs  : HTAN0,HTPOT real     anode HV, potentail HV in volts
C-             LAYER       integer  1,2 or 3
C-   Outputs : CORRECTION  real     correction for HV variations, and
C-   normalisation on Layer 2 with Anode = ANOREF and Potential = POTREF
C- The variation of the Gain is taken proportional to (E/E0)**VAR
C- where VAR=15 in this subroutine.
C- The correction takes into account the difference in the geometry
C- of the chambers.
C-                 IER : error code 0 : OK
C-                                  1 = correction not required in TRD.RCP
C-                                  2 = arithmetic fault
C-   Controls:
C-
C-   Created  26-NOV-1992   Y. Ducros
C-   Updated   3-JUN-1993   Alain PLUQUET  Call INRCP (for jobs<>RECO)
C-   Updated   3-JUN-1993   Alain PLUQUET  Correct bug (add IER=0)
C-   Updated  30-SEP-1994   Y. Ducros
C-                          IER=1 and Correction=1. if :
C-                            HTANO > 1670. or HTANO < 1550.
C-                                       or
C-                            HTPOT > 250. or HTPOT < 150.
C-   Updated  30-SEP-1994   Alain PLUQUET   Read reference frome TRD.RCP
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL HTANO,HTPOT,ANOREF(3),POTREF(3),ELPOT(3)
      REAL CORRECTION,VAR,DENOM
      INTEGER LAYER,IER,M,I,LOC
      PARAMETER (M=200)
      REAL HV_ARRAY(6,M)
      INTEGER RUN_LIMITS(2,M),R,RUN_NUMBER_ZONE,N_ZONES,RUNNO
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF(FIRST) THEN
        VAR=15.
        ELPOT(1)=0.32
        ELPOT(2)=0.18
        ELPOT(3)=0.36
        CALL EZLOC('TRD_RCP',LOC)
        IF (LOC.LE.0) CALL INRCP('TRD_RCP',IER)
        CALL EZPICK('TRD_RCP')
        CALL GET_TRD_COR_BY_RUN
     &    ('TRD_RCP','TRD_RCP','REF_HV',HV_ARRAY,6,RUN_LIMITS,N_ZONES)
        CALL EZRSET
        FIRST=.FALSE.
      ENDIF

      R=RUN_NUMBER_ZONE(RUN_LIMITS,RUNNO())
      DO I=1,3
        ANOREF(I)=HV_ARRAY(I,R)
      ENDDO
      DO I=1,3
        POTREF(I)=HV_ARRAY(I+3,R)
      ENDDO

      IER=0
      IF(HTPOT.LT.150..OR.HTPOT.GT.250.) THEN
        IER=1
        CORRECTION=1.
        RETURN
      END IF
      IF(HTANO.LT.1550..OR.HTANO.GT.1670.) THEN
        IER=1
        CORRECTION=1.
        RETURN
      END IF
      DENOM=HTANO-ELPOT(LAYER)*(HTPOT-POTREF(LAYER))
      IF(DENOM.LE.0.) THEN
        IER=1
        CORRECTION=1.
        RETURN
      END IF
C *****   Anode and Pot. HV Corrections
      CORRECTION=(ANOREF(LAYER)/DENOM)**VAR
      END
