      SUBROUTINE TRD_CORGAS
     &  (CORRECTION,TEMPCAN,TEMPTRD,ENRJCAN,SLOPCAN,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Gain correction for gas ,temperature and pressure
C-   variations
C-
C-   Inputs  : TEMPCAN : Canary Temperature (Kelvin)
C-             TEMPTRD : TRD temperature (Kelvin)
C-             ENRJCAN : Canary "Energy" ( Mean value of the 8 KeV signal)
C-             SLOPCAN : Variation of the collected energy function of the drift
C-                       time.
C-   Outputs : CORRECTION : Correction factor corrected for the Temperature
C-                          difference between the Canary and the TRD.
C-                       CORGAS takes into account Atmospheric pressure,
C-                       Gas quality and TRD temperature variations
C-                 IER : error code 0 : OK
C-                                  1 : Canary values out of bound
C-                                  2 : arithmetic fault
C-   Controls: TRD.RCP
C-
C-   Created  26-NOV-1992   Y. Ducros
C-   Updated   3-JUN-1993   Alain PLUQUET  Call INRCP (for jobs<>RECO)
C-   Updated   1-JUL-1993   Alain PLUQUET  Changes CANARY bounds
C-                                         Adds special code for bad gas
C-   Updated  17-FEB-1994   A. Zylberstejn Adapt for run 1b
C-   Updated   2-MAR-1994   Alain PLUQUET  gas reference by run number
C-                                         does not depend on T anymore(run 1a)
C-   Updated  23-JUN-1994   Alain PLUQUET  used L.C. canary fit for run 1a
C-   Updated  29-SEP-1994   Alain PLUQUET  Obsolet.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER M,J,R
      PARAMETER (M=200)
      REAL BUFFER(2,M)
      INTEGER RUN_LIMITS(2,M),N_ZONES,RUN_NUMBER_ZONE,RUNNO
      REAL TEMPCAN,TEMPTRD,ENRJCAN,SLOPCAN,ENRJREF(M),ALPHA
      REAL TEMPREF(M),TCANRAP,TTRDRAP,ENRAP,CORRECTION,CTCOR
      REAL RELATIVE_AGEING
      INTEGER IER,LOC
      LOGICAL FIRST,RUN1A
      DATA FIRST/.TRUE./
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZLOC('TRD_RCP',LOC)
        IF (LOC.LE.0) CALL INRCP('TRD_RCP',IER)
        CALL EZPICK ('TRD_RCP')
        CALL GET_TRD_COR_BY_RUN
     &    ('TRD_RCP','TRD_RCP','REF_GAS',BUFFER,2,RUN_LIMITS,N_ZONES)
        CALL EZRSET
        DO J=1,N_ZONES
          ENRJREF(J)=BUFFER(1,J)
          TEMPREF(J)=BUFFER(2,J)+273.15
        ENDDO
        ALPHA=7.1
      END IF

      IF (RUN1A()) THEN
        CORRECTION=CTCOR()*RELATIVE_AGEING()
        IER=0
      ELSE
        R=RUN_NUMBER_ZONE(RUN_LIMITS,RUNNO())
        IF (ENRJCAN.EQ.0.) THEN
          ENRJCAN=ENRJREF(R) ! approximates CANARY
        ENDIF
        IF (TEMPCAN.EQ.0.) THEN
          TEMPCAN=TEMPREF(R) ! approximates temperature
        ENDIF
        IF (TEMPTRD.EQ.0.) THEN
          TEMPTRD=TEMPREF(R) ! approximates temperature
        ENDIF
        IF (ENRJCAN.GT.0..AND.TEMPTRD.GT.0..AND.
     &      TEMPCAN.GT.0..AND.ENRJREF(R).GT.0..AND.
     &      TEMPREF(R).GT.0.) THEN
          ENRAP=ENRJCAN/ENRJREF(R)
          TCANRAP=TEMPCAN/TEMPREF(R)
          TTRDRAP=TEMPTRD/TEMPREF(R)
          IF((ENRJCAN.LT.200..OR.ENRJCAN.GT.900.).OR.
     &      ABS(TTRDRAP-1.).GT.0.5.OR.
     &      ABS(TCANRAP-1.).GT.0.5) THEN
            CORRECTION=1.
            IER=1
            CALL ERRMSG('TRD_CORGAS','TRD_CORGAS',
     &           ' Canary values out of bound','W')
          ELSE
            CORRECTION=(ENRJREF(R)/ENRJCAN)*
     &                  ((TEMPCAN/TEMPTRD)**ALPHA)*
     &                  RELATIVE_AGEING()
            IER=0
          ENDIF
        ELSE
          CORRECTION=1.
          IER=2
        ENDIF
      ENDIF
      END
