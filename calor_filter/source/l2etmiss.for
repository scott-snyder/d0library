      SUBROUTINE L2ETMISS( PARAM_SET_NUMBER,HARDWARE,RESULT_FLAG,
     &  EXTRA_FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TOOL MET : filter on missing ET
C-
C-   Inputs  : NOW_SET : # of parameter set to use
C-             HARDWARE: mask of set bits for LV1 trigger which started
C-                       this filter.
C-   Outputs : RESULT_FLAG : Flag set to TRUE when we want to pass tool
C-                           under this PARAM_SET_NUMBER
C-             EXTRA_FLAG  : Set to TRUE when we want to pass event and
C-                           do no further filtering.
C-   Controls:
C-
C-   Created  29-JUN-1991   James T. Linnemann
C-   Modified 13-Jul-1992   Ulrich Heintz: add missing Et significance cut
C-   Updated  08-MAR-1993   Amber Boehnlein, added QUALITY BITS, ETOT TO ESUM
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PARAM_SET_NUMBER,HARDWARE,IER
      LOGICAL EXTRA_FLAG,RESULT_FLAG
      INCLUDE 'D0$INC:ZEBCOM.INC'               ! zebra main store
      INCLUDE 'D0$INC:L2ETMISS_CUTS.INC'
      INCLUDE 'D0$INC:CL2_LINK.INC'
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      CHARACTER*80 MSG
      REAL    SLOPE,OFFSET
      INTEGER QUALITY_BITS
      LOGICAL OK,EZERROR                  
      DATA SLOPE/0.029/,OFFSET/0.9/
C----------------------------------------------------------------------
      EXTRA_FLAG = .FALSE.
      RESULT_FLAG = .FALSE.
      OK = .TRUE.
      IF ((PARAM_SET_NUMBER.LE.0).OR.(PARAM_SET_NUMBER.GT.NPAR)) THEN
        WRITE(MSG,'(A,I5,A,I5)')' parameter set requested = ',
     &      PARAM_SET_NUMBER, ' but only had ',NPAR
        CALL ERRMSG('L2ETMISS','L2ETMISS',MSG,'E')
      ELSE
        IF (L2PNUT.EQ.0) CALL CL2_CAEPFL(OK)      ! if needed
        IF (OK.AND.(L2PNUT.GT.0)) THEN
          RESULT_FLAG = (Q(L2PNUT+7).GT.ETMISS_MIN(PARAM_SET_NUMBER))
          IF(ETMISS_SIG_MIN(PARAM_SET_NUMBER).GT.0)THEN
            IF (Q(L2PNUT+14).LE.0) THEN
              RESULT_FLAG = .FALSE.   ! we will fail any neg-ET event
            ELSE
C...divide can only fail on - Et provided consts are +
            RESULT_FLAG = RESULT_FLAG .AND.
     &        (Q(L2PNUT+7)/(OFFSET+SLOPE*Q(L2PNUT+14)) .GT.
     &        ETMISS_SIG_MIN(PARAM_SET_NUMBER))
            ENDIF
          ENDIF
          QUALITY_BITS = 0
          CALL L2ET_PARSE_QUALITY_BITS(QUALITY_BITS)
          CALL ESUMFL('FILT',ID_ETMISS,Q(L2PNUT+7),Q(L2PNUT+9),
     &      Q(L2PNUT+9), Q(L2PNUT+10),QUALITY_BITS)
          CALL ESUMFL('FILT',ID_ETSUM,Q(L2PNUT+14),Q(L2PNUT+9),
     &      Q(L2PNUT+9), Q(L2PNUT+10),QUALITY_BITS)
          CALL ESUMFL('FILT',ID_ETOTAL,Q(L2GLOB+2),Q(L2PNUT+9),
     &      Q(L2PNUT+9), Q(L2PNUT+10),QUALITY_BITS)
        ENDIF
      ENDIF
  999 RETURN
      END
