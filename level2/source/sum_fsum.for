      SUBROUTINE SUM_FSUM(TIME_IN,TTIME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  add a single Filter Summary to the Grand filter
C-              summary
C-
C-   Inputs  :TIME_IN normalizing time
C-   Outputs :
C-   Controls:
C-
C-   Created   1-FEB-1992   James T. Linnemann
C-   Updated  15-APR-1992   Drew Baden add TIME normalization 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL WEIGHT   ! the weight for this summary
      INCLUDE 'D0$INC:FILTER_COM.INC'
      INCLUDE 'D0$INC:GRAND_FILTER_COUNT.INC'
      INCLUDE 'D0$PARAMS:L2_TYPE.DEF'
      INCLUDE 'D0$INC:FSUM.INC'
      INCLUDE 'D0$INC:GRAND_FILTER_TIMING.INC'
      INTEGER I,J,K
      REAL    WTOLD,OLD,NEW,TIME_IN,TTIME
C----------------------------------------------------------------------
      WEIGHT = EVENT_WEIGHT
      TTIME = TTIME + TIME
      IF (WEIGHT.LE.0) THEN
        CALL ERRMSG('ZERO_WEIGHT','SUM_FSUM',' zero weight; set to 1.0',
     &    'W')
        WEIGHT = 1.0
      ENDIF
      WTOLD = WTSUM
      WTSUM = WTSUM + WEIGHT
      DO I = 1,2
        DO J = 0,MAX_TOOL_FILTER
          DO K = 0,127
            GRAND_FILTER_COUNT(I,J,K) = GRAND_FILTER_COUNT(I,J,K) +
     &        FILTER_COUNT(I,J,K)*WEIGHT
            GRAND_FILTER_ERR(I,J,K) = SQRT(GRAND_FILTER_ERR(I,J,K)**2 +
     &        FILTER_COUNT(I,J,K)*WEIGHT**2)
          ENDDO
        ENDDO
      ENDDO
      OLD = GRAND_TOT_EVENTS(1)
      NEW = TOT_EVENTS_READ(1)*WEIGHT
      DO I = 1,3
        GRAND_TOT_EVENTS(I) = GRAND_TOT_EVENTS(I) +
     &    TOT_EVENTS_READ(I)*WEIGHT
        GRAND_TOT_ERR(I) = SQRT(GRAND_TOT_ERR(I)**2 +
     &    TOT_EVENTS_READ(I)*WEIGHT**2)
      ENDDO
      DO I = 1,2
        IF ((OLD+NEW).GT.0) THEN
          GRAND_AVGBITS(I) =
     &      (OLD*GRAND_AVGBITS(I) + AVGBITS(I)*NEW)/(OLD+NEW)
        ENDIF
      ENDDO
        OLD = GRAND_NCALL
        NEW = FNCALL*WEIGHT
        GRAND_NCALL = OLD + NEW
        IF (GRAND_NCALL.GT.0) THEN
          GRAND_AVG = (OLD*GRAND_AVG + FAVG*NEW)/GRAND_NCALL
          GRAND_SIGMA = SQRT((OLD*GRAND_SIGMA**2 + NEW*FSIGMA**2)
     &      / GRAND_NCALL)
      ENDIF
      DO I = 0,127
          OLD = GRAND_SNCALL(I)
          NEW = SNCALL(I)*WEIGHT
          GRAND_SNCALL(I) = OLD + NEW
          IF (GRAND_SNCALL(I).GT.0) THEN
            GRAND_SAVG(I) = (OLD*GRAND_SAVG(I) + SAVG(I)*NEW)
     &        /GRAND_SNCALL(I)
            GRAND_SSIGMA(I) = SQRT((OLD*GRAND_SSIGMA(I)**2 +
     &        (SSIGMA(I)**2)*NEW)
     &        /GRAND_SNCALL(I) )
          ENDIF
      ENDDO
      DO I = 1,MAXTOOL
          OLD = GRAND_TNCALL(I)
          NEW = TNCALL(I)*WEIGHT
          GRAND_TNCALL(I) = OLD + NEW
          IF (GRAND_TNCALL(I).GT.0) THEN
            GRAND_TAVG(I) = (OLD*GRAND_TAVG(I) + TAVG(I)*NEW)
     &        /GRAND_TNCALL(I)
            GRAND_TSIGMA(I) = SQRT((OLD*GRAND_TSIGMA(I)**2 +
     &        (TSIGMA(I)**2)*NEW)
     &        /GRAND_TNCALL(I))
          ENDIF
      ENDDO
  999 RETURN
      END
