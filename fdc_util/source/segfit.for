      SUBROUTINE SEGFIT(X,Y,N,SLOPE,INTER,CHIDOF,RESID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fit 2-D points to a straight line.
C-
C-   Inputs  : X,Y = X,Y points to be fitted to a line
C-             N   = Number of points to be fit
C-   Outputs : SLOPE   = Slope of fitted line
C-             INTER   = Y-intercept of fitted line
C-             CHIDOF  = CHISQ/DOF of fit
C-             RESID   = Residuals of the fitted points to the line,
C-                       fit INCLUDES the point.
C-
C-   Created  12-SEP-1989   Jeffrey Bantly
C-   Updated  16-APR-1990   Susan K. Blessing  Remove reference to
C-    Cernlib routine PROB which uses a lot of time and gives weird
C-    results sometimes (.gt. 1.0).  Use chi**2/dof instead.
C-    Calling sequence changed.
C-   Updated  17-JUN-1991   Robert E. Avery   Make some internal
C-    variables into double precision, and some changes to avoid
C-    roundoff error. Also correct normalization of Chisq.
C-   Updated  17-JUN-1991   Susan K. Blessing   Change A and B to
C-    SLOPE and INTER to avoid future errors like the one caused by
C-    the comments not matching the code.
C-   Updated  17-OCT-1991   Robert E. Avery  Include slope dependent
C-    correction to error.
C-   Updated   2-APR-1993   Susan K. Blessing  Add option to calculate
C-    residuals.  Default to false.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER N,I
      INTEGER UNIT
      INTEGER IER
C
      REAL X(16),Y(16)
      REAL SLOPE,INTER,CHIDOF
      REAL Y1,YREL
      REAL X1,XREL
      REAL CHISQ,RESID(16),PROB, YOFX
      REAL ERROR
      REAL FDC_ERROR_SLOPE
      REAL SMIN,IREL
C
      DOUBLE PRECISION A1,B1,D
      DOUBLE PRECISION XSUM,YSUM,XYSUM,XXSUM,YYSUM
C
      LOGICAL FIRST
      LOGICAL SEG_RESIDS
C
      SAVE FIRST,SEG_RESIDS
C
      DATA FIRST/.TRUE./
      DATA SEG_RESIDS/.FALSE./
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('SEG_RESIDS',SEG_RESIDS,IER)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
C simple straight-line fit
C
      XSUM=0.
      YSUM=0.
      XYSUM=0.
      XXSUM=0.
      YYSUM = 0.
C
      Y1 = Y(1)
      X1 = X(1)
      DO I=1,N
        XREL = X(I) - X1
        YREL = Y(I) - Y1
        XSUM = XSUM + XREL
        YSUM = YSUM + YREL
        XYSUM = XYSUM + XREL*YREL
        XXSUM = XXSUM + XREL*XREL
        YYSUM = YYSUM + YREL*YREL
      END DO
C
      A1 = YSUM*XXSUM - XSUM*XYSUM
      B1 = FLOAT(N)*XYSUM - XSUM*YSUM
      D = FLOAT(N)*XXSUM - XSUM*XSUM

      SLOPE = 0.
      INTER = 0.
      IF (D.NE.0.) THEN
        SLOPE = B1/D
        INTER = A1/D + Y1 - SLOPE * X1
        IREL = A1/D
      END IF
C
C Calculate the error on the slope
      UNIT = N/9                        ! Should be right most of time
C Error on points
      ERROR = FDC_ERROR_SLOPE(SLOPE,UNIT)
C
      CHISQ = 0.
      IF (SEG_RESIDS) THEN
        DO I=1,N
          YOFX = INTER + SLOPE * X(I)
          RESID(I) = Y(I) - YOFX
          CHISQ = CHISQ + (RESID(I)*RESID(I))
        END DO
        CHIDOF = CHISQ / (FLOAT(N-2) * ERROR**2)
      ELSE
C
        SMIN = YYSUM - IREL*YSUM - SLOPE*XYSUM
        CHIDOF = SMIN / (FLOAT(N-2) * ERROR**2)
      END IF
C
C
C----------------------------------------------------------------------
  999 RETURN
      END
