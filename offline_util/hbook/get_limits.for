      SUBROUTINE GET_LIMITS(XLOLIM,XHILIM,X1,X2,NX,Y,
     &  NENT,CUM,SUM,XLO,XHI,XMAX,YMAX, ILO,IHI,IMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GIVEN a histogram in Y, works out upper and lower
C-   confidence limits given by XLOLIM, XHILIM and the maximum value of the
C-   histogram. Returns the Cumulative histogram in CUM
C-
C-   Inputs  : XLOLIM,XHILIM = confidence levels . For 90% CL upper and lower
C-   bounds, XLOLIM,XHILIM = .05, .95
C-              X1,X2, are the lower and upper values of the Histogram 
C-              abscissa . NX is the number of bins. 
C-              Y = Contents of histogram
C-              NENT = number of entries of histogram
C-   Outputs : CUM = Cumulative Histogram of Y
C-             SUM = Sum of values of histogram
C-             XLO,XHi are the X values at which XLOLIM, XHILIM are reached.
C-             XMAX is the X value at which maximum value is reached
C-             YMAX is the maximum value of the histogram
C-             ILO,IHI are the bin numbers at which XLOLIM,XHILIM are reached
C-             IMAX is the maximum value bin.
C-   Controls: 
C-
C-   Created  22-JUN-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    X1,X2,XLO,XHI,XMAX,YMAX
      INTEGER NX,NENT,ILO,IHI,IMAX
      REAL    Y(NX),CUM(NX)
      REAL    DELX,SUM
      REAL    XLOLIM,XHILIM
      INTEGER I,LOUT
      INTEGER IDEL
      DATA IDEL/0/   !IF 1 WILL ADD HALF BIN WIDTH
      DATA LOUT/6/
C----------------------------------------------------------------------
      YMAX =0.0
      XMAX = 0.0
      IMAX=0
      XLO = 0.
      XHI = 0.
      ILO = 0
      IHI = 0
C
      DELX = (X2-X1)/NX
C
      CALL UCOPY(Y,CUM,NX)
C
      DO 100 I = 2, NX
        CUM(I)  = CUM(I)+CUM(I-1)
        IF ( Y(I).GT.YMAX ) THEN
          YMAX=Y(I)
          IMAX=I
          XMAX = X1+(I-1)*DELX+IDEL*0.5*DELX
        ENDIF
  100 CONTINUE
C
      SUM = CUM(NX)
C
      IF ( SUM.EQ.0.0 ) THEN
C        WRITE(LOUT,*)' ZERO WEIGHTS IN HISTOGRAM '
        RETURN
      ENDIF
C
C      WRITE(LOUT,*) ' SUM OF HISTOGRAM BINS ',SUM
C
      IF(NENT.NE.0) THEN
        DELX = (X2-X1)/NX
C        print *,nx,CUM(1),y(2)
        DO 200 I = 1, NX
          CUM(I)  = CUM(I)/SUM
C          WRITE(LOUT,*)I,CUM(I),Y(I),XLO,DELX
          IF ( CUM(I).GE.XLOLIM.AND.XLO.EQ.0.0 ) THEN
            XLO = X1+(I-1)*DELX+IDEL*0.5*DELX
C            WRITE(LOUT,*)I,CUM(I),XLO,DELX
            ILO = I
          ENDIF
          IF ( CUM(I).GE.XHILIM.AND.XHI.EQ.0.0 ) THEN
            XHI = X1+(I-1)*DELX+IDEL*0.5*DELX
C            WRITE(LOUT,*)I,CUM(I),XHI,DELX
            IHI = I + 1
          ENDIF
  200   CONTINUE
      ENDIF
C
  999 RETURN
      END
