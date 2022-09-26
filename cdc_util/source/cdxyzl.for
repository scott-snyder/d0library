      SUBROUTINE CDXYZL(XI,YI,ZI,WR,WZ,N,NDEGF,MINZ,PARFIT,CHISQ,RESID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fit a line X=X0 + S * COS(phi) SIN(teta)
C-                                    Y=Y0 + S * SIN(phi) SIN(teta)
C-                                    Z=Z0 + S * COS(teta)
C-               We suppose ( CDC, VTX ) that X,Y are measured by a
C-               drift time ( one error ), and Z is measured
C-               independently.
C-               We suppose that all WR errors are perpendicular
C-               to the final track, and error on Z is for the point
C-               at previously determined x,y from XI,YI (i.e. there
C-               must be a X,Y for each Z, but Z may be missing for X,Y.)
C-
C-               Computes first X0,Y0,Z0, then theta, phi and errors with
C-               corrected values ( i.e. Xi-X0 ) to avoid
C-               substraction of big numbers.
C-
C-   Inputs  : XI(N) : Abcissa of points
C-             YI(N) : Ordinates    "
C-             ZI(N) : Z coordinate
C-             WR(N) : weights in X-Y plane, inverse of error squared
C-             WZ(N) : weights in Z plane, inverse of error squared
C-             N     : number of points
C-             NDEGF : number of degree of freedom for SW and DL
C-             MINZ  : minimum Z hits for RZ fit
C-   Outputs : 
C-             NDEGF : number of degree of freedom for SW and DL
C-             In PARFIT, in that order :
C-                X0,Y0,Z0 : the non-correlation point on the track
C-                THETA  : angle of the track [ 0,pi ]   in R-Z plane
C-                PHI    : angle of the track [ 0,2*pi ] in X-Y plane
C-                ERRD   : error on the point X0,Y0, perpendicular to the track
C-                ERRZ   : error on Z0
C-                ERRTHE : error on theta ( radians )
C-                ERRPHI : error on phi ( radians )
C-                COVAR  : covariance term betwwen theta and Z
C-             CHISQ(2) : Chi squared of the fits ( X-Y, XY-Z )
C-             RESID(N,2) : residuals on X-Y and XY-Z
C-   Errors : If sum of weights is .LE. 0., all parameters = 0.
C-            If angle is not defined, return angle = 0.
C-            If Error is infinite,    return error = 99.
C-
C-   Created  23-JUL-1987   Olivier Callot
C-   Updated  18-FEB-1990   Qizhong Li-Demarteau  added VZERO to PARFIT 
C-                                    and fixed a bug in error on theta
C-   Updated   2-AUG-1990   Qizhong Li-Demarteau throw bad Z hits in fitting 
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated  24-OCT-1991   Qizhong Li-Demarteau  added an input MINZ 
C-   Updated  24-AUG-1992   Qizhong Li-Demarteau  recalculate errors 
C-                                        and added correlation term 
C-   Updated  28-OCT-1992   Domenico Pizzuto  Added flag to calculate Z
C-                                            Chi**2 according to measured
C-                                            error function.
C-   Updated  25-MAR-1993   Qizhong Li-Demarteau  introduced CDZCHI
C-   Updated  22-JUL-1993   Ed Oltman  FIX BUG IN R-Z ERROR TERMS 
C-   Updated  14-SEP-1994   Stefano Lami  Introduced a track-fit error
C-                          if errors(drift distance) are used (SWDERR=TRUE)
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER N
      REAL XI(*), YI(*), ZI(*), WR(*), WZ(*), RESID(N,2)
      REAL S0, SX, SY, SX2, SXY, SY2, S1, SZ, NUM, DEN, SLEN, SMS
      REAL CHISQ(2)
      REAL CPHI, SPHI, TANT, PARFIT(10), PHI, ZERRSC, DLTHSL
      REAL    ABSRES(8), CHIDGF, CDZCH2, CDZCHI
      REAL    TEMP, ERRSEG
      INTEGER NDEGF(2), INDEX(8), BADZID(8), MINZ, IER, J
      INTEGER I, FIRSPL
      LOGICAL FIRST, DLERRS, SWDERR, MCDATA
      LOGICAL EZERROR
      SAVE FIRST
      DATA FIRST/.TRUE./
      DATA MCDATA/.FALSE./
      DATA ZERRSC /1.0/
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        IF(IQ(LHEAD+1).GT.1000) MCDATA=.TRUE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','CDXYZL',
     &      'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('CDZCH2',CDZCH2,IER)
        CALL EZGET('CDZCHI',CDZCHI,IER)
        IF(.NOT. MCDATA) THEN
          CALL EZGET_l('DLERRS',DLERRS,IER)
          CALL EZGET('DLTHSL',DLTHSL,IER)
          CALL EZGET_l('SWDERR',SWDERR,IER)
          CALL EZGET('ERRSEG',ERRSEG,IER)
        ENDIF
        CALL EZRSET
      ENDIF
C
      FIRSPL = 0
      CALL VZERO(PARFIT(1), 10)
C
C ****  First, fit in the X-Y plane
C
      S0 = 0.
      SX = 0.
      SY = 0.
      DO 10 I = 1, N
        IF( WR(I) .GT. 0. ) THEN
          IF( FIRSPL .EQ. 0 ) FIRSPL = I
          IF(SWDERR) WR(I) = 1./(1./WR(I) + ERRSEG**2)
          S0 = S0 + WR(I)
          SX = SX + XI(I) * WR(I)
          SY = SY + YI(I) * WR(I)
        ENDIF
   10 CONTINUE
      IF (S0 .EQ. 0) GOTO 999
      PARFIT(1) = SX/S0
      PARFIT(2) = SY/S0
      PARFIT(6) = 1./SQRT( S0 )
      SX2 = 0.
      SXY = 0.
      SY2 = 0.
      DO 20 I = 1, N
        IF( WR(I) .LE. 0. ) GOTO 20
        XI(I) = XI(I) - PARFIT(1)               ! NOW, RELATIVE X's and y'S
        YI(I) = YI(I) - PARFIT(2)
        SX2 = SX2 +  XI(I) * XI(I) * WR(I)
        SXY = SXY +  XI(I) * YI(I) * WR(I)
        SY2 = SY2 +  YI(I) * YI(I) * WR(I)
   20 CONTINUE
      PHI = .5 * ATAN2( 2.*SXY, SX2-SY2 )
C
C ****  Phi has to be between 0 and 2*pi. The previous value is between
C ****  -pi/2 and +pi/2. We choose the direction of the track by reference
C ****  to the first point, and choose X or Y as most significative:
C
C ****  If X, then: if x1>x0, then: phi = phi+pi.
C ****                        else:  if phi<0, phi = phi + pi
C ****        else: if phi<0 : phi = phi + pi
C ****              if y1>y0 : phi = phi + pi
C
C ****  Like this, phi is always between 0 and 2*pi, in the track direction.
C
      IF ( SX2 .GT. SY2 ) THEN
        IF( XI(FIRSPL) .GT. PARFIT(1) ) THEN
          PHI = PHI + PI
        ELSE
          IF ( PHI .LT. 0 ) PHI = PHI + 2.*PI
        ENDIF
      ELSE
        IF( PHI        .LT.  0        ) PHI = PHI + PI
        IF( YI(FIRSPL) .GT. PARFIT(2) ) PHI = PHI + PI
      ENDIF
      PARFIT(5) = PHI
      PARFIT(9) = 1./SQRT( SX2+SY2 )
      CPHI = COS( PARFIT(5) )
      SPHI = SIN( PARFIT(5) )
      CHISQ(1) = 0.
      DO 30 I = 1, N
        IF ( WR(I) .GT. 0. ) THEN
          RESID(I,1) = XI(I)*SPHI - YI(I)*CPHI
          CHISQ(1)   = CHISQ(1) + WR(I) * RESID(I,1)**2
        ELSE
          RESID(I,1) = 0.
        ENDIF
   30 CONTINUE
C
C- Error scale factor for computing Chi**2
      IF (DLERRS) ZERRSC = (1+DLTHSL*ABS(PARFIT (4)-PI/2.))**2
C
C ****  Then computes PARFIT(3) = Z0
C
  201 S1 = 0.
      SZ = 0.
      SMS = 0.
      PARFIT(8) = 99.0
      PARFIT(10) = 99.0
      DO 110 I= 1, N
        IF( WZ(I) .LE. 0. ) GOTO 110
        S1 = S1 +         WZ(I)
        SZ = SZ + ZI(I) * WZ(I)
        SMS = SMS + ( XI(I)*CPHI + YI(I)*SPHI ) * WZ(I)
  110 CONTINUE
      IF ( S1 .LE. 0.) THEN
        PARFIT(3) = 0.
        PARFIT(7) = 99.
      ELSE
        PARFIT(3) = SZ/S1
        PARFIT(7) = 1./SQRT( S1 )
      ENDIF
C
C ****  Now, computes theta, we have to compute various sums
C
      NUM = 0.
      DEN = 0.
      DO 130 I = 1, N
        IF( WZ(I) .LE. 0. ) GOTO 130
        SLEN = XI(I) * CPHI + YI(I) * SPHI - SMS/S1
        NUM = NUM + SLEN *            SLEN     * WZ(I)
        DEN = DEN + SLEN * ( ZI(I)-PARFIT(3) ) * WZ(I)
  130 CONTINUE
C
C ****  protections, will never happen ??
C
      IF ( NUM .LE. 0. .AND. DEN .EQ. 0.) THEN
        PARFIT(4) = 0.
        TANT      = 0.
        PARFIT(8) = 99.
      ELSE
C ---------------- NUM is >0 => teta is between 0 and pi
        PARFIT(4) = ATAN2( NUM, DEN ) 
        TANT      = DEN / NUM
        IF( NUM .NE. 0. ) PARFIT(8) = SIN(PARFIT(4))**2 / SQRT(ABS(NUM))
      ENDIF
C
C ****  Compute PARFIT(3) at the PARFIT(1),PARFIT(2) point 
C ****  and its errors
C
      IF (S1 .GT. 0) THEN
        PARFIT(3) = ( SZ - SMS * TANT ) / S1
        IF (PARFIT(4) .GT. 0) THEN
          TEMP = PARFIT(8) * SMS / (S1 * SIN(PARFIT(4))**2)
          PARFIT(10) = PARFIT(8) * TEMP
        ENDIF
      ENDIF
      IF (PARFIT(4) .GT. 0 .AND. PARFIT(7) .LT. 99.0)
     &  PARFIT(7) = SQRT(PARFIT(7)**2 + TEMP**2)
C
      CHISQ(2) = 0.
      J = 0
      CALL VZERO_i(BADZID(1),8)
      CALL VZERO(ABSRES(1),8)
      CALL VZERO_i(INDEX(1),8)
      DO 40 I = 1, N
        RESID(I,2) = 999.
        IF( WR(I) .LE. 0. ) GOTO 40
        IF( WZ(I) .GT. 0. ) THEN
          RESID(I,2) = ZI(I)-PARFIT(3) - TANT*(XI(I)*CPHI + YI(I)*SPHI )
          J = J + 1
          BADZID(J) = I
          ABSRES(J) = ABS(RESID(I,2))
          CHISQ(2) = CHISQ(2) + RESID(I,2)**2 * (WZ(I)/ZERRSC)
        ENDIF
   40 CONTINUE
C
C   to get rid of bad Z hits in the fitting
C
      IF (NDEGF(2) .GT. 0) THEN
        CHIDGF = CHISQ(2) / NDEGF(2)
        IF (CHIDGF .GT. CDZCHI .AND. NDEGF(2) .GT. MINZ) THEN
          CALL SORTZV(ABSRES,INDEX,8,1,1,0)
          WZ(BADZID(INDEX(1))) = 0.0
          NDEGF(2) = NDEGF(2) - 1
          GOTO 201
        ENDIF
      ENDIF
      IF (CHISQ(2) .GT. 9999.0) CHISQ(2) = 9999.0
      IF (CHISQ(2) .GT. CDZCH2*NDEGF(2) ) THEN
        PARFIT(3) = 0.
        PARFIT(4) = 0.
        PARFIT(7) = 9999.
        PARFIT(8) = 99.
      ENDIF
C
      DO 41 I = 1, N
        XI(I) = XI(I) + PARFIT(1)              ! restore X, Y
        YI(I) = YI(I) + PARFIT(2)
   41 CONTINUE
C
  999 RETURN
      END
