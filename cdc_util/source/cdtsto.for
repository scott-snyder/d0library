      SUBROUTINE CDTSTO( LISTPT )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fit and store track segment
C-
C-   Inputs  : LISTPT( 0:NBSENS-1 ) List of hit number in HITINF
C-   Outputs :
C-
C-   Created  18-SEP-1987   Olivier Callot
C-   Updated   9-MAY-1988   Olivier Callot  Tag by number of wires
C-   Updated  30-MAR-1989   Qizhong Li-Demarteau    use SRCP 
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:DDEBUG.INC'
      INCLUDE 'D0$INC:CDLOCA.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
      INCLUDE 'D0$INC:CDLTRK.INC'
C
      INTEGER LISTPT( 0:MXSENS )
      REAL    X0, Y0, ERRD, PHI, ERRPHI, CHISQ, RESID( 0:MXSENS )
      REAL    XI(0:MXSENS), YI(0:MXSENS), WI(0:MXSENS)
      REAL    S0, SX, SY, SX2, SXY, SY2, CPHI, SPHI, PI, CDMXCH
      INTEGER  LABL, I, FIRSPL, LASTPL, NDEGF, JP, INIT, ERR
      INTEGER LTRSEG
      PARAMETER( LTRSEG= 8 + 2*NBSENS)
      REAL    TRASEG( LTRSEG )
      INTEGER KTRSEG( LTRSEG )
      EQUIVALENCE ( TRASEG(1), KTRSEG(1) )
      INTEGER IER
      LOGICAL EZERROR
      SAVE INIT
      DATA INIT / 0 /
C----------------------------------------------------------------------
      IF ( INIT .EQ. 0 ) THEN
        INIT = 1
        PI = ACOS( -1. )
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','CDTSTO',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('CDMXCH',CDMXCH,ERR)
        CALL EZRSET
      ENDIF
C
      FIRSPL = -1
      LASTPL = 0
      NDEGF = -2.
      S0 = 0.
      SX = 0.
      SY = 0.
      DO 10 I = 0, MXSENS
        IF( LISTPT(I) .NE. 0 ) THEN
          LASTPL = I
          IF( FIRSPL .LT. 0 ) FIRSPL = I
          JP = LHITL(I) + 7*(LISTPT(I)-1) + 1
          XI(I) = Q( JP+5 )
          YI(I) = Q( JP+6 )
          WI(I) = Q( JP+3 )
          S0 = S0 + WI(I)
          SX = SX + XI(I) * WI(I)
          SY = SY + YI(I) * WI(I)
          NDEGF = NDEGF + 1
        ELSE
          WI(I) = 0.
        ENDIF
   10 CONTINUE
      X0 = SX/S0
      Y0 = SY/S0
      ERRD = 1./SQRT( S0 )
      SX2 = 0.
      SXY = 0.
      SY2 = 0.
      DO 20 I = FIRSPL, LASTPL
        IF( WI(I) .LE. 0. ) GOTO 20
        SX2 = SX2 + ( XI(I)-X0 ) * ( XI(I)-X0 ) * WI(I)
        SXY = SXY + ( XI(I)-X0 ) * ( YI(I)-Y0 ) * WI(I)
        SY2 = SY2 + ( YI(I)-Y0 ) * ( YI(I)-Y0 ) * WI(I)
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
        IF( XI(FIRSPL) .GT. X0 ) THEN
          PHI = PHI + PI
        ELSE
          IF ( PHI .LT. 0 )      PHI = PHI + 2.*PI
        ENDIF
      ELSE
        IF( PHI        .LT.  0 ) PHI = PHI + PI
        IF( YI(FIRSPL) .GT. Y0 ) PHI = PHI + PI
      ENDIF
C
      ERRPHI = 1./SQRT( SX2+SY2 )
      CPHI = COS(PHI)
      SPHI = SIN(PHI)
      CHISQ = 0.
      DO 30 I = 0, MXSENS
        IF ( WI(I) .GT. 0. ) THEN
          RESID(I) = (XI(I)-X0)*SPHI - (YI(I)-Y0)*CPHI
          CHISQ    = CHISQ + WI(I) * RESID(I)**2
        ELSE
          RESID(I) = 0.
        ENDIF
   30 CONTINUE
      IF( DBGFLG .AND. LVLDBG(6) .GE. 2 ) THEN
        WRITE( LUNDBG, 1500 ) X0, Y0, ERRD, PHI, ERRPHI, CHISQ, NDEGF
        WRITE( LUNDBG, 1510 ) LISTPT
        WRITE( LUNDBG, 1520 ) RESID
 1500   FORMAT(10X,'X0,Y0 =',2F10.4,' +- ',F10.4,' Phi +- ',2F10.6,
     &                   ' chisq ',F10.3,'   ndeg ',I4)
 1510   FORMAT( 15X, 10I10  )
 1520   FORMAT( 15X, 10F10.4)
      ENDIF
C
C ****  Test the quality of the track
C
      IF ( CHISQ .GT. CDMXCH * NDEGF ) THEN
        IF ( DBGFLG .AND. LVLDBG(6) .GE. 2 ) THEN
          WRITE( LUNDBG, 1530 )
 1530     FORMAT(5X,'--- Chisq too big ---')
        ENDIF
        GOTO 999
      ENDIF
C
C ****  Store the track segment
C
      KTRSEG( 1 ) = 0         ! status
      KTRSEG( 2 ) = NDEGF
      TRASEG( 3 ) = X0
      TRASEG( 4 ) = Y0
      TRASEG( 5 ) = PHI
      TRASEG( 6 ) = ERRD
      TRASEG( 7 ) = ERRPHI
      TRASEG( 8 ) = CHISQ
      DO 300 WIRE = 0, MXSENS
        IF ( LISTPT( WIRE ) .NE. 0 ) THEN
          JP = LHITL(WIRE) + 7*(LISTPT(WIRE)-1) + 1
          KTRSEG( 9+WIRE ) = IQ( JP+4 )
          TRASEG( 9+NBSENS+WIRE ) = RESID( WIRE )
        ELSE
          KTRSEG( 9+WIRE ) = 0
          TRASEG( 9+NBSENS+WIRE ) = 0.
        ENDIF
  300 CONTINUE
      CALL ZFDTSG( TRASEG, LTRSEG )
C
C ****  Tag hit ( and hit image ) as being used ( no future seed ) in LHITL
C
      DO 145 WIRE = FIRSPL, LASTPL
        IF( LISTPT(WIRE) .NE. 0 ) THEN
          JP = LHITL(WIRE) + 7*(LISTPT(WIRE)-1) + 1
          IF( IQ( JP+7 ).NE. 0 ) GOTO 145
          LABL = IQ( JP+4 )
          IQ( JP+7 ) = NDEGF+2
          LABL = LABL + 1
          IF( .NOT. BTEST( LABL, 0 ) ) LABL = LABL - 2
          DO 146 I = 1, IQ( LHITL(WIRE)+1 )
            JP = LHITL(WIRE) + 7*(I-1) + 1
            IF( IQ(JP+4) .EQ. LABL ) THEN
              IQ( JP+7 ) = NDEGF+2
              GOTO 145
            ENDIF
  146     CONTINUE
        ENDIF
  145 CONTINUE
  999 RETURN
      END
