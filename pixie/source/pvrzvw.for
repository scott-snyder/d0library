      SUBROUTINE PVRZVW
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display r-z view of VTX
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  29-MAR-1990   Peter M. Grudberg
C-   Updated  02-AUG-1991   An-Dien Nguyen Adding track numbers    
C        parallel version   S. Hagopian  theta road, PUOPEN, JRCLOS
C-   Updated  16-AUG-1991   T. Trippe  combine versions
C-   Updated  18-MAR-1993   Alexandre Zinchenko - handle compressed hits
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      REAL RLAY(0:3), ZLAY(0:3)
      INTEGER NV, NVMAX
      PARAMETER ( NVMAX = 5 )
      REAL ZVER(NVMAX), DZVER(NVMAX)
      REAL PHI1, PHI2, PHI3, PHI4, MED, X, Y
      INTEGER IFVHIT, IFVTRK, DRWTRK, DRWHTS, I, LVERH, GZVERH
      INTEGER IFVRTX, IFBEAM,TYP,IER,IFVLBL
      CHARACTER*35 TEXT
      CHARACTER*4 CVAL, REM
      LOGICAL FIRST, VTONLY, EZERROR
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
C ****  get geometry values from STP
C
        IF ( LVGEH .LE. 0 ) GO TO 999   ! STP banks not defined
        DO I = 0, 3
          IF ( I .EQ. 3 ) THEN
            RLAY(I) = C( LVGEH + 16 )
            ZLAY(I) = ZLAY(2)
          ELSE
            RLAY(I) = C( LVGEH + 11 + 2 * I )
            ZLAY(I) = C( LVGEH + 17 + 2 * I )
          ENDIF
        ENDDO
      ENDIF
C
C ****  Get phi limits for plot
C
      CALL PVGPHI( PHI1, PHI2, PHI3, PHI4 )
C
C
C ****  Picking PIXIE RCP
C
      CALL EZPICK('PX_VTXDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PVRZVW','Cannot find PX_VTXDIS_RCP','W')
        GOTO 999
      ENDIF
C
C ****  Get some VTX constants
C
      CALL EZ_GET_ARRAY('PXPARAMS','VTX DRAW HITS',1,IFVHIT,
     &       CVAL,TYP,REM,IER)
      CALL EZ_GET_ARRAY('PXPARAMS','VTX DRAW TRACKS',1,IFVTRK,
     &       CVAL,TYP,REM,IER)
      CALL EZ_GET_ARRAY('PXPARAMS','VTX DRAW LABEL',1,IFVLBL,
     &       CVAL,TYP,REM,IER)
      CALL EZ_GET_ARRAY('PXPARAMS','VTX ONLY',1,VTONLY,
     &       CVAL,TYP,REM,IER)
      CALL EZ_GET_ARRAY('PXPARAMS','VTX DRAW VERTEX',1,IFVRTX,
     &       CVAL,TYP,REM,IER)
      CALL EZ_GET_ARRAY('PXPARAMS','VTX DRAW BEAMLINE',1,IFBEAM,
     &       CVAL,TYP,REM,IER)
C
C ****  Reseting RCP file
C
      CALL EZRSET
C open graphics segment
      CALL PUOPEN
C
C ****  Draw the beam line
C
      IF ( IFBEAM .GT. 0 ) THEN
        CALL PXCOLR('FOR')
        CALL JMOVE( -80., 0. )
        CALL JDRAW( 80., 0. )
        CALL JMOVE( 0., 1. )
        CALL JDRAW( 0., -1. )
      ENDIF
C
C ****  Now draw the chamber outline
C
      CALL PXCOLR('BLU')
      CALL JMOVE( ZLAY(0), RLAY(0) )
      CALL JDRAW( -ZLAY(0), RLAY(0) )
      CALL JDRAW( -ZLAY(0), RLAY(1) )
      CALL JDRAW( -ZLAY(1), RLAY(1) )
      CALL JDRAW( -ZLAY(1), RLAY(2) )
      CALL JDRAW( -ZLAY(2), RLAY(2) )
      CALL JDRAW( -ZLAY(2), RLAY(3) )
      CALL JDRAW( ZLAY(2), RLAY(3) )
      CALL JDRAW( ZLAY(2), RLAY(2) )
      CALL JDRAW( ZLAY(1), RLAY(2) )
      CALL JDRAW( ZLAY(1), RLAY(1) )
      CALL JDRAW( ZLAY(0), RLAY(1) )
      CALL JDRAW( ZLAY(0), RLAY(0) )
C
      CALL JMOVE( ZLAY(0), -RLAY(0) )
      CALL JDRAW( -ZLAY(0), -RLAY(0) )
      CALL JDRAW( -ZLAY(0), -RLAY(1) )
      CALL JDRAW( -ZLAY(1), -RLAY(1) )
      CALL JDRAW( -ZLAY(1), -RLAY(2) )
      CALL JDRAW( -ZLAY(2), -RLAY(2) )
      CALL JDRAW( -ZLAY(2), -RLAY(3) )
      CALL JDRAW( ZLAY(2), -RLAY(3) )
      CALL JDRAW( ZLAY(2), -RLAY(2) )
      CALL JDRAW( ZLAY(1), -RLAY(2) )
      CALL JDRAW( ZLAY(1), -RLAY(1) )
      CALL JDRAW( ZLAY(0), -RLAY(1) )
      CALL JDRAW( ZLAY(0), -RLAY(0) )
C ****  Draw all r-z hits if requested
C
CC      IF ( IFVHIT .GE. 3 ) THEN
        CALL PVRZHT( PHI1, PHI2, PHI3, PHI4 )
CC        IFVHIT = 0                    ! avoid redrawing hits
CC      ENDIF
      IF ( .NOT. VTONLY ) THEN
C
C ****  If plotting the full detector, only draw tracks within the calorimeter
C ****  defined roads (the phi range returned by PVGPHI), and only draw hits
C ****  from wires 0 or 7 associated with those tracks.
C
        DRWHTS = IFVHIT      ! Normally = 1 ==> only wire 0, 7 hits
        DRWTRK = MAX(IFVTRK, 1)
        CALL PVRZTRK( PHI1, PHI2, PHI3, PHI4, DRWTRK, DRWHTS, IFVLBL)
      ELSE
C
C ****  Draw R-Z tracks if requested
C
        IF ( IFVTRK .GT. 0 ) THEN
          DRWHTS = IFVHIT
          CALL PVRZTRK( PHI1, PHI2, PHI3, PHI4, IFVTRK, DRWHTS, IFVLBL)
        ENDIF
      ENDIF
C
C ****  Mark vertex (if found)
C
      IF ( IFVRTX .GT. 0 ) THEN
        LVERH = GZVERH()
        IF ( LVERH .LE. 0 ) GO TO 900
        CALL ZVERTE(NV, ZVER, DZVER)
        IF ( NV .EQ. 0 ) GO TO 900
        CALL PXCOLR('GRE')
        CALL JSIZE(2.2,2.0)
        CALL JFONT(5)
        CALL JJUST(2,2)
        CALL JCMARK(1)
        X = 2. * DZVER(1)
        Y = .20
        CALL J3MOVE(ZVER(1)-DZVER(1),-.10,0.)
        CALL JRRECT(X,Y)
        MED = ZVER(1)
        CALL J3MOVE(MED,0.,0.)
        CALL JHSTRG('X')
      ENDIF
  900 CALL JRCLOS
C
  999 RETURN
      END
