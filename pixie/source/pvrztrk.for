      SUBROUTINE PVRZTRK( PHI1, PHI2, PHI3, PHI4,DRWTRK, DRWHTS,DRWLBL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw r-z tracks and track numbers for the
C-                         VTX r-z event display.  If DRWHTS>0, then
C-                         also draw some or all of the associated
C-                         hits.  Tracks are normally drawn from first
C-                         wire hit to last wire hit, but if DRWTRK > 1,
C-                         then the tracks are extended in to the beamline and
C-                         out to the edge of the chamber.
C-
C-   Inputs  : PHI1, PHI2 = phi limits for upper part of display (PHI2>PHI1)
C-             PHI3, PHI4 = phi limits for lower part of display
C-             DRWTRK [I] = flag controlling drawing of tracks
C-                              <= 1: draw tracks from 1st to last wire hit
C-                              >1  : extend tracks to beam line and chamber
C-                                    edge
C-             DRWHTS [I] = flag controlling drawing of hits
C-                              = 0 : don't draw associated hits
C-                              = 1 : draw associated hits for wires 0, 7
C-                              >= 2: draw all associated wire hits
C-             DRWLBL [I] = flag controlling drawing of track numbers
C-                              = 0 : no track number
C-                              = 1 : draw track number
C-                              = 2 : rz phi cut and scale
C-                              = 3 : all
C-
C-   Outputs :
C-   Controls:
C-
C-   Created   5-APR-1990   Peter Grudberg
C-   Updated  24-SEP-1990   Lupe Howell  Implementing PIXIE_RCP
C-   Updated  02-AUG-1991   An-Dien Nguyen Adding track numbers
C-       parallel version   S. Hagopian removed PUOPEN, JRCLOS
C-   Updated  16-AUG-1991   T. Trippe  combine versions
C-   Updated   7-SEP-1991   S. Hagopian, changed from ZTRAKS.RCP
C-                                       to ZTRAKS.PARAMS
C-   Updated  15-JAN-1992   Nobuaki Oshima
C-                     Change PHICEN handling for global PHI selection.
C-   Updated  30-MAR-1992   Lupe Howell  Cleanup remove Machine block 
C-   Updated  29-MAY-1992   Peter M. Grudberg  Handle case where there are no z
C-                          hits on a track 
C-   Updated  21-OCT-1992   Peter M. Grudberg  Use new VTXT format
C-   Updated  10-MAY-1993   Alexandre Zinchenko - check PHI of center of
C-            gravity of tracks instead of track PHI (to find sign of RPOS)
C-   Updated  11-JUN-1993   A. Zinchenko - check NHIT returned from VTXTHT
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$LINKS:IZVTXT.LINK'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS/LIST'
C
      REAL PHI1, PHI2, PHI3, PHI4, IMPCUT, CHICUT, CHISQZ
      REAL RVTXLO, RVTXHI, RWIR(0:7,0:2)
      REAL XCEN, YCEN, ZCEN, SCEN, RCEN, PHICEN, THETA, PHITRK
      REAL RTRKHI, RTRKLO, TRKLEN, IMPACT, ZMAX
      REAL RPOS1, RPOS2, ZPOS1, ZPOS2, RPOS, ZPOS, ZERR
      REAL ZTRKNM, RTRKNM, ROFF, ZOFF, ROF, ZOF
      REAL HITX(24), HITY(24), HITZ(24), WR(24), WZ(24)
      REAL XMIN, XMAX, YMIN, YMAX, XSIZ, YSIZ, SIZSCAL
      REAL PHICHK1, PHICHK4
      INTEGER DRWHTS, LVRFT, GZVRFT, LAY, WIR, NHIT
      INTEGER LVTRH, GZVTRH, LVTXT, INDEX, NXV, IXV(32)
      INTEGER IWFIRS, IWLAST, ILFIRS, ILLAST, DRWTRK
      INTEGER ITRK, DRWLBL
      CHARACTER*4 CVAL, REM, TRKNUM
      INTEGER IER,TYP
      LOGICAL IDEBUG
      LOGICAL INEL,INMU,INTAU,INVEE
      LOGICAL FIRST,EZERROR
      DATA FIRST / .TRUE. /
C-----------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C-
C--- For global PHI selection
        IF ( PHI1 .LT. 0. ) THEN
          PHICHK1 = PHI1 + TWOPI
          PHICHK4 = PHI4
        ELSEIF ( PHI4 .GT. TWOPI ) THEN
          PHICHK4 = PHI4 - TWOPI
          PHICHK1 = PHI1
        ENDIF
C-
C
C ****  Get geometry values from STP
C
        IF ( LVGEH .LE. 0 ) GO TO 999   ! geometry not defined
        RVTXLO = C( LVGEH + 9 )
        RVTXHI = C( LVGEH + 10 )
        ZMAX = C( LVGEH + 21 )
        LVRFT = GZVRFT()
        DO LAY = 0, 2
          DO WIR = 0, 7
            RWIR(WIR,LAY) = C( LVRFT + 7 + 7*LAY ) +
     &                      C( LVRFT + 23 + WIR )
          ENDDO
        ENDDO
      ENDIF
C
C ****  Picking PIXIE RCP
C
      CALL EZPICK('PX_VTXDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PVRZTRK','Cannot find PX_VTXDIS_RCP','W')
        GOTO 999
      ENDIF
C
C ****  Get some VTX constants
C
      CALL EZ_GET_ARRAY('PXPARAMS','VTX IMPACT CUT',1,IMPCUT,
     &       CVAL,TYP,REM,IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('PIXIE','PVRZTRK',
     &      'PXPARAMS NOT FOUND','W')
        GOTO 995
      ENDIF
      CALL EZ_GET_ARRAY('PXPARAMS','VTX CHISQZ CUT',1,CHICUT,
     &       CVAL,TYP,REM,IER)
C
      LVTRH = GZVTRH()
      IF ( LVTRH .LE. 0 ) GO TO 995     ! no tracks
C
      ROF   = 2.0
      ZOF   = 0.0
C
      LVTXT = LQ( LVTRH - IZVTXT )  ! get struct. link to track bank
   10 CONTINUE
      IF ( LVTXT .LE. 0 ) GO TO 900     ! no more tracks
C
C ****  Does this track have rz track info?  If not, skip it.
C
      THETA = Q( LVTXT + 9 )
      IF(THETA.EQ.0.0)GO TO 20
C set color for tracks in roads depending on particle TYPE
      INMU=(IBITS(IQ(LVTXT),MUROAD,1).EQ.1)
      INEL=(IBITS(IQ(LVTXT),ELROAD,1).EQ.1)
      INTAU=(IBITS(IQ(LVTXT),TAUROAD,1).EQ.1)
      INVEE=(IBITS(IQ(LVTXT),9,1).EQ.1)
      CALL PXCOLR('FOR')
      IF(INMU)CALL PXCOLR('GRE')
      IF(INEL)CALL PXCOLR('RED')
      IF(INTAU)CALL PXCOLR('CYA')
      IF(INVEE)CALL PXCOLR('YEL')
      XCEN = Q( LVTXT + 7 )
      YCEN = Q( LVTXT + 8 )
      PHICEN = ATAN2( YCEN, XCEN )
      IF ( PHICEN .LT. 0 ) PHICEN = PHICEN + TWOPI
C
C ****  If the center of the track in r-phi is outside the selected phi region,
C ****  skip it.  Also skip tracks that don't point to the beam line within the
C ****  given tolerance.
C-
C--- Check for PHI1<0 or PHI4>2pi here... ( Nobu. 15-JAN-1992 )
C-
      IF ( PHICEN.GT.PHI2 .AND. PHICEN.LT.PHI3 )         GO TO 20
      IF ( PHI1.LT.0. .OR. PHI4.GT.TWOPI ) THEN
        IF ( PHICEN.GT.PHICHK4 .AND. PHICEN.LT.PHICHK1 ) GO TO 20
C-
C---        0 < PHI1 < PHI2 < PHI3 < PHI4 < TWOPI
      ELSE
        IF ( PHICEN.LT.PHI1 .OR.  PHICEN.GT.PHI4 )       GO TO 20
      ENDIF
C---
C-
      RCEN = SQRT( XCEN**2 + YCEN**2 )
      PHITRK = Q( LVTXT + 6 )
      IMPACT = ABS( RCEN * SIN(PHICEN - PHITRK) )
      IF ( IMPACT .GT. IMPCUT ) GO TO 20
      CHISQZ = Q( LVTXT + 13 )
      IF ( (CHISQZ/IQ(LVTXT + 14)) .GT. CHICUT ) GO TO 20
C
C ****  plot the track!
C
      IF ( DRWTRK .GE. 2 ) THEN
        RTRKHI = RVTXHI
        RTRKLO = 0.
      ELSE                  ! draw from first to last wire hit
        CALL UBITS(IQ(LVTXT + 4), 32, IXV, NXV)
        IF ( NXV .LE. 0 ) GO TO 20  ! Protect against no z hits
        IWFIRS = IXV(1) - 1
        IWLAST = IXV(NXV) - 1
        ILFIRS = IWFIRS / 8
        ILLAST = IWLAST / 8
        IWFIRS = MOD(IWFIRS, 8)
        IWLAST = MOD(IWLAST, 8)
        RTRKHI = RWIR(IWLAST, ILLAST) + 0.2
        RTRKLO = RWIR(IWFIRS, ILFIRS) - 0.2
      ENDIF
      ZCEN = Q( LVTXT + 11 )
      TRKLEN = (RTRKHI - RCEN) / SIN(THETA)
      ZPOS1 = ZCEN + TRKLEN * COS(THETA)
      RPOS1 = RTRKHI
      ROFF  = ROF
      ZOFF  = ZOF
      IF ( ABS(ZPOS1) .GT. ZMAX ) THEN
        IF ( ZPOS1 .GT. 0. ) THEN
          ZPOS1 = ZMAX
        ELSE
          ZPOS1 = - ZMAX
        ENDIF
        TRKLEN = (ZPOS1 - ZCEN) / COS(THETA)
        RPOS1 = RCEN + TRKLEN * SIN(THETA)
      ENDIF
      TRKLEN = (RTRKLO - RCEN) / SIN(THETA)
      ZPOS2 = ZCEN + TRKLEN * COS(THETA)
      RPOS2 = RTRKLO
      IF ( ABS(ZPOS2) .GT. ZMAX ) THEN
        IF ( ZPOS2 .GT. 0. ) THEN
          ZPOS2 = ZMAX
        ELSE
          ZPOS2 = - ZMAX
        ENDIF
        TRKLEN = (ZPOS2 - ZCEN) / COS(THETA)
        RPOS2 = RCEN + TRKLEN * SIN(THETA)
      ENDIF
C-
C--- For global PHI selection ( It was only "IF ( PHITRK .GT. PI ) THEN" )
C-
      IF ( PHI4 .GT. TWOPI ) THEN
        IF ( PHICEN.GT.PHI3 .OR. PHICEN.LT.PHICHK4 ) THEN
          RPOS1 = - RPOS1
          RPOS2 = - RPOS2
          ROFF  = - ROF
        ENDIF
      ELSE
        IF ( PHICEN.GT.PHI3 .AND. PHICEN.LT.PHI4 ) THEN
          RPOS1 = - RPOS1
          RPOS2 = - RPOS2
          ROFF  = - ROF
        ENDIF
      ENDIF
C---
C
C  *** track number position
C
      ZTRKNM = ZPOS1 + ZOFF
      RTRKNM = RPOS1 + ROFF
C
C      get track number values
C
      ITRK = IQ(LVTXT -5)
      IF (ITRK .LT. 10) THEN
        WRITE (TRKNUM, 1001) ITRK
 1001   FORMAT (1X, I1, 2X)
      ELSE
        IF (ITRK .LT. 100) THEN
          WRITE (TRKNUM, 1002) ITRK
 1002     FORMAT  (1X, I2, 1X)
        ELSE
          WRITE (TRKNUM, 1003) ITRK
 1003     FORMAT  (1X, I3)
        ENDIF
      ENDIF
C
      CALL JMOVE( ZPOS1, RPOS1 )
      CALL JDRAW( ZPOS2, RPOS2 )
C
C  *** draw track number
C
      CALL J4RGET(1, XMIN, XMAX, YMIN, YMAX)
      SIZSCAL = .015
      XSIZ = (XMAX-XMIN)*SIZSCAL
      YSIZ = (YMAX-YMIN)*SIZSCAL
      IF (MOD(DRWLBL,2) .NE. 0) THEN
        CALL JJUST(2,2)
        CALL JMOVE(ZTRKNM, RTRKNM)
        CALL JSIZE(XSIZ, YSIZ)
        CALL J1STRG (TRKNUM)
      ENDIF
C
C ****  Draw the associated hits if requested
C
      IF ( DRWHTS .LE. 0 ) GO TO 20
      CALL VTXTHT(LVTXT, NHIT, HITX, HITY, HITZ, WR, WZ)
      IF (NHIT.LE.0) GO TO 20
      DO LAY = 0, 2
        DO 30 WIR = 0, 7
          INDEX = 8 * LAY + WIR + 1
          IF ( WZ(INDEX) .EQ. 0. ) GO TO 30     ! wire not used in fit
          IF ( DRWHTS .EQ. 1 ) THEN
            IF(WIR.GT.0.AND.WIR.LT.7)GO TO 30
          ENDIF
          RPOS = SQRT( HITX(INDEX)**2 + HITY(INDEX)**2 )
          IF ( PHITRK .GT. PI ) RPOS = - RPOS
          ZPOS = HITZ(INDEX)
          ZERR = SQRT( 1 / WZ(INDEX) )
          CALL JMOVE(ZPOS - ZERR/2., RPOS)
          CALL JDRAW(ZPOS + ZERR/2., RPOS)
   30   CONTINUE
      ENDDO
C
C ****  Find next track
C
   20 CONTINUE
      LVTXT = LQ(LVTXT)
      GO TO 10
C
  900 CONTINUE
C
C ****  Reseting RCP file
C
  995 CALL EZRSET
  999 RETURN
      END
