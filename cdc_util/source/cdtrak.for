      SUBROUTINE CDTRAK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Associates segments to build a full track.
C-              Then, fit the track and store it
C-
C-   Inputs  : none ( banks DTSG, DSEC )
C-   Outputs : none (bank DTRKs)
C-
C-   Created  30-OCT-1987   Olivier Callot
C-   Updated  30-MAR-1989   Qizhong Li-Demarteau  use SRCP 
C-   Updated  27-JUL-1989   Qizhong Li-Demarteau  use modified DTRH 
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated  14-OCT-1991   Qizhong Li-Demarteau  fill DTRK id in DTSG bank 
C-                                       and added crossing X axis matching
C-   Updated   1-NOV-1991   Qizhong Li-Demarteau  added building "edge tracks" 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:DDEBUG.INC'
      INCLUDE 'D0$INC:CDRESF.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
      INCLUDE 'D0$INC:CDLTRK.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER NBTWIR
      PARAMETER( NBTWIR= NBSENS*4 )
      INTEGER LAYF, LAYL, SEC, SEG, SEGR, SEGL, IPL, IPR, IPF
      INTEGER I, LAY, NUMHIT, ISIDE, KPDSEC, LHIT, J
      INTEGER NDTSG(0:3), NWDTSG
      INTEGER LISPT(0:3), NUSEGM(0:3)
      INTEGER LABEL( NBTWIR ), NFADC, IPHIT, WIR
      INTEGER NMISS, CDMINL, ERR
      INTEGER IER, DTRKID
      REAL    CDTDPH, CDTDIS
      REAL    XR, YR, DY, DELPOS, DELPHI, DELPH1, DELPH2
      REAL    PHIF, XF, YF, PHIL, XL, YL, DIST, DISMIN, S, PHID
      LOGICAL FIRST, EZERROR, EDGE_TRACKING
C
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','CDTRAK',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('CDMINL',CDMINL,ERR)
        CALL EZGET('CDTDPH',CDTDPH,ERR)
        CALL EZGET('CDTDIS',CDTDIS,ERR)
        CALL EZGET('EDGE_TRACKING',EDGE_TRACKING,ERR)
        CALL EZRSET
      ENDIF
C
      CALL VZERO(NDTSG(0),4)
      IF (LDTRH .GT. 0) CALL UCOPY( IQ( LDTRH+3 ), NDTSG(0), 4 )
      NWDTSG = 8 + 2*NBSENS
      IF( DBGFLG .AND. LVLDBG(7) .GE. 5) WRITE( LUNDBG,3000 )
 3000 FORMAT( '0** CDTRAK ** After fit: seg #, x,y,z,tet,phi and',
     &  '  err(d,z,tet,phi)'/)
      DO 110 LAYF = 3, CDMINL-1, -1
        DO 120 SEG = 1, NDTSG(LAYF)
          IPF = LDTSG(LAYF) + 2 + (SEG-1) * NWDTSG
          IF( IQ(IPF+1) .NE. 0 ) GOTO 120      ! Dont reuse
          CALL VZERO( LABEL, NBTWIR )
          CALL VZERO( LISPT, 4)
          CALL VZERO( NUSEGM, 4 )
          NMISS = LAYF+1 - CDMINL
          XF   = Q( IPF+3 )
          YF   = Q( IPF+4 )
          PHIF = Q( IPF+5 )
          LISPT( LAYF) = IPF
          NUSEGM(LAYF) = SEG
          CALL UCOPY( IQ(IPF+9), LABEL(LAYF*NBSENS+1), NBSENS )
          IF ( DBGFLG .AND. LVLDBG(7).GE.3 ) THEN
            WRITE( LUNDBG, 2000 ) LAYF, SEG, PHIF, XF, YF
 2000       FORMAT(/' Start tracking for layer',I2,' segment',I4,
     &              ' Phi, x, y =',F10.6,2F10.4)
          ENDIF
          DO 130 LAYL = LAYF-1, 0, -1
            DISMIN = 1000.
            DO 140 SEGL = 1, NDTSG(LAYL)
              IPL = LDTSG(LAYL) + 2 + (SEGL-1) * NWDTSG
              IF( IQ(IPL+1) .NE. 0 ) GOTO 140
              PHIL = Q( IPL+5 )
              DELPH1 = ABS(PHIL - PHIF)
              DELPH2 = TWOPI - DELPH1
              IF (DELPH1 .GT. CDTDPH .AND. DELPH2 .GT. CDTDPH) GOTO 140
              XL   = Q( IPL+3 )
              YL   = Q( IPL+4 )
              S = .5*SQRT( (XL-XF)**2 + (YL-YF)**2 )
              PHID = ATAN2( YF-YL, XF-XL )
              DIST = S * ( TAN( PHIF-PHID ) + TAN( PHIL - PHID ) )
              IF( ABS(DIST) .LT. DISMIN ) THEN
                DISMIN = ABS(DIST)
                IPR    = IPL
                SEGR   = SEGL
                DELPHI = MIN(DELPH1,DELPH2)
                DELPOS = DIST
              ENDIF
  140       CONTINUE
            IF( DISMIN .GT. CDTDIS ) THEN
              NMISS = NMISS - 1
              IF( NMISS .LT. 0 ) GOTO 120
              GOTO 130
            ENDIF
            IF ( DBGFLG .AND. LVLDBG(7) .GE. 3 ) THEN
              WRITE( LUNDBG, 2100 ) LAYL, SEGR, DELPOS, DELPHI
 2100         FORMAT(20X,'On layer ',I2,' segment',I3,' dist=',F10.4,
     &               ' delphi=',F10.6)
            ENDIF
            NUSEGM(LAYL) = SEGR
            LISPT( LAYL ) = IPR
            PHIF = Q( IPR+5 )
            XF   = Q( IPR+3 )
            YF   = Q( IPR+4 )
            CALL UCOPY( IQ( IPR+9 ), LABEL(LAYL*NBSENS+1), NBSENS )
  130     CONTINUE
C
C ****  OK, we have now a trak, prepare parameters for fit, fit, test if
C ****  good and store if good. Return nusegm = 0 if bad...
C
          CALL CDTFIT(NUSEGM,LABEL,0)
C
C ****  Tag segments, to avoid reusing them later...
C
          DO 131 LAYL = 0, 3
            IF( LISPT( LAYL ) .EQ. 0 ) GOTO 131
            IF( NUSEGM(LAYL ) .EQ. 0 ) GOTO 131
            DTRKID = IQ(LDTRK - 5)
            IF (DTRKID .GT. 0) THEN
              IQ( LISPT(LAYL)+1 ) = IQ(LDTRK-5)
            ELSE
              IQ( LISPT(LAYL)+1 ) = 1
            ENDIF
  131     CONTINUE
  120   CONTINUE
  110 CONTINUE
C
C  edge_tracking when requested
C
      IF (EDGE_TRACKING) CALL DTRK_EDGE
C
C ****  Now the debugging
C
      IF( DBGFLG .AND. LVLDBG(7) .NE. 0 ) THEN
        CALL PRDTRK( LUNDBG, 0, 0, 'ALL', LVLDBG(7) )
      ENDIF
  999 RETURN
      END
