      SUBROUTINE DTRK_EDGE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to build DTRKs which go out of CDC from 
C-                         the edge of the chamber
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  21-SEP-1991   Qizhong Li-Demarteau
C-   Modified 22-NOV-1993   Stefano Lami   added protection for ATAN2
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:DDEBUG.INC'
      INCLUDE 'D0$INC:CDRESF.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
      INCLUDE 'D0$INC:CDLTRK.INC'
      INTEGER NBTWIR
      PARAMETER( NBTWIR= NBSENS*4 )
      INTEGER LAYF, LAYL, SEC, SEG, SEGR, SEGL, IPL, IPR, IPF
      INTEGER I, LAY, NUMHIT, ISIDE, KPDSEC, LHIT, J
      INTEGER NDTSG(0:2), NWDTSG
      INTEGER LISPT(0:2), NUSEGM(0:3)
      INTEGER LABEL( NBTWIR ), NFADC, IPHIT, WIR
      INTEGER NMISS, CDMINL, ERR
      INTEGER IER, DTRKID
      REAL    CDTDPH, CDTDIS, ZPOS(2)
      REAL    XR, YR, DY, DELPOS, DELPHI
      REAL    PHIF, XF, YF, PHIL, XL, YL, DIST, DISMIN, S, PHID
      LOGICAL FIRST, EZERROR, OK, DTRKOK
C
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','DTRKPT',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('CDMINL_EDGE',CDMINL,ERR)
        CALL EZGET('CDTDPH',CDTDPH,ERR)
        CALL EZGET('CDTDIS',CDTDIS,ERR)
        CALL EZRSET
      ENDIF
C
      CALL VZERO(NDTSG(0),3)
      IF (LDTRH .GT. 0) CALL UCOPY( IQ( LDTRH+3 ), NDTSG(0), 3 )
      NWDTSG = IQ(LDTSG(0)+2)
      IF( DBGFLG .AND. LVLDBG(7) .GE. 5) WRITE( LUNDBG,3000 )
 3000 FORMAT( '0** CDTRAK ** After fit: seg #, x,y,z,theta,phi and',
     &  '  err(d,z,theta,phi)'/)
      DO 110 LAYF = 2, CDMINL-1, -1
        DO 120 SEG = 1, NDTSG(LAYF)
          IPF = LDTSG(LAYF) + 2 + (SEG-1) * NWDTSG
          IF( IQ(IPF+1) .NE. 0 ) GOTO 120      ! Dont reuse
          CALL VZERO(LABEL,NBTWIR)
          CALL VZERO(LISPT(0),3)
          CALL VZERO(NUSEGM(0),4)
          CALL DTSG_Z(LAYF,SEG,ZPOS)
          CALL DCHKZ_EDGE(LAYF,ZPOS,OK)
          IF (.NOT. OK) GOTO 120
          NMISS = LAYF+1 - CDMINL
          XF   = Q(IPF+3)
          YF   = Q(IPF+4)
          PHIF = Q(IPF+5)
          LISPT(LAYF) = IPF
          NUSEGM(LAYF) = SEG
          CALL UCOPY(IQ(IPF+9), LABEL(LAYF*NBSENS+1), NBSENS)
          IF ( DBGFLG .AND. LVLDBG(7).GE.3 ) THEN
            WRITE( LUNDBG, 2000 ) LAYF, SEG, PHIF, XF, YF
 2000       FORMAT(/' Start edge_tracking for layer',I2,' segment',I4,
     &              ' Phi, x, y =',F10.6,2F10.4)
          ENDIF
          DO 130 LAYL = LAYF-1, 0, -1
            DISMIN = 1000.
            DO 140 SEGL = 1, NDTSG(LAYL)
              IPL = LDTSG(LAYL) + 2 + (SEGL-1) * NWDTSG
              IF( IQ(IPL+1) .NE. 0 ) GOTO 140
              PHIL = Q( IPL+5 )
              IF( ABS( PHIL-PHIF ) .GT. CDTDPH ) GOTO 140
              XL   = Q( IPL+3 )
              YL   = Q( IPL+4 )
              S = .5*SQRT( (XL-XF)**2 + (YL-YF)**2 )
              IF(S.NE.0.) PHID = ATAN2( YF-YL, XF-XL )
              DIST = S * ( TAN( PHIF-PHID ) + TAN( PHIL - PHID ) )
              IF( ABS(DIST) .LT. DISMIN ) THEN
                CALL DTSG_Z(LAYL,SEGL,ZPOS)
                CALL DCHKZ_EDGE(LAYL,ZPOS,OK)
                IF (.NOT. OK) GOTO 140
                DISMIN = ABS(DIST)
                IPR    = IPL
                SEGR   = SEGL
                DELPHI = PHIL-PHIF
                DELPOS = DIST
              ENDIF
  140       CONTINUE
            IF (DISMIN .GT. CDTDIS) THEN
              NMISS = NMISS - 1
              IF (NMISS .LT. 0) GOTO 120
              GOTO 130
            ENDIF
            IF ( DBGFLG .AND. LVLDBG(7) .GE. 3 ) THEN
              WRITE( LUNDBG, 2100 ) LAYL, SEGR, DELPOS, DELPHI
 2100         FORMAT(20X,'On layer ',I2,' segment',I3,' dist=',F10.4,
     &               ' delphi=',F10.6)
            ENDIF
            NUSEGM(LAYL) = SEGR
            LISPT(LAYL) = IPR
            PHIF = Q(IPR+5)
            XF   = Q(IPR+3)
            YF   = Q(IPR+4)
            CALL UCOPY( IQ(IPR+9), LABEL(LAYL*NBSENS+1), NBSENS )
  130     CONTINUE
C
C     do not build tracks only with same drift direction hits
C
          IF (NUSEGM(1) .EQ. 0) GOTO 999 
C
C ****  OK, we have now a track, prepare parameters for fit, fit, test if
C ****  good and store if good. Return nusegm = 0 if bad...
C
          CALL CDTFIT(NUSEGM,LABEL,1)
C
C ****  Tag segments, to avoid reusing them later...
C
          DTRKOK = .FALSE.
          DO 131 LAYL = 0, 2
            IF (LISPT(LAYL) .EQ. 0) GOTO 131
            IF (NUSEGM(LAYL) .EQ. 0) GOTO 131
            DTRKOK = .TRUE.
            DTRKID = IQ(LDTRK - 5)
            IF (DTRKID .GT. 0) THEN
              IQ (LISPT(LAYL)+1) = IQ(LDTRK-5)
            ELSE
              IQ (LISPT(LAYL)+1) = 1
            ENDIF
  131     CONTINUE
C
C   mark that this track goes through the edge of the CDC
C
          IF (DTRKOK .AND. LDTRK .GT. 0) THEN
            IQ(LDTRK+1) = IBSET(IQ(LDTRK+1),0)
          ENDIF
  120   CONTINUE
  110 CONTINUE
C
C ****  Now the debugging
C
      IF (DBGFLG .AND. LVLDBG(7) .NE. 0) THEN
        CALL PRDTRK(LUNDBG, 0, 0, 'ALL', LVLDBG(7))
      ENDIF
  999 RETURN
      END
