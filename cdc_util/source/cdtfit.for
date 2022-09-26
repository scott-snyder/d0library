      SUBROUTINE CDTFIT(NUSEGM,LABEL,MODE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fit the CDC space track with points given in
C-              LABEL. Store the result in the bank DTRK
C-
C-   Inputs  : NUSEGM(0:3) [I] : Used segment numbers
C-             LABEL(*)    [I]= packed hit ident
C-             MODE        [I]: MODE=0 for regular tracks;
C-                              MODE=1 for edge tracks
C-   Outputs : bank DTRK
C-
C-   Created  10-FEB-1988   Olivier Callot
C-   Updated  30-MAY-1989   Qizhong Li-Demarteau  use SRCP and add MINZDF
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated  15-AUG-1991   Qizhong Li-Demarteau  update DHIT bank
C-   Updated  21-OCT-1991   Qizhong Li-Demarteau  fill segment # into DTRK
C-   Updated   4-NOV-1991   Qizhong Li-Demarteau  added an input MODE and
C-                                      made the routine to handle both 
C-                                      regular tracks and the edge tracks 
C-   Updated  24-AUG-1992   Qizhong Li-Demarteau  added covariance term in
C-                                      track parameters
C-                                      
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:DDEBUG.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
      INCLUDE 'D0$INC:CDLTRK.INC'
      INTEGER NBTWIR
      PARAMETER( NBTWIR= 4*NBSENS)
      INTEGER LABEL( NBTWIR), NUSEGM(0:3), MODE
      INTEGER I, J, LAY, SEC, WIR, NUMHIT, ISIDE
      INTEGER KPDSEC, LHIT, IPHIT, NFADC, LDALS, IPAL
      INTEGER NDEGF(2), ERR, CDSURV
      INTEGER MINZDF, MINNMZ, MINZ_EDGE
      INTEGER IER
      REAL    XI(NBTWIR), YI(NBTWIR), ZI(NBTWIR), WR(NBTWIR), WZ(NBTWIR)
      REAL    XR, YR, DY, CHISQ(2), PARFIT(10)
      REAL    RESID(NBTWIR,2), CDTCH2
      LOGICAL FIRST, EZERROR, BUILD_DHIT, EDGETR
      SAVE FIRST
C
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
C
      NDEGF(1) = -2
      NDEGF(2) = -2
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','CDTFIT',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET_i('CDSURV',CDSURV,ERR)
        CALL EZGET('CDTCH2',CDTCH2,ERR)
        CALL EZGET_i('MINNMZ',MINNMZ,ERR)
        CALL EZGET_i('MINZ_EDGE',MINZ_EDGE,ERR)
        CALL EZGET_l('BUILD_DHIT',BUILD_DHIT,ERR)
        CALL EZRSET
      ENDIF
      DO 200 I = 1, NBTWIR
        IF( LABEL(I) .EQ. 0 ) THEN
          XI(I) = 0.
          YI(I) = 0.
          WR(I) = 0.
          ZI(I) = 0.
          WZ(I) = 0.
        ELSE
          LAY = IBITS( LABEL(I), 16, 2 )
          SEC = IBITS( LABEL(I), 11, 5 )
          WIR = IBITS( LABEL(I),  8, 3 )
          LDALS = LC( LC( LC( LSCDC-5 ) -(LAY+1) ) -(SEC+1) )
          IPAL  = LDALS + 6 + IC( LDALS+6 ) * WIR
          NUMHIT = IBITS( LABEL(I),  1, 7 )
          ISIDE  = IBITS( LABEL(I),  0, 1 )
          KPDSEC = LDSEC( SEC, LAY )
          LHIT   = IQ( KPDSEC + 3 )
          NFADC  = IQ( KPDSEC + 2 )
          IPHIT  = IQ( KPDSEC + NFADC + 4 + WIR ) +
     &                     ( NUMHIT-1 ) * LHIT + KPDSEC
          YR = Q( IPHIT + ISIDE + 2 ) - C( LC(LDGEH-3) +26+WIR )
          DY = Q( IPHIT + 5 )
          XI(I) = C( IPAL+1 ) + YR * C( LDALS+3 )
          YI(I) = C( IPAL+2 ) + YR * C( LDALS+4 )
          WR(I) = 1./ DY**2
          NDEGF(1) = NDEGF(1) + 1
          WZ(I) = 0.
          ZI(I) = 0.
          IF( Q( IPHIT+6 ) .LT. 9999. ) THEN
            ZI(I) = Q( IPHIT+4 )
            WZ(I) = 1./Q(IPHIT+6) **2
            NDEGF(2) = NDEGF(2) + 1
          ENDIF
        ENDIF
  200 CONTINUE
C
C ****  If not enough Z measures, suppress them to avoid dummy fits
C
      IF (MODE .NE. 0) THEN
        IF (NDEGF(2) .LE. -2) GOTO 999    !don't build non-Z edge track 
        MINZDF = MINZ_EDGE - 2
      ELSE
        MINZDF = MINNMZ - 2
      ENDIF
      IF (NDEGF(2) .LT. MINZDF) THEN
        CALL VZERO(WZ(1), NBTWIR)
        NDEGF(2) = -2
      ENDIF
C
      CALL CDXYZL(XI,YI,ZI,WR,WZ,NBTWIR,NDEGF,MINZDF,PARFIT,CHISQ,RESID)
C
      IF( DBGFLG .AND. LVLDBG(7) .GE. 3) THEN
        WRITE( LUNDBG, 3100 ) NUSEGM, PARFIT
 3100   FORMAT( 2X,4I4,2X,3F10.3,2F10.6,2F10.3,3F12.6)
        WRITE( LUNDBG, 2200 ) (CHISQ(I), NDEGF(I),I=1,2)
 2200   FORMAT(10X,'XY : Chisq ',F10.3,' Ndeg =',I3,
     &         10X,'RZ : Chisq ',F10.3,' Ndeg =',I3)
        IF( LVLDBG(7) .GE. 10 ) WRITE( LUNDBG, 2300 )
     &            (I,XI(I),YI(I),ZI(I),(RESID(I,J), J=1,2), I=1,NBTWIR)
 2300   FORMAT( 20X,I3,3F10.4,2X,2F10.4 )
      ENDIF
C
C ****  Now, test the track quality ( Chisq )
C
      IF( CHISQ(1) .GT. CDTCH2*NDEGF(1) ) THEN
        CALL VZERO_i(NUSEGM(0), 4)
        IF( DBGFLG .AND. LVLDBG(7) .GE. 3) WRITE(LUNDBG,3200)
 3200   FORMAT(10X,'Chisquare on XY is too big. Track rejected')
        GOTO 999
      ENDIF
      IF (MODE .NE. 0) THEN
        CALL DCHKT_EDGE(PARFIT,EDGETR)
        IF (.NOT. EDGETR) THEN
          CALL VZERO_i(NUSEGM(0), 4)
          GOTO 999
        ENDIF
      ENDIF
C
C ****  Track is OK, store it and residuals
C
      CALL ZFDTRK(PARFIT, CHISQ, NDEGF, LABEL, RESID)
      CALL DTRKSG(NUSEGM,LDTRK)
C
C   set bits in the compressed hits bank for the hits on DTRK
C
      IF (BUILD_DHIT) CALL DHITST(LABEL)
C
      IF ( CDSURV .NE. 0 ) THEN
        CALL CDSRES( NBTWIR, LABEL, RESID, WZ, NDEGF)
      ENDIF
C
  999 RETURN
      END
