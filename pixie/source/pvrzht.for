      SUBROUTINE PVRZHT( PHI1, PHI2, PHI3, PHI4 )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw all hits in VTX R-Z display in given phi region.
C-
C-   Inputs  : PHI1, PHI2 = phi limits of upper part of display
C-             PHI3, PHI4 = phi limits of lower part of display
C-   Outputs :
C-   Controls:
C-
C-   Created  29-MAR-1990   Peter M. Grudberg
C-   Updated  15-JAN-1992   Nobuaki Oshima
C-                                 Handling for global PHI selection.
C-   Updated  18-MAR-1993   Alexandre Zinchenko - handle compressed hits
C-   Updated  10-MAY-1993   A. Zinchenko - modify handling of PHI-limits
C-   Updated  11-JUN-1993   A. Zinchenko - handle compressed and normal
C-                                         hits simultaneously
C-   Updated  27-June-1994  Danilo Puseljic  handle hits in VCHT banks
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      REAL PHI1, PHI2, PHI3, PHI4, RWIR(0:7,0:2)
      REAL COSPHI, SINPHI, XWIR, YWIR, RWIRE, ZPOS, ZERR
      REAL DDIS, XHPOS, YHPOS, RHPOS, HITPHI
      REAL CHKPHI1, CHKPHI4
      INTEGER NHITS, IPWIR, IHIT, IPHIT, ISTAT
      INTEGER ISIDE, DRWNOZ
      INTEGER LVRFT, GZVRFT, LAY, WIR, LVTXH, GZVTXH
      INTEGER TPSEC1, TPSEC2, BTSEC1, BTSEC2, NUM, SEC
      INTEGER NSEC(0:2), SECT, LVSEC, GZVSEC, LVALS, GZVALS
      INTEGER LVHIT, GZVHIT, IFVHIT
      INTEGER LVCHT,GZVCHT
      DATA NSEC / 16, 32, 32 /
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
C ****  get geometry values from STP
C
        LVRFT = GZVRFT()
        IF ( LVRFT .LE. 0 ) GO TO 999   ! geometry not defined
        DO LAY = 0, 2
          DO WIR = 0, 7
            RWIR(WIR,LAY) = C( LVRFT + 7 + 7*LAY ) +
     &                      C( LVRFT + 23 + WIR )
          ENDDO
        ENDDO
      ENDIF
C
      CALL PUGETV('DRAW NO Z MATCH', DRWNOZ)
      CALL PUGETV('VTX DRAW HITS', IFVHIT)
C
      LVTXH = GZVTXH()
      IF ( LVTXH .LE. 0 ) GO TO 999     ! no hits
c      IF ( IQ(LVTXH + 1) .EQ. 0 ) GO TO 999     ! ibid
c      IF ( IQ(LVTXH + 1).NE.0.AND.IFVHIT.LT.3 ) GO TO 999 ! AlexZ
C
C ****  Calculate sector range to plot.  For values of phi very close to two
C ****  pi, extend the top part range down to zero and the bottom part range up
C ****  to 31 in order to not miss any hits (sector 0 of each layer straddles
C ****  phi=0.)
C
      DO LAY = 0, 2
        NUM = NSEC(LAY)
        IF (LAY.LT.2) THEN
          IF (PHI1 .LT. 0.) THEN
            CHKPHI1 = PHI1 + TWOPI
            TPSEC1  = MIN(INT( CHKPHI1*NUM / TWOPI ) - 1, NUM-1)
            BTSEC2  = INT( PHI4 * NUM / TWOPI ) + 1
          ELSEIF ( PHI4 .GT. TWOPI ) THEN
            CHKPHI4 = PHI4 - TWOPI
            TPSEC1  = INT( PHI1 * NUM / TWOPI ) - 1
            BTSEC2  = MAX(INT( CHKPHI4*NUM / TWOPI ) + 1, 0)
          ELSE
            TPSEC1 = MAX(INT( PHI1 * NUM / TWOPI ) - 1,  0)
            BTSEC2 = MIN(INT( PHI4 * NUM / TWOPI ) + 1, NUM-1)
          ENDIF
          TPSEC2 = MIN(INT( PHI2 * NUM / TWOPI ) + 1, INT(NUM/2.))
          BTSEC1 = MAX(INT( PHI3 * NUM / TWOPI ) - 1, INT(NUM/2.))
        ENDIF
        DO 10 SEC = 0, NSEC(LAY) - 1
          SECT = SEC
C-
C--- Check for PHI1<0 or PHI4>2pi here... ( Nobu. 22-JAN-1992 )
C-
          IF (SECT .LT. BTSEC1 .AND. SECT .GT. TPSEC2)  GO TO 10
          IF ( PHI1.LT.0. .OR. PHI4.GT.TWOPI ) THEN
            IF (SECT .LT. TPSEC1 .AND. SECT .GT. BTSEC2)  GO TO 10
C-
C---        0 < PHI1 < PHI2 < PHI3 < PHI4 < TWOPI
          ELSE
            IF (SECT .LT. TPSEC1 .OR. SECT .GT. BTSEC2)  GO TO 10
          ENDIF
C
C -- Handle hits either from VSEC,VHIT or VCHT banks
C
          LVSEC = GZVSEC(LAY, SEC)
          IF ( LVSEC .LE. 0 ) THEN
            LVHIT = GZVHIT() 

            IF (LVHIT.GT.0) THEN
              CALL PVZHIT_CMPRS(LAY,SEC,IFVHIT,DRWNOZ,
     &                        PHI1,PHI2,PHI3,PHI4)
              GOTO 10
            ELSE
              LVCHT = GZVCHT() 
              IF (LVCHT.NE.0) THEN
                CALL VCHT_UNPACK(LAY,SEC,ISTAT)
                LVSEC = GZVSEC(LAY, SEC)
              ELSE
                GOTO 10
              ENDIF
            ENDIF            

          ENDIF            
C
          IF (IFVHIT.LT.3) GO TO 10 
          IF ( IQ(LVSEC + 1) .EQ. 0 ) GO TO 10  ! no hits
          LVALS = GZVALS(LAY, SEC)
          COSPHI = C(LVALS + 3)
          SINPHI = C(LVALS + 4)
          DO 20 WIR = 0, 7
            NHITS = IQ(LVSEC + 4 + WIR)
            IF ( NHITS .EQ. 0 ) GO TO 20
            IPWIR = LVALS + 6 + IC(LVALS + 6) * WIR
            XWIR = C(IPWIR + 1)
            YWIR = C(IPWIR + 2)
            RWIRE = RWIR(WIR, LAY)
            DO 30 IHIT = 1, NHITS
              IPHIT = LVSEC + IQ(LVSEC + 12 + WIR) +
     &          IQ(LVSEC + 3)*(IHIT - 1)
              ZPOS = Q(IPHIT + 3)
              ZERR = Q(IPHIT + 5)
              ISTAT = IBITS(IQ(IPHIT + 9), 0, 2)
              IF ( ISTAT .EQ. 3 ) THEN  ! matched z hit
                CALL PXCOLR('RED')
                DO ISIDE = 0, 1
                  LVRFT = GZVRFT()
                  DDIS = Q(IPHIT + 1 + ISIDE) -
     &                C(LVRFT + 31 + WIR) * (-1.)**SEC
                  XHPOS = XWIR + DDIS * COSPHI
                  YHPOS = YWIR + DDIS * SINPHI
                  RHPOS = SQRT( XHPOS**2 + YHPOS**2 )
                  HITPHI = ATAN2(YHPOS, XHPOS)
                  IF ( HITPHI .LT. 0. ) HITPHI = HITPHI + TWOPI
C-
C--- For global PHI selection
C-
                  IF ( PHI1 .LT. 0. ) THEN
                    IF (HITPHI.GE.CHKPHI1 .OR. HITPHI.LE.PHI2) THEN
                      CALL JMOVE(ZPOS-ZERR/2., RHPOS)
                      CALL JDRAW(ZPOS+ZERR/2., RHPOS)
                      IF (SECT.NE.0.AND.SECT.NE.INT(NUM/2.)) GO TO 30
                    ELSEIF (HITPHI.GE.PHI3 .AND. HITPHI.LE.PHI4) THEN
                      CALL JMOVE(ZPOS-ZERR/2., -RHPOS)
                      CALL JDRAW(ZPOS+ZERR/2., -RHPOS)
                      IF (SECT.NE.0.AND.SECT.NE.INT(NUM/2.)) GO TO 30
                    ENDIF
                  ELSEIF ( PHI4 .GT. TWOPI ) THEN
                    IF (HITPHI.GE.PHI1 .AND. HITPHI.LE.PHI2) THEN
                      CALL JMOVE(ZPOS-ZERR/2., RHPOS)
                      CALL JDRAW(ZPOS+ZERR/2., RHPOS)
                      IF (SECT.NE.0.AND.SECT.NE.INT(NUM/2.)) GO TO 30
                    ELSEIF (HITPHI.GE.PHI3 .OR. HITPHI.LE.CHKPHI4) THEN
                      CALL JMOVE(ZPOS-ZERR/2., -RHPOS)
                      CALL JDRAW(ZPOS+ZERR/2., -RHPOS)
                      IF (SECT.NE.0.AND.SECT.NE.INT(NUM/2.)) GO TO 30
                    ENDIF
                  ELSE
                    IF (HITPHI.GE.PHI1 .AND. HITPHI.LE.PHI2) THEN
                      CALL JMOVE(ZPOS-ZERR/2., RHPOS)
                      CALL JDRAW(ZPOS+ZERR/2., RHPOS)
                      IF (SECT.NE.0.AND.SECT.NE.INT(NUM/2.)) GO TO 30
                    ELSEIF (HITPHI.GE.PHI3 .AND. HITPHI.LE.PHI4) THEN
                      CALL JMOVE(ZPOS-ZERR/2., -RHPOS)
                      CALL JDRAW(ZPOS+ZERR/2., -RHPOS)
                      IF (SECT.NE.0.AND.SECT.NE.INT(NUM/2.)) GO TO 30
                    ENDIF
                  ENDIF
C---
C-
                ENDDO
              ELSE                      ! unmatched hit
                IF ( DRWNOZ .GT. 0 ) THEN
C
C ****  Mark unmatched hit with dot at end of chamber
C
                  HITPHI = ATAN2(YWIR,XWIR)
                  IF ( HITPHI .LT. 0 ) HITPHI = HITPHI + TWOPI
                  CALL PXCOLR('YEL')
                  CALL JCMARK(1)
C-
C--- For global PHI selection
C-
                  IF ( PHI4 .GT. TWOPI ) THEN
                    IF (HITPHI.GE.PHI3 .OR. HITPHI.LE.CHKPHI4) THEN
                      RWIRE = - RWIRE
                    ENDIF
                  ELSE
                    IF (HITPHI.GE.PHI3 .AND. HITPHI.LE.PHI4) THEN
                      RWIRE = - RWIRE
                    ENDIF
                  ENDIF
                  CALL JMARK(ZPOS, RWIRE)
                ENDIF
              ENDIF
   30       CONTINUE
   20     CONTINUE
   10   CONTINUE
      ENDDO
C
C
  999 RETURN
      END
