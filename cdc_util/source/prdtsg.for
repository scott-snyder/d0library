      SUBROUTINE PRDTSG( PRUNIT, KDTSG, NDTSG, CARFL, IPRFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print bank DTSG
C-
C-   Inputs  : PRUNIT [I] : Logical unit for output
C-             KDTSG  [I] : (not used)
C-             NDTSG  [I] : Layer number (when CARFL = 'SINGLE')
C-             CARFL  [A] : 'ALL' or 'SINGLE'
C-             IPRFL  [I] : print level (Residuals if IPRFL > 1)
C-
C-   Created  22-SEP-1987   Olivier Callot
C-   Updated  13-JUL-1989   Qizhong Li-Demarteau   rewrite to match D0
C-                                                 standards
C-   Updated  21-OCT-1991   Qizhong Li-Demarteau   added track # in printout 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDLTRK.INC'
      INTEGER PRUNIT, KDTSG, NDTSG, IPRFL, LAY, NVERS
      INTEGER NBDTSG, LNDTSG, LAYER, KPDTSG
      INTEGER I, IP, K
      CHARACTER*(*) CARFL
      CHARACTER*8   ZFORMA
      LOGICAL  EXIST
C
      DATA  EXIST/.FALSE./
C----------------------------------------------------------------------
C
      IF ((CARFL .EQ. 'ALL') .AND. (IPRFL .GE. 1)) THEN
        DO 100 LAY = 0, 3
          IF (LDTSG(LAY) .GT. 0) THEN
            EXIST = .TRUE.
            CALL ZGDTSG( LAY, NBDTSG, LNDTSG, KPDTSG )
            NVERS = IBITS(IQ(LDTSG(LAY)),13,5) 
            WRITE( PRUNIT, 2000 ) NVERS, LAY, NBDTSG
            DO 101 I = 1, NBDTSG
              IP = KPDTSG + LNDTSG*(I-1)
              WRITE( PRUNIT, 2100 ) I,(IQ(IP+K),K=1,2),(Q(IP+K), K=3,8)
 101        CONTINUE
          ENDIF
 100    CONTINUE
        IF (.NOT. EXIST) THEN
          WRITE(PRUNIT, 2001)
 2001     FORMAT(1X,
     &        '   PRDTSG: no segment has been found in this event')
        ENDIF
      ELSE
        IF( CARFL .EQ. 'SINGLE' .AND. (IPRFL .GE. 1)) THEN
          LAYER = NDTSG
          CALL ZGDTSG( LAYER, NBDTSG, LNDTSG, KPDTSG )
          NVERS = IBITS(IQ(LDTSG(LAYER)),13,5) 
          WRITE( PRUNIT, 2000 ) NVERS, LAYER, NBDTSG
 2000     FORMAT(/,' Bank DTSG: (version',I2,')  In layer ',I2,
     &    '   we found ',I4, ' segments'/
     &      '   num trk dgf        x0        y0       phi      errd',
     &      '  errphi   chisq')
          DO 310 I = 1, NBDTSG
            IP = KPDTSG + LNDTSG*(I-1)
            IF ( IPRFL .LE. 1 ) THEN
              WRITE( PRUNIT, 2100 ) I,(IQ(IP+K),K=1,2),(Q(IP+K), K=3,8)
            ELSE
              WRITE( PRUNIT, 2100 ) I,(IQ(IP+K),K=1,2),(Q(IP+K), K=3,8),
     &                                    (ZFORMA(IQ(IP+K)), K=9,15)
              WRITE( PRUNIT, 2200 ) ( Q(IP+K), K=16,LNDTSG)
            ENDIF
 2100       FORMAT(I6,2I4,3F10.4,2X,2F8.4,2X,F6.2,7A8)
 2200       FORMAT(70X,7F8.4)
  310     CONTINUE
        ENDIF
      ENDIF
  999 RETURN
      END
