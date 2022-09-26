      SUBROUTINE CRHITL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Generate a table of hits sorted in phi
C-              for one layer of the CDC.
C-
C-   Inputs  :
C-   Outputs : banks LHITL
C-
C-   Created  17-SEP-1987   Olivier Callot
C-   Updated  20-APR-1989   Qizhong Li-Demarteau   use SRCP 
C-   Updated  21-NOV-1990   Qizhong Li-Demarteau replaced SORTMQ by SORTRQ 
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:DDEBUG.INC'
      INCLUDE 'D0$INC:CDLOCA.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
      INCLUDE 'D0$INC:CDLTRK.INC'
      INCLUDE 'D0$LINKS:IZUSER.LINK'
      INCLUDE 'D0$LINKS:IZDALH.LINK'
C
      INTEGER MPHITL(5), MPUSER(5)
      INTEGER NBHITS(0:31), IPHITS(0:31), NMORE, IPSTO, JOFF
      INTEGER LHIT, IHIT, ISIDE, NH, IP, IH, LABEL, I, J
      INTEGER LDALL, LDALS, IPAL, NBEXTR
      PARAMETER( NBEXTR = 2 )
      INTEGER STAT, ION, IDONE, MAXSEC, ERR
      INTEGER IER
      REAL    STAG, PI, CDMXPH
      REAL    X0SEC, Y0SEC, XR, YR, DY, XABS, YABS, CPHI, SPHI
      CHARACTER*8 ZFORMA
      LOGICAL FIRST
      LOGICAL EZERROR
C
      SAVE FIRST
      DATA      MPHITL / 0, 0, 0, 0, 0 /
      DATA      MPUSER / 0, 7, 7, 0, 0 /
      DATA      FIRST / .TRUE. /
      DATA ION,IDONE/1,2/
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL UCTOH( 'HITL', MPHITL(1), 4, 4 )
        PI = ACOS( -1. )
        CALL UCTOH('USER', MPUSER(1), 4, 4 )
        MPUSER(2) = NBSENS
        MPUSER(3) = NBSENS
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','CRHITL',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET_i('MAXSEC',MAXSEC,ERR)
        CALL EZGET('CDMXPH',CDMXPH,ERR)
        CALL EZRSET
      ENDIF
C
C ****  Need a set of NBSENS temporary banks for hits. Book it from USER
C
      IF ( LQ(LHEAD-IZUSER) .NE. 0 ) THEN
        CALL MZDROP( IXCOM,  LQ(LHEAD-IZUSER), 'L')
      ENDIF
      CALL MZLIFT(IXMAIN, LUSER, LHEAD, -IZUSER, MPUSER, 0 )

      DO 100 WIRE = 0, MXSENS
        NH = 0
        NMORE = 0
        JOFF = IQ( LCDCH+3 ) + WIRE + 4
        LHIT = IQ( LCDCH+2 )
        CALL VZERO( NBHITS, 32 )
        DO 105 SECTOR = 0, MAXSEC
          IF( LDSEC( SECTOR, LAYER ) .EQ. 0 ) GOTO 105
          STAT=IQ(LDSEC(SECTOR,LAYER))
          IF (BTEST(STAT,IDONE)) THEN
            GO TO 105
          ELSE
            IF (.NOT.BTEST(STAT,ION)) THEN
              GO TO 105
            ELSE
              IF (WIRE.EQ.MXSENS) THEN
                IQ(LDSEC(SECTOR,LAYER))=IBSET(STAT,IDONE)
              END IF
            END IF
          END IF
          NBHITS( SECTOR ) = IQ( LDSEC( SECTOR, LAYER ) + 4 + WIRE )
          IPHITS( SECTOR ) = IQ( LDSEC( SECTOR, LAYER ) + JOFF )
          NH = NH + IQ( LDSEC( SECTOR, LAYER ) + 4 + WIRE )
  105   CONTINUE
C
C ****  We have 7 words per hit. Book for twice the number of hit because
C ****  we will store the two possible solution ( drift direction ambiguity )
C ****  We add NBEXTR possible hits for the -pi, pi copy.
C
        NH = 2 * NH             ! two sides
        MPHITL(4) = 7*(NH+NBEXTR) + 1
        CALL MZLIFT( IXMAIN, LHITL(WIRE), LUSER, -(WIRE+1), MPHITL, -1)
        IPSTO = LHITL( WIRE ) + 1
        IQ( LHITL(WIRE) + 1 ) = NH
        IF( NH .EQ. 0 ) GOTO 100
C
C ****  The staggering is added in DSEC. Here, we have to substract it because
C ****  we have the true wire position in DALS. The value in DSEC is the
C ****  position in the cell frame, which is the drift distance + staggering
C ****  ( see ZFDSEC )
C
        STAG = C( LC(LDGEH-3) + 26 + WIRE )     ! Staggering
        DO 110 SECTOR = 0, MAXSEC
          IF( NBHITS( SECTOR ) .EQ. 0 ) GOTO 110
          LDALH = LC( LSCDC - IZDALH )          !DALH
          LDALL = LC( LDALH-(LAYER+1) )         !DALL
          LDALS = LC( LDALL-(SECTOR+1) )        !DALS
          CPHI = C( LDALS+3 )
          SPHI = C( LDALS+4 )
          IPAL = LDALS + 6 + IC(LDALS+6) * WIRE
          X0SEC = C( IPAL+1 )
          Y0SEC = C( IPAL+2 )
          LABEL = 256 * (( LAYER * 32 + SECTOR) * 8 + WIRE)
          IP = LDSEC( SECTOR, LAYER ) + IPHITS( SECTOR )
          DO 130 IHIT = 1, NBHITS( SECTOR)
            DO 140 ISIDE = 1, 2
              YR = Q( IP + ISIDE+1 ) - STAG
              DY = Q( IP + 5 )
              XABS = X0SEC + YR * CPHI
              YABS = Y0SEC + YR * SPHI
              Q( IPSTO+1 ) = SQRT( XABS**2 + YABS**2 )          ! R
              Q( IPSTO+2 ) = ATAN2( YABS, XABS )                ! Phi
              Q( IPSTO+3 ) = 1. / DY**2                         ! Error
              IQ(IPSTO+4 ) = LABEL + IHIT * 2 + ISIDE-1         ! label
              Q( IPSTO+5 ) = XABS                               ! X real
              Q( IPSTO+6 ) = YABS                               ! Y real
              IQ(IPSTO+7 ) = 0                                  ! For tagging
              IF( Q(IPSTO+2) .LT. CDMXPH-PI ) NMORE = NMORE + 1 ! nb to copy
              IPSTO = IPSTO + 7
  140       CONTINUE
            IP = IP + LHIT
  130     CONTINUE
  120     CONTINUE
  110   CONTINUE
C
C ****  Now, we have all the hits in the layer. We will sort them, and
C ****  copy the firsts after the last, to ease the -pi,+pi discontinuity
C ****  work.
C
        CALL SORTRQ( Q(LHITL(WIRE)+2), 7, NH, 2 )
        IF ( NMORE .GT. NBEXTR ) THEN
          CALL MZPUSH( IXCOM,  LHITL( WIRE ), 0, 7*(NMORE-NBEXTR), 'I' )
        ENDIF
        IPSTO = LHITL(WIRE) + 7*NH + 1
        IP    = LHITL(WIRE) + 1
        IQ( IP ) = IQ( IP ) + NMORE
        DO 160 IH = 1, NMORE
          CALL UCOPY( Q(IP+1), Q(IPSTO+1), 7 )
          Q( IPSTO+2 ) = Q( IPSTO+2 ) + 2*PI
          IP = IP + 7
          IPSTO = IPSTO + 7
  160   CONTINUE
  100 CONTINUE
      IF ( DBGFLG .AND. LVLDBG(6) .GE. 2 ) THEN
        WRITE( LUNDBG, 1000 ) LAYER
 1000   FORMAT('0** CDHITL **    Hit info for layer ',I2/)
        DO 300 WIRE = 0, MXSENS
          WRITE( LUNDBG, 1100 ) WIRE
 1100     FORMAT(' For wire ',I2)
          DO 310 IH = 1, IQ(LHITL(WIRE)+1)
            IP = LHITL(WIRE) + 7*(IH-1) + 1
            WRITE( LUNDBG, 1200 ) (Q(IP+I),I=1,3), ZFORMA( Q(IP+4) ),
     &                            (Q(IP+I),I=5,6)
 1200       FORMAT(10X,2F10.5,F10.3,A9,2F10.5)
  310     CONTINUE
  300   CONTINUE
      ENDIF
  999 RETURN
      END
