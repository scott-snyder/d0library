      SUBROUTINE PDSIDE( SECMIN, SECMAX )
C===================================================================
C
C  Decription:  Displays an event for the sectors secmin, secmax of the
C  ===========  Central Drift Chambers
C
C
C  Parameter Descriptions:
C  =======================
C  None
C-   Created  20-MAY-1986   Tami Kramer
C-   Updated  17-DEC-1987   Olivier Callot   Clean up
C-   Updated  01-AUG-1989   Qizhong Li-Demarteau  change parameters to integer
C-   Updated  09-NOV-1989   Qizhong Li-Demarteau  use PATHST
C-   Updated   3-APR-1991   Harrison B. Prosper   Fixed some EZ problems
C-   Updated  15-APR-1991   Qizhong Li-Demarteau  use alignment information
C-                               for sector drawing and added SEC # display
C-   Updated   6-MAR-1991   Qizhong Li-Demarteau  draw hits from compressed
C-                                  hits bank if the DSEC banks do not exit
C
C====================================================================
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:GRAPHF77.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:CDPARA.INC/LIST'
      INCLUDE 'D0$INC:PXPARA.INC/LIST'
C
C  Local Declarations:
C  ===================
C
      LOGICAL EZERROR
      INTEGER SECMIN, SECMAX, IS
      INTEGER I,L
      INTEGER NHITS(0:NBSENS-1),NWIR,LHIT
      INTEGER KPWIRE(0:NBSENS-1),KP
      INTEGER LAY, SEC, IWIR, ISEG
      INTEGER IFDSEG, IFISTR, IFDSEC, IFDWIR, IFDHIT
      INTEGER LDHIT, GZDHIT
      INTEGER IPATH, ERR, PLDALS, GZDALS
      CHARACTER*4 DPATH
      EQUIVALENCE (IPATH, DPATH)
      CHARACTER*2 SECNUM
      LOGICAL FIRST, ALGNMT
      REAL    PHIW, CPHIW, SPHIW, PCPHIW
      REAL    DDIS1,DDIS
      REAL    DRANG1,DRANG2
      REAL    XHPOS,YHPOS
      REAL    SIZDIS, DEGRAD
      REAL    RAYCEN, DELRAY, PHICEN, DELPHI
      REAL    X1, X2, Y1, Y2, R1, R2, PHI1, PHI2, ZZ
      REAL    X3, Y3, R3, NUMPHI, RCHATH
      INTEGER LDRFT, LDALS, IPWIR
      REAL    XWIR, YWIR, TLSEG, XCENT, YCENT, XORI, YORI
      INTEGER KPDTSG, NBDTSG, LDTSG, NBWIR
      PARAMETER( DEGRAD = 3.1415926535/180.)
C
C  Data Statements:
C  ================
C
      DATA TLSEG / 3.0 /
      DATA SIZDIS / 0.2 /
      DATA FIRST/.TRUE./, ALGNMT/.FALSE./
C
C  Executable Code:
C  =================
C
      CALL PUGETV('CDC DRAW ISATRK ',   IFISTR )
      CALL PUGETV('CDC DRAW SECTORS',   IFDSEC )
      CALL PUGETV('CDC DRAW WIRES',     IFDWIR )
      CALL PUGETV('CDC DRAW HITS',      IFDHIT )
      CALL PUGETV('CDC DRAW TSEG',      IFDSEG )
      IF(IFISTR .NE. 0) CALL PDISTR
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( .NOT. EZERROR(ERR) ) THEN
          CALL EZGET('DPATH',IPATH,ERR)
          CALL EZRSET
        ENDIF
        PLDALS = GZDALS(0,0)
        PCPHIW = C(PLDALS+3)
        IF (ABS(PCPHIW) .LE. 0.001) ALGNMT = .TRUE.
      ENDIF
C
      CALL PUOPEN
      DO 88 LAY = 0,3
C
C ****  Access DRFT bank for nominal geometry
C
        LDRFT = LC( LDGEH - 3 )
        RAYCEN = C( LDRFT+11+2*LAY )
        PHICEN = C( LDRFT+12+2*LAY ) * DEGRAD
        DELRAY = C( LDRFT+8 )
        DELPHI = C( LDRFT+9 ) * DEGRAD
        R1 = ( RAYCEN - DELRAY ) / COS( DELPHI )
        R2 = ( RAYCEN + DELRAY ) / COS( DELPHI )
        DO 89 IS = SECMIN, SECMAX
          SEC = IS
          IF( SEC .LT. 0  ) SEC = SEC + 32
          IF( SEC .GT. 31 ) SEC = SEC - 32
          CALL PXCOLR('GRE')
          IF (IFDSEC .NE. 0) THEN
C
C ****  Draw the cell limits.
C
            PHI1 = PHICEN + (2*SEC-1) * DELPHI
            PHI2 = PHICEN + (2*SEC+1) * DELPHI
            X1 =  R1 * COS( PHI1 )
            Y1 =  R1 * SIN( PHI1 )
            IF (ALGNMT) CALL PDALGN(X1,Y1,ZZ)
            CALL JMOVE( X1, Y1 )
            X2 =  R1 * COS( PHI2 )
            Y2 =  R1 * SIN( PHI2 )
            IF (ALGNMT) CALL PDALGN(X2,Y2,ZZ)
            CALL JDRAW( X2, Y2 )
            X2 =  R2 * COS( PHI2 )
            Y2 =  R2 * SIN( PHI2 )
            IF (ALGNMT) CALL PDALGN(X2,Y2,ZZ)
            IF (IFDSEC .LE. 1) THEN
              CALL JMOVE(X2,Y2)
            ELSE
              CALL JDRAW(X2,Y2)
            ENDIF
            X2 =  R2 * COS( PHI1 )
            Y2 =  R2 * SIN( PHI1 )
            IF (ALGNMT) CALL PDALGN(X2,Y2,ZZ)
            CALL JDRAW( X2, Y2 )
            IF (IFDSEC .GT. 1) CALL JDRAW(X1,Y1)
            IF (IFDSEC .EQ. 3) THEN
              IF (LAY .EQ. 3) THEN
                IF (SEC .LE. 15) R3 = R2 + 0.05*(R2-R1)*SEC
                IF (SEC .GT. 15) R3 = R2 + 0.05*(R2-R1)*(31-SEC) + 1.0
                PHI1 = ATAN2(Y2,X2)
                NUMPHI = PHI1 + DELPHI / 2
                X3 =  R3 * COS(NUMPHI)
                Y3 =  R3 * SIN(NUMPHI)
                IF (SEC .LT. 10) THEN
                  WRITE (SECNUM, 1001) SEC
                ELSE
                  WRITE (SECNUM, 1002) SEC
                ENDIF
 1001           FORMAT (I1)
 1002           FORMAT (I2)
                CALL JMOVE(X3, Y3)
                CALL J1STRG(SECNUM)
              ENDIF
            ENDIF
          ENDIF
          CALL PXCOLR( 'MAG' )
          CALL PATHST(DPATH)
          CALL ZGDSEC( LAY, SEC, NBWIR, KPWIRE(0), NHITS(0), LHIT)
          IF (LHIT .EQ. 0 .AND. IFDHIT .NE. 0) THEN
            LDHIT = GZDHIT()
            IF (LDHIT .NE. 0) THEN
              CALL PDXYHT_CMPRS(LAY,SEC)
            ENDIF
          ENDIF
          LDALS = GZDALS(LAY, SEC)
          CPHIW = C( LDALS+3 )
          SPHIW = C( LDALS+4 )
          DO 99 IWIR= 0,NBWIR-1
            IPWIR = LDALS + 6 + IC(LDALS+6) * IWIR
            XWIR = C( IPWIR+1 )
            YWIR = C( IPWIR+2 )
C
C  Mark wire positions with an "+"
C
            IF (IFDWIR .NE. 0) THEN
              CALL JCMARK(1)
              CALL JMARK( XWIR, YWIR )
            ENDIF
            IF ( LHIT .EQ. 0) GO TO 99
            IF (IFDHIT .NE. 0) THEN
              DO 66 L = 1,NHITS(IWIR)
C
C  Get drift distances from ZEBRA banks and mark raw hit positions
C  with a short segment
C
                KP = KPWIRE( IWIR ) + (L-1)*LHIT + 2
                DDIS = Q(KP) - C(LDRFT+26+IWIR) - .5*SIZDIS
                XHPOS = XWIR + DDIS * CPHIW
                YHPOS = YWIR + DDIS * SPHIW
                CALL JMOVE( XHPOS, YHPOS )
                XHPOS = XHPOS + SIZDIS * CPHIW
                YHPOS = YHPOS + SIZDIS * SPHIW
                CALL JDRAW( XHPOS, YHPOS )
                DDIS = Q(KP+1) - C(LDRFT+26+IWIR) - .5*SIZDIS
                XHPOS = XWIR + DDIS * CPHIW
                YHPOS = YWIR + DDIS * SPHIW
                CALL JMOVE( XHPOS, YHPOS )
                XHPOS = XHPOS + SIZDIS * CPHIW
                YHPOS = YHPOS + SIZDIS * SPHIW
                CALL JDRAW( XHPOS, YHPOS )
   66         CONTINUE
            ENDIF
   99     CONTINUE
CC          CALL PUCLOSE
   89   CONTINUE
C
C ****  Now, draw track segments
C
        IF (IFDSEG .NE. 0) THEN
          CALL PATHRS
          CALL ZGDTSG( LAY, NBDTSG, LDTSG, KPDTSG )
          IF (KPDTSG .EQ. 0) GOTO 88
CC          CALL PUOPEN
          DO 90 L = 1, NBDTSG
            KP = LDTSG * (L-1) + KPDTSG
            IF( IFDSEG .GT. 1. .AND.
     &                      IQ( KP+1 ) .EQ. IFDSEG - 2 ) GOTO 90
            XHPOS = Q( KP+3 ) + TLSEG * COS( Q(KP+5) )
            YHPOS = Q( KP+4 ) + TLSEG * SIN( Q(KP+5) )
            CALL JMOVE( XHPOS, YHPOS )
            XHPOS = Q( KP+3 ) - TLSEG * COS( Q(KP+5) )
            YHPOS = Q( KP+4 ) - TLSEG * SIN( Q(KP+5) )
            CALL JDRAW( XHPOS, YHPOS )
   90     CONTINUE
CC          CALL PUCLOSE
        ENDIF
   88 CONTINUE
      CALL PUCLOSE
      RETURN
      END

