      SUBROUTINE PFPICK_ROAD(HALF, ZVERT, DX, DY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Requests which EM cluster the user wants.
C-                         For FDC road display.
C-
C-   Inputs  : none
C-   Outputs : HALF     Half for choosen cluster
C-             ZVERT    z-vertex choosen
C-             DX, DY   Slopes for choosen cluster
C-
C-   Created   1-OCT-1992   Robert E. Avery
C-   Updated   2-MAR-1993   Robert E. Avery  Allow choice of vertex.
C-   Updated  11-MAR-1993   Robert E. Avery  Now based on PPHO and PELC banks,
C-                                              rather than on roads.
C-   Updated   9-FEB-1994   Robert E. Avery  Include PMUO too. 
C-   Updated  14-FEB-1994   Robert E. Avery  and why not PTAU, while we're
C-                                                 at it. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
C  Output:
      INTEGER HALF
      REAL    ZVERT, DX, DY
C
C  Local:
C
      INTEGER MAX_CLUS 
      PARAMETER( MAX_CLUS = 20 )
C
      INTEGER GZPPHO, GZPELC, GZPMUO, GZPTAU
      INTEGER LPPHO, LPELC, LCLUS, LCACL,LPMUO,LPTAU 
      INTEGER LPOSITION 
      INTEGER IP,NIP_GD,IP_GD
      INTEGER IV,NZV,IVERT
      INTEGER HALFGD(MAX_CLUS)
      INTEGER LEN,II,JJ,I
      INTEGER OFFSET
      INTEGER ICLUS
C
      REAL PHI, THETA
      REAL PHI_GD(MAX_CLUS), THETA_GD(MAX_CLUS)
      REAL ZV(5), DZV(5)
      REAL RFDC, ZFDC(0:1)
      REAL ZCH, THETACH(0:1)
      REAL XCLUS,YCLUS,ZCLUS,RCLUS
      REAL ET
C
      LOGICAL FLGVAL,HARDCOPY 
C
      CHARACTER*80 FTEXT 
      CHARACTER*60 PROM1,PROM2
      CHARACTER*80 STRING
      CHARACTER*1 CHALF(0:1)
      CHARACTER*3 CTYPE
C
      DATA PROM1/' Enter Vertex number (default prev or 1)>'/
      DATA PROM2/' Enter Particle number (default prev or 1)>'/
      DATA CHALF/'N','S'/
      DATA RFDC, ZFDC /65.,-105.,105./
C----------------------------------------------------------------------
      HARDCOPY = FLGVAL('HARDCOPY')
C
C get road phi limits from PARH bank
C
      IP_GD = 0
      LPPHO = GZPPHO()
      LPELC = GZPELC()
      LPMUO = GZPMUO(0)
      LPTAU = GZPTAU()
      IF ((LPELC.LE.0).AND.(LPPHO.LE.0)
     &  .AND.(LPMUO.LE.0).AND.(LPTAU.LE.0) ) GO TO 999
C
C  Get z vertex. Use ZV(1); set to 0 if no primary vertex
C
      CALL ZVERTE(NZV,ZV,DZV)
      IF (NZV.EQ.0) THEN
        IVERT = 1
        ZV(IVERT) = 0.
C
C  Choose vertex if more than 1.
C
      ELSEIF (NZV.GT.1) THEN
        IF ( .NOT.HARDCOPY ) THEN
          CALL INTMSG(' ')
          FTEXT = ' Vertex Z(cm)'
          CALL INTMSG(FTEXT)
          DO IV =  1, NZV
            WRITE (FTEXT,101) IV,ZV(IV)
            CALL INTMSG(FTEXT)
          ENDDO
  101     FORMAT(1X,I4,F10.4)
C
          CALL OUTMSG('1')
          CALL OUTMSG(' Choose Vertex to use for roads')
          STRING=' '
          LEN=0
          CALL GETPAR(1,PROM1,'U',STRING)
          CALL SWORDS(STRING,II,JJ,LEN)
          IF (LEN.NE.0) READ(STRING(1:LEN),*,ERR=980) IVERT
        ENDIF
        IF(IVERT.LT.1 .OR. IVERT.GT.NZV) THEN
          IVERT = 1
          CALL OUTMSG(' Choice outside limits, use default value of 1.')
        ENDIF
      ELSE
        IVERT = 1
      ENDIF
C
C  Get MAX theta of each FDC chamber
C
      DO HALF =  0, 1
        ZCH = ZFDC(HALF) - ZV(IVERT)
        IF ( ZCH.EQ.0.0 ) ZCH = 0.01
        THETACH(HALF)=ATAN(RFDC/ZCH)
        IF (THETACH(HALF).LT.0) THETACH(HALF) = THETACH(HALF) + PI
      ENDDO
C
C  Display listing of roads.
C
      IF ( .NOT.HARDCOPY ) THEN
        CALL INTMSG(' ')
        FTEXT = '  Clus Ha Type   Phi       Theta    Et       Zvtx'
        CALL INTMSG(FTEXT)
      ENDIF
C
      DO I =  1, 4
        IF ( I.EQ.1 ) THEN
          LCLUS = GZPPHO()
          CTYPE = 'Pho'
          LPOSITION = LCLUS + 20
        ELSEIF ( I.EQ.2 ) THEN
          LCLUS = GZPELC()
          CTYPE = 'Ele'
          LPOSITION = LCLUS + 23
        ELSEIF ( I.EQ.3 ) THEN
          LCLUS = GZPMUO(0)
          CTYPE = 'Muo'
        ELSEIF ( I.EQ.4 ) THEN
          LCLUS = GZPTAU()
          CTYPE = 'Tau'
        ENDIF
        DO WHILE ( ( LCLUS.NE.0).AND.(IP_GD.LT.MAX_CLUS) ) 
          IF ( I.LE.2 ) THEN            ! EM Clusters
            LCACL = LQ(LCLUS-2)
            IF ( LCACL.GT.0  ) THEN
              LPOSITION = LCACL + 14
            ENDIF
C
            XCLUS = Q( LPOSITION )
            YCLUS = Q( LPOSITION + 1 )
            ZCLUS = Q( LPOSITION + 2 )
            ET    = Q( LCLUS + 7 )
            RCLUS = SQRT(XCLUS**2+YCLUS**2)
            ZCLUS = ZCLUS-ZV(IVERT) 
            THETA = ATAN(RCLUS/ZCLUS)
            IF (THETA.LT.0) THETA = THETA + PI
            PHI = ATAN2(YCLUS,XCLUS)
            IF (PHI .LT.0.) PHI = PHI + TWOPI
          ELSEIF ( I.EQ.3 ) THEN            ! Muons
            THETA = Q( LCLUS + 15 )
            IF (THETA.LT.0) THETA = THETA + PI
            PHI   = Q( LCLUS + 17 )
            IF (PHI .LT.0.) PHI = PHI + TWOPI
            ET = Q( LCLUS + 14 )
          ELSEIF ( I.EQ.4 ) THEN            ! Taus, angles only right for 
            THETA = Q( LCLUS + 8 )          !    first vertex.
            IF (THETA.LT.0) THETA = THETA + PI
            PHI   = Q( LCLUS + 9 )
            IF (PHI .LT.0.) PHI = PHI + TWOPI
            ET = Q( LCLUS + 7 )
          ENDIF
C
C  Check theta limits of road:
C
          IF ( THETA.GT.THETACH(0) ) THEN
            HALF = 0
          ELSEIF ( THETA.LT.THETACH(1) ) THEN
            HALF = 1
          ELSE
            GOTO 100
          ENDIF
C
C  If OK, then list
C 
          IP_GD = IP_GD + 1
          HALFGD(IP_GD) = HALF
          PHI_GD(IP_GD) = PHI
          THETA_GD(IP_GD) = THETA
C
          IF ( .NOT.HARDCOPY ) THEN
            WRITE (FTEXT,102) IP_GD, CHALF(HALF),CTYPE,
     &        PHI,THETA, ET, ZV(IVERT)
            CALL INTMSG(FTEXT)
          ENDIF
  102     FORMAT(1X,I4,2X,A,2X,A,4(F10.4) )
C
  100     CONTINUE
C
          LCLUS = LQ(LCLUS)
        ENDDO
      ENDDO
      NIP_GD = IP_GD 
C
C  Make choice of CLUS to display
C
      IF ( NIP_GD.EQ.0) THEN
        HALF=-1
        GOTO 999
      ELSEIF ( NIP_GD.EQ.1) THEN
        ICLUS = 1
      ELSE
        IF ( .NOT.HARDCOPY ) THEN
          CALL OUTMSG('1')
          CALL OUTMSG(' Choose a Particle road to Display ')
          STRING=' '
          LEN=0
          CALL GETPAR(1,PROM2,'U',STRING)
          CALL SWORDS(STRING,II,JJ,LEN)
          IF(LEN.NE.0) READ(STRING(1:LEN),*,ERR=980) ICLUS
        ENDIF
        IF(ICLUS.LE.0 .OR. ICLUS.GT.NIP_GD) THEN
C  User makes choice outside possible range, default to Track 1
          ICLUS = 1
          CALL OUTMSG(' Choice outside limits, use default value of 1.')
        END IF
      ENDIF
C
      HALF = HALFGD(ICLUS)
      ZVERT = ZV(IVERT)
      DX = TAN(THETA_GD(ICLUS)) * COS(PHI_GD(ICLUS))
      DY = TAN(THETA_GD(ICLUS)) * SIN(PHI_GD(ICLUS))
C
      GOTO 999
C----------------------------------------------------------------------
C
  980 CONTINUE
      CALL OUTMSG(' Error reading input.')
C
  999 RETURN
      END
