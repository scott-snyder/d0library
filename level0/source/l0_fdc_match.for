      SUBROUTINE L0_FDC_MATCH(END,ZVTX,XP,YP,NGOOD,LOCATION)
C--------------------------------------------------------------------------
C
C    Purpose and Methods : Find sectors in FDC chamber along a road
C
C    Input  : END    = 1=North, 2=South
C             ZVTX   = Z vertex position for road
C             XP, YP = level 0 pad center 
C    Output : LOCATION = array of track location info
C
C-   Updated   5-AUG-1992   Jeffrey Bantly   from FLFSEC.FOR
C
C--------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE,LOC,ICALL
      INTEGER STAT,IER
      INTEGER LOCATION(20,4)
      INTEGER IGOOD,END
      INTEGER NGOOD
C
      REAL ZVTX,L0Z(2),Z0(2)
      REAL PHI,THE,XP,YP,RP
      REAL DELANGP,DELANGT,DELZ,DELZ0
      REAL PHIMIN,PHIMAX,THEMIN,THEMAX,DEGRAD,THETA1,THETA2
      REAL PHIQUD,PHILO,PHIHI,WIDSEC,THELO,THEHI
      REAL XC,YC,ZC,RC,RCD,CELL_LEN(0:5,2),LENGTH,LENGTH0
      REAL WIDPAD,DELPHI,DELPHI0
      REAL EXTPHI,EXTTHE
      REAL EXTTHE0,EXTPHI0
      REAL THECUT(2)
      REAL DIMEN(6)
C  FUNCTION:
      INTEGER FDC_QUADTYPE
      EXTERNAL FDC_QUADTYPE
C
      LOGICAL FIRST,PHICHK,THECHK
C
      SAVE DEGRAD,FIRST,WIDSEC,CELL_LEN,WIDPAD
      DATA DEGRAD/0.017453/
      DATA WIDPAD/6.985/
      DATA FIRST/.TRUE./
C-------------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('EXTPHI',EXTPHI,IER)
        CALL EZGET('EXTTHE',EXTTHE,IER)
        CALL EZGET('WIDSEC',WIDSEC,IER)
        CALL EZGET_rarr('Z0',Z0,IER)
        CALL EZRSET
        CALL EZPICK('LEVEL0_RCP')
        CALL EZGET_rarr('L0Z',L0Z,IER)
        CALL EZRSET
        EXTTHE=0.0
        EXTPHI=0.0
        THECUT(1)=ATAN2(11.0,Z0(1))
        THECUT(2)=ATAN2(11.0,Z0(2))
        DO SECTOR =  0, MXSECT
C  A type
          CALL GTFWTX(0,5,SECTOR,DIMEN)
          CELL_LEN(SECTOR,1) = DIMEN(1)
C  B type
          CALL GTFWTX(0,4,SECTOR,DIMEN)
          CELL_LEN(SECTOR,2) = DIMEN(2)
        ENDDO
        FIRST=.FALSE.
      END IF
C
C  Calculate search limits based on pad size.
C
      NGOOD = 0
      IGOOD = 0
      DELZ0= L0Z(END)-ZVTX
      DELZ = Z0(END)-ZVTX
      RP=SQRT(XP**2 + YP**2)
      THE=ATAN2(RP,DELZ0)
      PHI=ATAN2(YP,XP)
      IF ( PHI.LT. 0.0 ) PHI = PHI + TWOPI
      DELANGP=ABS((ATAN2(WIDPAD,RP))/2.)
      PHIMIN=PHI-DELANGP
      PHIMAX=PHI+DELANGP
      IF ( PHIMIN.LT. 0.0 ) THEN
        PHIMIN=PHIMIN+TWOPI
        PHIMAX=PHIMAX+TWOPI
      ENDIF
      DELANGT=ABS((ATAN2(WIDPAD,ABS(DELZ0)))/2.)
      THEMIN=THE-DELANGT
      THEMAX=THE+DELANGT
      IF ( THEMIN.LT. 0.0 ) THEMIN = 0.0
      IF ( THEMAX.GT. PI ) THEMAX = PI
      HALF=END-1
      WIRE=0
C
C  Calculate FDC sector coverage and compare to desired pad for Theta
C  sectors.   Store location of matches.
C
      UNIT=0
      DO 200 QUAD=0,7
        IF (QUAD.LE.3) THEN
          PHIQUD=(45.+90.*FLOAT(QUAD))*DEGRAD
        ELSE
          PHIQUD=(90.*FLOAT(QUAD-4))*DEGRAD
        END IF
        PHILO=PHIQUD-WIDSEC
        PHIHI=PHIQUD+WIDSEC
        IF ( PHILO.LT. 0.0 ) THEN
          PHILO=PHILO+TWOPI
          PHIHI=PHIHI+TWOPI
        ENDIF
        PHICHK=.FALSE.
        IF ( PHIMIN.GE.PHILO ) THEN
          IF ( PHIMIN.LE.PHIHI) PHICHK=.TRUE.
        ELSE
          IF ( PHIMAX.GT.PHIHI ) PHICHK=.TRUE.
          IF ( PHIMIN.LT.(PHIHI-TWOPI)) PHICHK=.TRUE.
        ENDIF
        IF ( PHIMAX.GE.PHILO ) THEN
          IF ( PHIMAX.LE.PHIHI ) PHICHK=.TRUE.
        ENDIF
        IF (PHICHK) THEN
          DO 300 SECTOR=0,4             ! LEVEL0 only extends to sector 4
            WIRE=0
            LENGTH = CELL_LEN(SECTOR,FDC_QUADTYPE(QUAD,HALF))
            CALL GTFALH(HALF,UNIT,QUAD,SECTOR,WIRE,XC,YC,ZC)
            IF(ZC.EQ. 0.0) GOTO 300
            RC=SQRT(XC**2+YC**2)
            DELPHI=ABS(ATAN2(LENGTH,RC))
            DELPHI0=ABS(PHI-PHIQUD)
            IF ( DELPHI0.GT.DELPHI ) THEN
              DELPHI0=ABS(PHI-PHIQUD+TWOPI)
              IF ( DELPHI0.GT.DELPHI ) GOTO 200
            ENDIF
            LENGTH0=LENGTH*(DELPHI0/DELPHI)
            RCD=SQRT(RC**2+LENGTH0**2)
            EXTTHE0=EXTTHE
            IF (SECTOR.LT.3) THEN
              EXTTHE0=EXTTHE+5.74
              IF ( DELPHI0.GT. 0.0 ) EXTTHE0=EXTTHE0/COS(DELPHI0)
              IF (SECTOR.EQ.1) THEN
                THETA1=ATAN2(RCD-EXTTHE0,DELZ)
                THETA2=ATAN2(RCD+EXTTHE,DELZ)
              ELSE
                THETA1=ATAN2(RC-EXTTHE,DELZ)
                THETA2=ATAN2(RCD+EXTTHE0,DELZ)
              END IF
            ELSE
              EXTTHE0=EXTTHE+5.33
              IF ( DELPHI0.GT. 0.0 ) EXTTHE0=EXTTHE0/COS(DELPHI0)
              THETA1=ATAN2(RCD-EXTTHE0,DELZ)
              THETA2=ATAN2(RCD+EXTTHE0,DELZ)
            END IF
            IF (THETA1.LT.THETA2) THEN
              THELO=THETA1
              THEHI=THETA2
            ELSE
              THELO=THETA2
              THEHI=THETA1
            END IF
            THECHK=.FALSE.
            IF ( THEMIN.GE.THELO ) THEN
              IF ( THEMIN.LE.THEHI) THECHK=.TRUE.
            ELSE
              IF ( THEMAX.GT.THEHI) THECHK=.TRUE.
            ENDIF
            IF ( THEMAX.GE.THELO ) THEN
              IF ( THEMAX.LE.THEHI ) THECHK=.TRUE.
            ENDIF
            IF ( THECHK ) THEN
              IGOOD=IGOOD+1
              IF ( IGOOD.GT.20 ) GOTO 100
              LOCATION(IGOOD,1)=HALF
              LOCATION(IGOOD,2)=UNIT
              LOCATION(IGOOD,3)=QUAD
              LOCATION(IGOOD,4)=SECTOR
            ENDIF
  300     CONTINUE
        END IF
  200 CONTINUE
C                       ! Loop over Phi sectors
      UNIT=1
      QUAD=0
      IF ( THE.LT.HALFPI ) THEN
        IF ( THEMAX.GT.THECUT(2) ) THECHK=.TRUE.
      ELSE
        IF ( THEMIN.LT.THECUT(1) ) THECHK=.TRUE.
      ENDIF
      IF ( THECHK ) THEN
        DO 400 SECTOR=0,35
          PHILO=(10.*FLOAT(SECTOR))*DEGRAD - EXTPHI
          PHIHI=(10.*FLOAT(SECTOR+1))*DEGRAD + EXTPHI
          PHICHK=.FALSE.
          IF ( PHILO.LT. 0.0 ) THEN
            PHIHI=PHIHI+TWOPI
            PHILO=PHILO+TWOPI
          ENDIF
          IF ( PHIMIN.GE.PHILO ) THEN
            IF ( PHIMIN.LE.PHIHI) PHICHK=.TRUE.
          ELSE
            IF ( PHIMAX.GT.PHIHI ) PHICHK=.TRUE.
            IF ( PHIMIN.LT.(PHIHI-TWOPI)) PHICHK=.TRUE.
          ENDIF
          IF ( PHIMAX.GE.PHILO ) THEN
            IF ( PHIMAX.LE.PHIHI ) PHICHK=.TRUE.
          ENDIF
          IF (PHICHK) THEN
            IGOOD=IGOOD+1
            IF ( IGOOD.GT.20 ) GOTO 100
            LOCATION(IGOOD,1)=HALF
            LOCATION(IGOOD,2)=UNIT
            LOCATION(IGOOD,3)=QUAD
            LOCATION(IGOOD,4)=SECTOR
          END IF
  400   CONTINUE
      ENDIF
C
C  Done.
C
  100 CONTINUE
      NGOOD = IGOOD
C-------------------------------------------------------------------------
  999 RETURN
      END
