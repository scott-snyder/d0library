      SUBROUTINE VTXC_FILL_EMPTY
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Put average values of calibration constants
C-                for channels with no value
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  17-SEP-1992   M. Pang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:VTXCOFF.INC/LIST'
      INTEGER LAYER,SECTOR,WIRE,END
      INTEGER MXLAY,MXWIRE,NSEC(3)
      INTEGER IER,ICATG,IDRIFT,N
      INTEGER IW,IDRFT
      CHARACTER*32 MESSID,CALLER,LAYSEC
      CHARACTER*80 MESSAG
      REAL SUM
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL EZPICK( 'VTRAKS_RCP' )
        CALL EZGET( 'MXLAY', MXLAY, IER )
        CALL EZGET( 'MXWIRE', MXWIRE, IER )
        CALL EZGET( 'NSEC', NSEC, IER )
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
C  Fill empty Area correction factors with best estiamted values
C
      DO LAYER = 0,MXLAY
        DO ICATG = 0,2
          DO IDRIFT = -20,20
            IF ( AREA_DIST(IDRIFT,ICATG,LAYER) .EQ. 0. ) THEN
              N = 0
              SUM = 0.
              DO IDRFT = -20, 20
                IF ( AREA_DIST(IDRFT,ICATG,LAYER) .NE. 0. ) THEN
                  IF ( IABS(IDRFT) .GE. 4 ) THEN
                    N = N + 1
                    SUM = SUM + AREA_DIST(IDRFT,ICATG,LAYER)
                  END IF
                END IF
              END DO
              IF ( N .NE. 0 ) THEN
                AREA_DIST(IDRIFT,ICATG,LAYER) = SUM / FLOAT(N)
              ELSE
                AREA_DIST(IDRIFT,ICATG,LAYER) = 1.
              END IF
            END IF
          END DO
        END DO
      END DO
C
C  Fill empty Gain correction factors with best estiamted values
C
      DO LAYER = 0,MXLAY
        DO SECTOR = 0,NSEC(LAYER+1)
          DO WIRE = 0,7
            IF ( AREA_GAIN(WIRE,SECTOR,LAYER) .EQ. 0. ) THEN
              N = 0
              SUM = 0.
              DO IW = 0,7
                IF ( AREA_GAIN(IW,SECTOR,LAYER) .NE. 0. ) THEN
                  N = N + 1
                  SUM = SUM + AREA_GAIN(IW,SECTOR,LAYER)
                END IF
              END DO
              IF ( N .NE. 0 ) THEN
                AREA_GAIN(WIRE,SECTOR,LAYER) = SUM / FLOAT(N)
              ELSE
                WRITE(LAYSEC,300)LAYER,SECTOR
                MESSID = ' !POSSIBLE DEAD SECTOR!!!'//LAYSEC
                CALLER = ' VTXC_FILL_EMPTY'
                MESSAG = ' SET TO 1 '
                CALL ERRMSG(MESSID,CALLER,MESSAG,'I')
                AREA_GAIN(WIRE,SECTOR,LAYER) = 1.
              END IF
            END IF
          END DO
        END DO
      END DO
C
  300 FORMAT(2X,I1,2X,I2)
C
  999 RETURN
      END
