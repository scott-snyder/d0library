      SUBROUTINE FSTPAR_V2
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Generate the 2nd order alignment banks for
C-                         the FDCs, based on survey + tracks from FDC.  
C-                         Data obtained from FSTP.RCP file.
C-
C-
C-   Created  16-FEB-1993   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDPRM2.INC'
      INCLUDE 'D0$LINKS:IZFALH.LINK'
C
      INTEGER IER
      INTEGER LOWRUN, HIGRUN, NUMTWR, NUMPWR
      INTEGER NPARWR, NPARDL, NUMTDL, NUMPDL
      INTEGER HALF, THETA, UNIT, QUAD, SECTOR
      INTEGER WIRE, DELAY, I, J
      INTEGER LBANK, LKFALH, LKFASE
      PARAMETER( NUMTWR = 8 )
      PARAMETER( NUMTDL = 1 )
      PARAMETER( NUMPWR = 16 )
      PARAMETER( NUMPDL = 0 )
      INTEGER GZFALH
      INTEGER LZLAST,LZFIND
C
      REAL    NVECTOR2(3),NMATRIX2(3,3)
      REAL    SVECTOR2(3),SMATRIX2(3,3)
      REAL    XCENT,YCENT,ZCENT
      REAL    XCENP,YCENP,ZCENP
C
      CHARACTER*(*) RCPFIL
C
      PARAMETER( RCPFIL = 'FSTP_RCP' )   ! Logical name of control file
C
      LOGICAL OK,EZERROR
C
      DATA LOWRUN,HIGRUN,NPARWR,NPARDL / 999999998, 999999999, 6, 6/
C----------------------------------------------------------------------
C
C  Create the temporary link area FDPRM2, if not already done.
C
      IF ( FDPRM2(1).EQ.0 ) THEN
        CALL MZLINT(IXSTP,'/FDPRM2/',FDPRM2,LFASE(1,1,7,35),FDPRM2)
      ENDIF
C
C  Read in 1st order alignment.
C
      CALL EZPICK('FSTP_RCP')
      IF( EZERROR(IER) ) THEN
        CALL ERRMSG('FSTP','FSTPAR','FSTP RCP file missing','F')
      ELSE
        CALL EZGET('NVECTOR2',NVECTOR2,IER)
        CALL EZGET('SVECTOR2',SVECTOR2,IER)
        CALL EZGET('NMATRIX2',NMATRIX2,IER)
        CALL EZGET('SMATRIX2',SMATRIX2,IER)
        CALL EZRSET
      ENDIF
C
C  Generate a new FALH structure in a linear chain after the last.
C  Make top level alignment bank FALH as next bank in FALH linear chain.
C
      LKFALH = LSFDC-IZFALH
      LKFALH = LZLAST(0,LKFALH)
      IF ( LKFALH.EQ.0 ) GOTO 999
C
      CALL MZBOOK ( IDVSTP, LFALH2, LKFALH, 0, 'FALH',
     &     3,3,2,2, -1 )
      IC( LFALH2-5 ) = 2
      IC( LFALH2+1 ) = LOWRUN
      IC( LFALH2+2 ) = HIGRUN
C
C  Copy alignment banks 0 (design) to new FALH header.
C
      LKFALH = LC(LSFDC-IZFALH)
      LKFALH = LZFIND(IXSTP,LKFALH,0,-5)
      IF ( LKFALH.EQ.0 ) GOTO 999
C
      DO 5 HALF=0,1
        LFAHF(HALF)=LC(LKFALH-(HALF+1))
        IF(LFAHF(HALF).LE.0) THEN
          CALL ERRMSG('FSTP','FSTPAR','Half alignment bank missing','F')
        ENDIF
        CALL MZCOPY(IDVSTP,LFAHF(HALF),IDVSTP,LFALH2,-(HALF+1),'L')
    5 CONTINUE
C
C  Loop over all wires and shift their positions by VECTOR and rotate them
C  by MATRIX2.
C
      DO 10 HALF = 0, 1
        LFAHF(HALF)=LC(LFALH2-(HALF+1))
        IF(LFAHF(HALF).LE.5) THEN
          CALL ERRMSG('FSTP','FSTPAR','Half new align bank missing','W')
          GOTO 10
        ENDIF
        IC( LFAHF(HALF)-5 ) = HALF
        IC( LFAHF(HALF)+1 ) = LOWRUN
        IC( LFAHF(HALF)+2 ) = HIGRUN
C
C   Alignment Banks for the Theta Chambers
C
        UNIT = 0
        LFAUN(HALF,UNIT)=LC(LFAHF(HALF)-(UNIT+1))
        IF(LFAUN(HALF,UNIT).LE.5) THEN
          CALL ERRMSG('FSTP','FSTPAR','Half new align bank missing','W')
          GOTO 20
        ENDIF
        IC( LFAUN(HALF,UNIT)-5 ) = UNIT
        IC( LFAUN(HALF,UNIT)+1 ) = LOWRUN
        IC( LFAUN(HALF,UNIT)+2 ) = HIGRUN
        DO 30 QUAD = 0, 7
          THETA = 0
          IF( QUAD.GE.4) THETA = 1
          LFAQD(HALF,QUAD)=LC(LFAUN(HALF,UNIT)-(QUAD+1))
          IF(LFAQD(HALF,QUAD).LE.5) THEN
            CALL ERRMSG('FSTP','FSTPAR','Quad new align bank missing',
     &                                                            'W')
            GOTO 30
          ENDIF
          IC( LFAQD(HALF,QUAD)-5 ) = QUAD
          IC( LFAQD(HALF,QUAD)+1 ) = LOWRUN
          IC( LFAQD(HALF,QUAD)+2 ) = HIGRUN
          DO 40 SECTOR = 0, 5
            LFASE(HALF,UNIT,QUAD,SECTOR)=LC(LFAQD(HALF,QUAD)-(SECTOR+1))
            IF(LFASE(HALF,UNIT,QUAD,SECTOR).LE.5) THEN
              CALL ERRMSG('FSTP','FSTPAR',
     &                    'Sector new align bank missing','W')
              GOTO 40
            ENDIF
            LKFASE = LFASE(HALF,UNIT,QUAD,SECTOR)
            LKFASE = LKFASE + 6
            DO 50 WIRE = 0, NUMTWR-1
              XCENT  = C( LKFASE+1 )
              YCENT  = C( LKFASE+2 )
              ZCENT  = C( LKFASE+3 )
              IF ( HALF.EQ.0 ) THEN
                C( LKFASE+1 ) = XCENT*NMATRIX2(1,1)+YCENT*NMATRIX2(1,2)
     &          + ZCENT*NMATRIX2(1,3) + NVECTOR2(1)
                C( LKFASE+2 ) = XCENT*NMATRIX2(2,1)+YCENT*NMATRIX2(2,2)
     &          + ZCENT*NMATRIX2(2,3) + NVECTOR2(2)
                C( LKFASE+3 ) = XCENT*NMATRIX2(3,1)+YCENT*NMATRIX2(3,2)
     &          + ZCENT*NMATRIX2(3,3) + NVECTOR2(3)
              ELSE
                C( LKFASE+1 ) = XCENT*SMATRIX2(1,1)+YCENT*SMATRIX2(1,2)
     &          + ZCENT*SMATRIX2(1,3) + SVECTOR2(1)
                C( LKFASE+2 ) = XCENT*SMATRIX2(2,1)+YCENT*SMATRIX2(2,2)
     &          + ZCENT*SMATRIX2(2,3) + SVECTOR2(2)
                C( LKFASE+3 ) = XCENT*SMATRIX2(3,1)+YCENT*SMATRIX2(3,2)
     &          + ZCENT*SMATRIX2(3,3) + SVECTOR2(3)
              ENDIF
              LKFASE = LKFASE + NPARWR
   50       CONTINUE
            DO 60 DELAY = 0, NUMTDL-1             !  adjacent to wire 0
              XCENT  = C( LKFASE+1 )
              YCENT  = C( LKFASE+2 )
              ZCENT  = C( LKFASE+3 )
              IF ( HALF.EQ.0 ) THEN
                C( LKFASE+1 ) = XCENT*NMATRIX2(1,1)+YCENT*NMATRIX2(1,2)
     &          + ZCENT*NMATRIX2(1,3) + NVECTOR2(1)
                C( LKFASE+2 ) = XCENT*NMATRIX2(2,1)+YCENT*NMATRIX2(2,2)
     &          + ZCENT*NMATRIX2(2,3) + NVECTOR2(2)
                C( LKFASE+3 ) = XCENT*NMATRIX2(3,1)+YCENT*NMATRIX2(3,2)
     &          + ZCENT*NMATRIX2(3,3) + NVECTOR2(3)
              ELSE
                C( LKFASE+1 ) = XCENT*SMATRIX2(1,1)+YCENT*SMATRIX2(1,2)
     &          + ZCENT*SMATRIX2(1,3) + SVECTOR2(1)
                C( LKFASE+2 ) = XCENT*SMATRIX2(2,1)+YCENT*SMATRIX2(2,2)
     &          + ZCENT*SMATRIX2(2,3) + SVECTOR2(2)
                C( LKFASE+3 ) = XCENT*SMATRIX2(3,1)+YCENT*SMATRIX2(3,2)
     &          + ZCENT*SMATRIX2(3,3) + SVECTOR2(3)
              ENDIF
              LKFASE = LKFASE + NPARDL
   60       CONTINUE
   40     CONTINUE
   30   CONTINUE
C
C   Alignment Banks for the Phi Chambers
C
   20   CONTINUE
        UNIT=1
        QUAD=0
        LFAUN(HALF,UNIT)=LC(LFAHF(HALF)-(UNIT+1))
        IF(LFAUN(HALF,UNIT).LE.5) THEN
          CALL ERRMSG('FSTP','FSTPAR','Unit new align bank missing','W')
          GOTO 10
        ENDIF
        IC( LFAUN(HALF,UNIT)-5 ) = UNIT
        IC( LFAUN(HALF,UNIT)+1 ) = LOWRUN
        IC( LFAUN(HALF,UNIT)+2 ) = HIGRUN
        DO 70 SECTOR = 0, 35
          LFASE(HALF,UNIT,QUAD,SECTOR)=LC(LFAUN(HALF,UNIT)-(SECTOR+1))
          IF(LFASE(HALF,UNIT,QUAD,SECTOR).LE.5) THEN
            CALL ERRMSG('FSTP','FSTPAR','Sector new align bank missing',
     &                                                              'W')
            GOTO 70
          ENDIF
          LKFASE = LFASE(HALF,UNIT,QUAD,SECTOR)
          LKFASE = LKFASE + 6
          DO 80 WIRE = 0, NUMPWR-1
            XCENP  = C( LKFASE+1 )
            YCENP  = C( LKFASE+2 )
            ZCENP  = C( LKFASE+3 )
            IF ( HALF.EQ.0 ) THEN
              C( LKFASE+1 ) = XCENP*NMATRIX2(1,1) + YCENP*NMATRIX2(1,2)
     &          + ZCENP*NMATRIX2(1,3) + NVECTOR2(1)
              C( LKFASE+2 ) = XCENP*NMATRIX2(2,1) + YCENP*NMATRIX2(2,2)
     &          + ZCENP*NMATRIX2(2,3) + NVECTOR2(2)
              C( LKFASE+3 ) = XCENP*NMATRIX2(3,1) + YCENP*NMATRIX2(3,2)
     &          + ZCENP*NMATRIX2(3,3) + NVECTOR2(3)
            ELSE
              C( LKFASE+1 ) = XCENP*SMATRIX2(1,1) + YCENP*SMATRIX2(1,2)
     &          + ZCENP*SMATRIX2(1,3) + SVECTOR2(1)
              C( LKFASE+2 ) = XCENP*SMATRIX2(2,1) + YCENP*SMATRIX2(2,2)
     &          + ZCENP*SMATRIX2(2,3) + SVECTOR2(2)
              C( LKFASE+3 ) = XCENP*SMATRIX2(3,1) + YCENP*SMATRIX2(3,2)
     &          + ZCENP*SMATRIX2(3,3) + SVECTOR2(3)
            ENDIF
            LKFASE = LKFASE + NPARWR
   80     CONTINUE
   70   CONTINUE
   10 CONTINUE
C
C---------------------------------------------------------------------------
  999 CONTINUE
      FDPRM2(1) = 0
      RETURN
      END
