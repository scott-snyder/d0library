      SUBROUTINE VTXC_WRITE_GAINS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write final gain constants out into a ascii
C-                  and also Zebra file
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  18-SEP-1992   M. Pang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:VTXCOFF.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER NWFADC,NBFADC,MPVGNL(5)
      INTEGER LAYER,SECMAX,LVGNL,GZVGNH
      INTEGER LOWRUN,HIGRUN,ICATG,IDRIFT
      INTEGER N,SECTOR,ADC,IP,WIRE,END
      PARAMETER( NWFADC = 1 )
      PARAMETER( NBFADC = 16)
      INTEGER NSEC(3),LUN0,LUN1,LUN2
      INTEGER IUSER,IERR,IER,MXLAY
      INTEGER MXWIRE,RUNNO,ILEN,LENGTH
      INTEGER FIRSTRUN, VTXC_FIRST
      CHARACTER*50 AREA
      CHARACTER*75 GAINFILE, AREAFILE, VGNHFILE
      CHARACTER*10 CHOPT
      CHARACTER*6  RUNNUM
      LOGICAL FIRST,WVGNH,WVGNHA,OPENED,XCHANGE
      DATA FIRST /.TRUE./
      DATA IUSER /777/
      DATA    MPVGNL / 0, 0, 0, 0, 0 /
      DATA LOWRUN, HIGRUN / 0, 999999/
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL EZPICK( 'VTRAKS_RCP' )
        CALL EZGET_i( 'MXLAY', MXLAY, IER )
        CALL EZGET_i( 'MXWIRE', MXWIRE, IER )
        CALL EZGET_iarr( 'NSEC', NSEC, IER )
        CALL EZRSET
        CALL EZPICK( 'VTXCOFF_RCP' )
        CALL EZGET_l( 'WVGNH', WVGNH, IER )
        CALL EZGET_l( 'WVGNHA', WVGNHA, IER )
        CALL EZGET( 'XCHANGE', XCHANGE, IER )
        CALL EZGETS('OUTPUT_AREA',1,AREA,LENGTH,IER)
        CALL EZRSET
        FIRST = .FALSE.
        CALL UCTOH('VGNL',MPVGNL(1),4,4)
      END IF
C
C  WRITE OUT FILES IF REQUESTED
C
      FIRSTRUN = VTXC_FIRST()   ! Get run number of earliest run processed
      WRITE(RUNNUM,'(I6)') FIRSTRUN
      IF ( WVGNHA ) THEN
        GAINFILE = AREA//'GAINS_'//RUNNUM//'.DAT'
        AREAFILE = AREA//'AREAS_'//RUNNUM//'.DAT'
        CALL GTUNIT(IUSER,LUN0,IERR)
        CALL GTUNIT(IUSER,LUN1,IERR)
        CALL D0OPEN(LUN0,GAINFILE,'OF',OPENED)
        IF ( .NOT. OPENED ) THEN
          CALL ERRMSG('File open error','VTXC_WRITE_GAINS',
     &      'Cannot open ASCII file for gains','W')
          GO TO 999
        ENDIF
        CALL D0OPEN(LUN1,AREAFILE,'OF',OPENED)
        IF ( .NOT. OPENED ) THEN
          CALL ERRMSG('File open error','VTXC_WRITE_GAINS',
     &      'Cannot open ASCII file for areas','W')
          GO TO 999
        ENDIF
      END IF
      IF ( WVGNH ) THEN
        VGNHFILE = AREA//'VGNH_'//RUNNUM//'.DAT'
        CALL GTUNIT(IUSER,LUN2,IERR)
        IF ( XCHANGE ) THEN
          CALL D0OPEN(LUN2,VGNHFILE,'OX',OPENED)
          IF ( .NOT. OPENED ) THEN
            CALL ERRMSG('File open error','VTXC_WRITE_GAINS',
     &        'Cannot open ZEBRA file for VGNH structure','W')
            GO TO 999
          ENDIF
        ELSE
          CALL D0OPEN(LUN2,VGNHFILE,'OU',OPENED)
          IF ( .NOT. OPENED ) THEN
            CALL ERRMSG('File open error','VTXC_WRITE_GAINS',
     &        'Cannot open ZEBRA file for VGNH structure','W')
            GO TO 999
          ENDIF
        ENDIF
        CALL XZRECL(ILEN,CHOPT)
        CALL FZFILE(LUN2,ILEN,CHOPT)
      END IF
C
C  DROP VGNH BANK AND BELOW
C
      LVGNH = GZVGNH()
      CALL MZDROP(IXSTP,LVGNH,' ')
C
C  BOOK NEW VGNH BANK AND FILL RUN VALIDITY WORDS
C
      LVGNH = GZVGNH()
      IF ( LVGNH .LE. 0 ) THEN
        CALL BKVGNH( LVGNH )
        IC(LVGNH + 1) = FIRSTRUN  ! Use earliest run processed as key for DBL3
        IC(LVGNH + 2) = HIGRUN
      ENDIF
C
C  Write Area correction factors
C
      CALL MZFORM( 'VGNL', '5I -F', MPVGNL(5) )
      DO LAYER = 0,MXLAY
        SECMAX = NSEC(LAYER+1) + 1
        MPVGNL(4) = 5 + 3*41 + SECMAX*NBFADC*NWFADC
        LVGNH = GZVGNH()
        CALL MZLIFT( IDVSTP, LVGNL, LVGNH, -(LAYER+1), MPVGNL, -1)
        IC( LVGNL-5 ) = LAYER
        IC( LVGNL+1 ) = FIRSTRUN
        IC( LVGNL+2 ) = HIGRUN
        IC( LVGNL+3 ) = NWFADC
        IC( LVGNL+4 ) = NBFADC
        IC( LVGNL+5 ) = SECMAX
        N = 0
        DO ICATG = 0,2
          DO IDRIFT = -20,20
            N = N + 1
            C(LVGNL+5+N) = AREA_DIST(IDRIFT,ICATG,LAYER)
            IF ( WVGNHA ) THEN
              WRITE(LUN1,500) LAYER,ICATG,IDRIFT,
     &                        AREA_DIST(IDRIFT,ICATG,LAYER)
            END IF
          END DO
        END DO
        DO SECTOR = 0,NSEC(LAYER+1)
          DO 30 ADC = 0, NBFADC-1
            IP = LVGNL + ( SECTOR*NBFADC + ADC ) * NWFADC + 5 + 3*41
            WIRE = INT(FLOAT(ADC)/2.)
            END = MOD(ADC,2)
            IF ( END .EQ. 0 ) THEN
              C( IP+1 ) = AREA_GAIN(WIRE,SECTOR,LAYER) /
     &              ( 1. - EPSILON(WIRE,SECTOR,LAYER)/2. )
            ELSE IF ( END .EQ. 1 ) THEN
              C( IP+1 ) = AREA_GAIN(WIRE,SECTOR,LAYER) /
     &              ( 1. + EPSILON(WIRE,SECTOR,LAYER)/2. )
            END IF
            IF ( WVGNHA ) THEN
              WRITE(LUN0,600) LAYER,SECTOR,WIRE,END,C(IP+1)
            END IF
   30     CONTINUE
        END DO
      END DO
C
  500 FORMAT(1X,I1,2X,I1,2X,I4,2X,F7.4)
  600 FORMAT(1X,I1,2X,I2,2X,I1,2X,I1,2X,F7.4)
C
      IF ( WVGNH ) THEN
        CALL FZOUT(LUN2,IDVSTP,LVGNH,1,' ',0,0)
        CALL FZENDO(LUN2,'QT')
        CLOSE(LUN2)
        CALL RLUNIT(IUSER,LUN2,IERR)
      END IF
C
      IF ( WVGNHA ) THEN
        CLOSE(LUN0)
        CLOSE(LUN1)
        CALL RLUNIT(IUSER,LUN0,IERR)
        CALL RLUNIT(IUSER,LUN1,IERR)
      END IF
C
  999 RETURN
      END
