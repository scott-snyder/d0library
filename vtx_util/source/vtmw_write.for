      SUBROUTINE VTMW_WRITE(T0VTMW,SIVTMW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write offline TZEROS into a Zebra file
C-
C-   Inputs  : T0VTMW, SIVTMW: Tzeros and sigmas for all VTX channels
C-   Outputs : 
C-   Controls: 
C-
C-   Created  28-JUL-1992  V. D.Elvira and P. Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE      
      INTEGER IR,IER,IERR,IERRR,LUN,ILEN
      INTEGER LVTMW,GZVTMW,GZVTMH,GZSVTX,LVDTM,LENGTH
      INTEGER LAYER,SECTOR,WIRE,END,SECMAX,POINTER
      INTEGER FIRSTRUN, VTXC_FIRST
C----------------------------------------------------------------------
      REAL T0VTMW(0:2,0:31,0:7,0:1),SIVTMW(0:2,0:31,0:7,0:1)
C----------------------------------------------------------------------
      LOGICAL OK,XCHANGE,WVTMH,WVTMHA,FIRST
C----------------------------------------------------------------------
      CHARACTER*6  RUNNUM
      CHARACTER*10 XCHOP
      CHARACTER*50 AREA
      CHARACTER*70 TITZEB,TITASC
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('VTXCOFF_RCP')
        CALL EZGET('XCHANGE',XCHANGE,IR)
        CALL EZGET('WVTMH',WVTMH,IR)
        CALL EZGET('WVTMHA',WVTMHA,IR)
        CALL EZGETS('OUTPUT_AREA',1,AREA,LENGTH,IR)
        CALL EZRSET
      ENDIF
C----------------------------------------------------------------------
C-  Read in VTX_D0STPFILE if necessary
C----------------------------------------------------------------------
      LSVTX = GZSVTX()
      IF ( LSVTX .LE. 0 ) CALL VTISTP('D0$STP:VTX_D0STPFILE.DAT',IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('File not found','VTMW_WRITE',
     &    'STP file not found, T0s not written','W')
        GO TO 999
      ENDIF
C----------------------------------------------------------------------
C-  Correct offline tzeros are written into VTMW bank and VDTM banks 
C-  are dropped
C----------------------------------------------------------------------
      DO LAYER=0,2
        LVTMW=GZVTMW(LAYER)
        IF (LAYER.EQ.0) THEN
          SECMAX=15
        ELSE
          SECMAX=31
        ENDIF
        DO SECTOR=0,SECMAX
          DO WIRE=0,7
            POINTER=LVTMW+(SECTOR*IC(LVTMW+4)+WIRE)*5+5
            C(POINTER+1)=T0VTMW(LAYER,SECTOR,WIRE,0)
            C(POINTER+2)=SIVTMW(LAYER,SECTOR,WIRE,0)
            C(POINTER+3)=T0VTMW(LAYER,SECTOR,WIRE,1)
            C(POINTER+4)=SIVTMW(LAYER,SECTOR,WIRE,1)
          ENDDO
          LVDTM=LC(LVTMW-SECTOR-1)
          IF (LVDTM.GT.0) CALL MZDROP(IXSTP,LVDTM,' ')  ! drop DTM banks
        ENDDO
      ENDDO
C----------------------------------------------------------------------
C- VTMH and it's hanging banks are written into a file
C----------------------------------------------------------------------
C
C ****  Get run number of earliest run processed and use it in the output file
C ****  names.  Also store it in run validity words of the time banks
C
      FIRSTRUN = VTXC_FIRST()
      WRITE(RUNNUM,'(I6)') FIRSTRUN
      TITZEB=AREA//'VTMH_'//RUNNUM//'.DAT'
      TITASC=AREA//'TIMES_'//RUNNUM//'.DAT'
      IF ( WVTMHA ) CALL VTMW_TO_ASCII(TITASC)
      IF ( WVTMH ) THEN
        CALL GTUNIT(50,LUN,IERR)  ! get an available unit number 
        IF (XCHANGE) THEN
          CALL D0OPEN(LUN,TITZEB,'OX',OK)  ! open an
                                                               ! x-change
          CALL XZRECL(ILEN,XCHOP)                    ! get ILEN and XCHOP
          CALL FZFILE(LUN,ILEN,XCHOP)                ! file declared to ZEBRA
        ELSE
          CALL D0OPEN(LUN,TITZEB,'OU',OK)
          CALL FZFILE(LUN,0,'O')
        ENDIF
C----------------------------------------------------------------------
C- Write out structure VTMH and below
C----------------------------------------------------------------------
        LVTMH = GZVTMH()
        IF (LVTMH.GT.0) THEN
          IC(LVTMH+1) = FIRSTRUN
          IC(LVTMH+2) = 999999
          DO LAYER = 0, 2
            LVTMW = GZVTMW(LAYER)
            IC(LVTMW+1) = FIRSTRUN
            IC(LVTMW+2) = 999999
          ENDDO
          CALL FZOUT(LUN,IDVSTP,LVTMH,1,' ',0,0,0) !write one data stuct.
        ELSE
          CALL ERRMSG('ERROR','WRITE_VTMW',' ','S')
        ENDIF
        CALL FZENDO(LUN,'QTU') ! terminate output file (end-of-run) and
                               ! position for furthur output
        CALL RLUNIT(50,LUN,IERRR) ! get available unit number
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
