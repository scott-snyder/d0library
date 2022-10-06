      SUBROUTINE FCHT_EXPAND(HALF,UNIT,QUAD,SECTOR,WIRE,NHIT )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Uses contents of FCHT banks to fill HITS 
C-      array in the FDEVNT common block.
C-
C-   Inputs  : HALF,UNIT,QUAD,SECTOR,WIRE = channel specification
C-   Outputs : NHIT = number of hits returned for that channel.
C-             HITS in FDEVNT common block filled.
C-
C-   Created   3-NOV-1993   Robert E. Avery
C-   Updated  13-JUL-1994   Robert E. Avery   Peak height must be bilinear
C-        converted. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FDEVNT.INC'
C
      INTEGER NHIT
C
      INTEGER LFCHT,GZFCHT
      INTEGER PNT_FCHT
      INTEGER HALF,UNIT,QUAD,SECTOR, WIRE 
      INTEGER HIT,ERR
      INTEGER TSCALE
      INTEGER FIRSTHIT
      INTEGER LABEL  
      INTEGER STATUS
      INTEGER MAP(0:255)
      INTEGER PEAK
C      
      REAL    ETZERO,ATZERO,VELOP,VELOM
      REAL    TIME, AREA, WIDTH  
      REAL    ERROR_THETA, ERROR_PHI
      REAL    FDC_ERROR_SLOPE, VELOCT, VELOCP
C
      LOGICAL FIRST
C
      SAVE TSCALE, MAP
      SAVE ERROR_THETA, ERROR_PHI
      SAVE FIRST
C
      DATA TSCALE /64/ 
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      LFCHT = GZFCHT()
      IF ( LFCHT.LE.0  ) GOTO 999       ! 
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET_i('TSCALE',TSCALE,ERR)
        CALL EZGET('VELOCT',VELOCT,ERR)
        CALL EZGET('VELOCP',VELOCP,ERR)
        CALL EZGET_iarr('MAP(1)',MAP,ERR)
        CALL EZRSET
        ERROR_THETA = FDC_ERROR_SLOPE(0.0,0) * 10000./VELOCT
        ERROR_PHI = FDC_ERROR_SLOPE(0.0,1) * 10000./VELOCP
      ENDIF
C
      CALL FGTLTM( HALF,UNIT,QUAD,SECTOR,WIRE,
     &             ETZERO,ATZERO,VELOP,VELOM)
C
      CALL FCHTPT(HALF,UNIT,QUAD,SECTOR,WIRE,FIRSTHIT,NHIT)
C
      PNT_FCHT = LFCHT + FIRSTHIT
C
      DO HIT = 1, NHIT
C
        LABEL  = IBITS(IQ(PNT_FCHT), 0, 16)
        STATUS = IBITS(IQ(PNT_FCHT), 16, 4)
        WIDTH  = IBITS(IQ(PNT_FCHT), 20, 4) * 4. * NBPBIN 
        PEAK   = IBITS(IQ(PNT_FCHT), 24, 8)
        TIME   = IBITS(IQ(PNT_FCHT + 1), 0, 18) / TSCALE
        AREA   = IBITS(IQ(PNT_FCHT + 1), 18, 14)
C
        HITS(1,HIT,WIRE) = LABEL
        HITS(2,HIT,WIRE) = TIME - ETZERO       
        HITS(3,HIT,WIRE) = AREA     
        HITS(4,HIT,WIRE) = WIDTH
        HITS(5,HIT,WIRE) = MAP(PEAK)
        IF (UNIT.LE.0) THEN
          HITS(6,HIT,WIRE) = ERROR_THETA  
        ELSE
          HITS(6,HIT,WIRE) = ERROR_PHI
        END IF
C
        HITS(7,HIT,WIRE) = SQRT(AREA) 
        HITS(8,HIT,WIRE) = STATUS
        PNT_FCHT = PNT_FCHT + 2
      ENDDO

  999 RETURN
      END
