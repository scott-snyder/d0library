C VAX/DEC CMS REPLACEMENT HISTORY, Element FCHTFL.FOR
C *1     9-NOV-1993 17:54:39 AVERY "updates in FDC for v12 RECO"
C VAX/DEC CMS REPLACEMENT HISTORY, Element FCHTFL.FOR
      SUBROUTINE FCHTFL(NHITS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill compressed hit bank (FCHT) 
C-      bank from FxDA banks. This bank is essentially the same in 
C-      form as the level 2 hit bank, CDH3, except that all hits
C-      are in one "crate" (ie. no crate ordering).
C-
C-   Inputs  : NHITS  = Number of hits in FxDA BANKS.
C-             Contents of FxDA BANKS.
C-   Outputs : FDC hit data in (new) FCHT bank.
C-   Controls: none.
C-
C-   Created  25-OCT-1993   Robert E. Avery from FCDH3FL by Chris Klopfenstein
C_
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER NHITS
C
      INTEGER LFCHT,GZFCHT
      INTEGER LFXDA,GZFXDA
      INTEGER PNT_FCHT,PNT_FXDA 
      INTEGER NHITWIRE,NWDS_FXDA,MAXWIR  
      INTEGER HALF,UNIT,QUAD,SECTOR, WIRE 
      INTEGER IHIT,ERR
      INTEGER ID,ITIME,IAREA,IWIDTH,ISTATUS,IPEAK   
      INTEGER HEADER_LENGTH 
      INTEGER TRAILER_LENGTH
      INTEGER NWORDS 
      INTEGER MAX_QUAD(0:1), MAX_SECT(0:1)
      INTEGER TSCALE,TMAX,ARMAX,WIDMAX
      INTEGER ICOMPRESSED 
      PARAMETER( ICOMPRESSED  = 12 )
C      
      REAL    ETZERO,ATZERO,VELOP,VELOM
      REAL    TIME, AREA, PEAK, WIDTH  
C
      LOGICAL FIRST
C
      SAVE MAX_QUAD, MAX_SECT 
      SAVE TSCALE,TMAX,ARMAX,WIDMAX
      SAVE FIRST
C
      DATA TRAILER_LENGTH /2/
      DATA MAX_QUAD / MXQUAD, 0 /
      DATA MAX_SECT / MXSECT, MXSECP / 
      DATA TSCALE,TMAX,ARMAX,WIDMAX /64,262143,16383,15/
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      LFCHT = GZFCHT()
      IF ( LFCHT.GT.0  ) GOTO 999       ! Already done
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('TSCALE',TSCALE,ERR)
        CALL EZGET('TMAX',TMAX,ERR)
        CALL EZGET('ARMAX',ARMAX,ERR)
        CALL EZGET('WIDMAX',WIDMAX,ERR)
        CALL EZRSET
      ENDIF
C
C  Book result bank FCHT
C
      CALL BKFCHT(LFCHT,NHITS)
C
      HEADER_LENGTH = IQ(LFCHT+1) 
      PNT_FCHT = LFCHT + HEADER_LENGTH
C
C  find Hits
C
      DO HALF = 0, MXHALF
        DO UNIT = 0, MXUNIT
          DO QUAD =  0,MAX_QUAD(UNIT)
            DO SECTOR =  0, MAX_SECT(UNIT)
C
              LFXDA = GZFXDA(HALF,UNIT,QUAD,SECTOR)
              IF ( LFXDA .LE. 0 ) GOTO 100
              IF ( BTEST( IQ(LFXDA),ICOMPRESSED ) ) GOTO 100     
              IQ(LFXDA) = IBSET( IQ(LFXDA),ICOMPRESSED ) 
C
              MAXWIR  = IQ( LFXDA+2 )
              NWDS_FXDA  = IQ( LFXDA+3 )
              PNT_FXDA = LFXDA + 3 + 2*MAXWIR
C
              DO WIRE = 0, MAXWIR-1
                CALL FGTLTM(HALF,UNIT,QUAD,SECTOR,WIRE,
     &            ETZERO,ATZERO,VELOP,VELOM)
C
                NHITWIRE = IQ( LFXDA+4+WIRE )
                DO IHIT = 1, NHITWIRE
C
                  TIME   = Q(PNT_FXDA+2) + ETZERO
                  AREA   = Q(PNT_FXDA+3)
                  PEAK   = Q(PNT_FXDA+5) 
                  WIDTH  = Q(PNT_FXDA+4)/NBPBIN 
C
                  ID      = IQ(PNT_FXDA+1)
                  ITIME   = MIN( NINT(TIME * TSCALE), TMAX )
                  IAREA   = MIN( NINT(AREA), ARMAX )
                  IWIDTH  = MIN( NINT(WIDTH/4), WIDMAX )
                  ISTATUS = IAND( IQ(PNT_FXDA+8),15 )
                  IPEAK   = MAX( PEAK,0. )
C
                  IQ(PNT_FCHT+1) = ID + ISHFT(ISTATUS, 16) 
     &                + ISHFT(IWIDTH, 20) + ISHFT(IPEAK, 24)
                  IQ(PNT_FCHT+2) = ITIME + ISHFT(IAREA, 18)
C
                  PNT_FCHT = PNT_FCHT + 2
                  PNT_FXDA = PNT_FXDA + NWDS_FXDA  
                ENDDO
              ENDDO
C
  100         CONTINUE
            ENDDO                       
          ENDDO                         
        ENDDO
      ENDDO
C
C  Fill word count 
C
      IQ(PNT_FCHT+1) = 2*NHITS + HEADER_LENGTH + TRAILER_LENGTH
C
C  Release empty space in fcht bank (shouldn't be necc.)
C
      NWORDS = IQ(PNT_FCHT+1) - IQ(LFCHT - 1)
      IF ( NWORDS.GT.0  ) THEN
        CALL MZPUSH(IXCOM, LFCHT, 0, NWORDS, 'R')
      ENDIF
C
  999 RETURN
      END
