      FUNCTION FDC_TRACK_CHK(HALF,LADDER,QTRAK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check if a track really goes through the
C-   sectors of the segments that make it up for either the first or
C-   last wire of each segment.
C-
C-   Returned value  : .true. if good, .false. if at least
C-                      one sector is bad.
C-   Inputs  : HALF,LADDER,QTRAK
C-
C-   Created  16-MAY-1991   Robert E. Avery
C-   Updated  17-SEP-1991   Susan K. Blessing  Change size of QTRAK to 
C-     26 (two errors and two spares).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL FDC_TRACK_CHK
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C   Inputs:
      INTEGER HALF,LADDER(0:2)
      REAL QTRAK(26)
C
C   Local:      
      INTEGER LAYER 
      INTEGER LFTRH 
      INTEGER LSEGM
      INTEGER IADD,IER
      INTEGER H,UNIT,QUAD_SEGM,SECTOR_SEGM,WIRE,UBIT
      INTEGER QUAD,SECTOR
      INTEGER NHIT 
      INTEGER XSECT_HIT 
C
      REAL    Z0(0:1)
      REAL    XWIRE,YWIRE,ZWIRE
      REAL    XPOS,YPOS
C
      LOGICAL LAYER_OK 
      LOGICAL FIRST 
C
C   Functions:
      INTEGER GZFTRH
      INTEGER GZFSEG
      INTEGER LZFIND
      LOGICAL FDC_MISS_SEG_CHK
C
      SAVE FIRST,Z0
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        LFTRH = GZFTRH()
        Z0(0)= Q(LFTRH+3)
        Z0(1)= Q(LFTRH+4)
        FIRST = .FALSE.
      ENDIF
C
      FDC_TRACK_CHK= .TRUE.
      DO  LAYER =  0, 2
        IF ( LADDER(LAYER).GT.0 ) THEN
          LSEGM=GZFSEG(HALF,LAYER)
          LSEGM=LZFIND(IXCOM,LSEGM,LADDER(LAYER),-5)
          XSECT_HIT = IQ(LSEGM+1)/1000
          IADD = IQ(LSEGM+2)
          NHIT = IQ(LSEGM+3)
C
C  Check first wire:
C 
          CALL FCODER(IADD,H,UNIT,QUAD_SEGM,SECTOR_SEGM,WIRE,UBIT,1)
          CALL GTFALH( H,UNIT,QUAD_SEGM,SECTOR_SEGM,WIRE,
     &               XWIRE,YWIRE,ZWIRE)
          XPOS = QTRAK(4) + QTRAK(7)* (ZWIRE - Z0(HALF) ) 
          YPOS = QTRAK(5) + QTRAK(8)* (ZWIRE - Z0(HALF) )
C
          CALL FGET_SECTOR(XPOS,YPOS,HALF,LAYER,QUAD,SECTOR)
          LAYER_OK = (QUAD .EQ. QUAD_SEGM)
     &         .AND. (SECTOR .EQ. SECTOR_SEGM) 

C
C  Check last wire:
C 
          WIRE = IQ(LSEGM+3+NHIT)/2.
          IF ( ABS(XSECT_HIT) .GT. 0 ) THEN
            SECTOR_SEGM = SECTOR_SEGM + SIGN(1,XSECT_HIT)
          ENDIF
          CALL GTFALH( H,UNIT,QUAD_SEGM,SECTOR_SEGM,WIRE,
     &               XWIRE,YWIRE,ZWIRE)
          XPOS = QTRAK(4) + QTRAK(7)* (ZWIRE - Z0(HALF) ) 
          YPOS = QTRAK(5) + QTRAK(8)* (ZWIRE - Z0(HALF) )
C
          CALL FGET_SECTOR(XPOS,YPOS,HALF,LAYER,QUAD,SECTOR)
          LAYER_OK = LAYER_OK .OR.
     &                 ( (QUAD .EQ. QUAD_SEGM)
     &             .AND. (SECTOR .EQ. SECTOR_SEGM) )
C
        ELSE
C
C  Check if segment should have been found here
C
          LAYER_OK = FDC_MISS_SEG_CHK(HALF,QTRAK,LAYER)
C
        ENDIF
        FDC_TRACK_CHK = FDC_TRACK_CHK .AND. LAYER_OK 
      ENDDO
C----------------------------------------------------------------------
  999 RETURN
      END
