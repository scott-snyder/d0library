      SUBROUTINE PF_3_SECTORS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Action Routine to draw a multiple window 
C-                         display of an FDC R-Z view and a single track's
C-                         sector plots.
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  13-MAY-1991   Robert E. Avery 
C-                      based on PFRZAD by Jeffrey Bantly
C-   Updated  17-JUN-1991   Susan K. Blessing  Change size of CONT array.
C-   Updated   9-SEP-1991   Robert E. Avery  Correction in use of 
C-                              status word of FDCT track (bit 0 = half). 
C-   Updated  10-SEP-1991   Robert E. Avery   Draw sectors even if 
C-                              ladder is lost.
C-   Updated  17-SEP-1991   Susan K. Blessing  Change size of (I)QTRAK to 
C-     26 (two errors and two spares).
C-   Updated  28-FEB-1992   Robert E. Avery  Draw sector if the track
C-     passes through at either first or last wire of cell (for phi only).
C-   Updated  27-AUG-1992   Robert E. Avery  Same as above comment for Thetas. 
C-   Updated   7-MAY-1993   Robert E. Avery  Draw boundary plot, if more 
C-                                              appropriate. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LFTRH,GZFTRH
      INTEGER ICALL,JCALL,IFL
      INTEGER TRKNUM,LADDER(0:2)
      INTEGER HALF,UNIT
      INTEGER QUAD_F, SECTOR_F, SECTOR
      INTEGER QUAD_L, SECTOR_L
      INTEGER LAYER,QD
      INTEGER WMAX
C
      REAL    CONT(62)
      REAL    XWIRE,YWIRE,ZWIRE
      REAL    XTRK,YTRK 
      REAL    Z0(0:1)
C
      REAL QTRAK(26),QHTRK(3,34)        
      INTEGER IQTRAK(26)
      EQUIVALENCE (IQTRAK,QTRAK)
C
      SAVE ICALL,JCALL,IFL,Z0
      DATA ICALL,JCALL,IFL/0,3,2/
C----------------------------------------------------------------------
C
      IF(JCALL.EQ.3) THEN
        JCALL=1
      ELSEIF(JCALL.EQ.1) THEN
        JCALL=2
      ELSEIF(JCALL.EQ.2) THEN
        JCALL=3
      ENDIF
C
      LFTRH=GZFTRH()
      IF(LFTRH.GT.0) THEN
        Z0(0)=Q(LFTRH+3)
        Z0(1)=Q(LFTRH+4)
      ELSE
        IF(JCALL.EQ.1) CALL INTMSG(' No FDC track bank present.')
        GOTO 999
      ENDIF
C
C  List all of the FDC tracks shown and choose track desired.
C
      IF(JCALL.EQ.1) CALL PFPICK_TRACK(TRKNUM,HALF,IFL)
      IF(TRKNUM.LE.0) THEN
        GOTO 999
      ENDIF
C
C  Fetch segment ladder and obtain logical channel address.
C
      IF(JCALL.EQ.1) CALL GTFDCT(TRKNUM,QTRAK,QHTRK,LADDER)
C        
      LAYER = JCALL-1
      IF ( LAYER .EQ. 2 ) THEN
        UNIT=1
        QD = 0
        WMAX = 15
      ELSE
        UNIT = 0
        QD = 0
        IF ( LAYER .EQ. 1 ) QD = 4
        WMAX = 7
      ENDIF
C
C First wire:
C
      CALL GTFALH( HALF,UNIT,QD,0,0,
     &               XWIRE,YWIRE,ZWIRE)
      XTRK = QTRAK(4) + QTRAK(7)* (ZWIRE - Z0(HALF) ) 
      YTRK = QTRAK(5) + QTRAK(8)* (ZWIRE - Z0(HALF) )
      CALL FGET_SECTOR(XTRK,YTRK,HALF,LAYER,QUAD_F,SECTOR_F)
C
C Last wire:
C
      CALL GTFALH( HALF,UNIT,QD,0,WMAX,
     &               XWIRE,YWIRE,ZWIRE)
      XTRK = QTRAK(4) + QTRAK(7)* (ZWIRE - Z0(HALF) ) 
      YTRK = QTRAK(5) + QTRAK(8)* (ZWIRE - Z0(HALF) )
      CALL FGET_SECTOR(XTRK,YTRK,HALF,LAYER,QUAD_L,SECTOR_L)
C
C Draw appropriate cell (or cells):
C
      IF ( SECTOR_F.NE.SECTOR_L ) THEN
        IF ( QUAD_F.EQ.QUAD_L ) THEN
          SECTOR = MIN(SECTOR_F,SECTOR_L)
          IF (UNIT.EQ.0) THEN
            CALL PF_THETA_BOUND_VIEW(HALF,QUAD_L,SECTOR)
          ELSEIF (UNIT.EQ.1) THEN
            CALL PF_PHI_BOUND_VIEW(HALF,SECTOR)
          ENDIF
        ELSE
          IF ( SECTOR_F.EQ.-1 ) THEN
            SECTOR_F = SECTOR_L
            QUAD_F = QUAD_L
          ELSE
            SECTOR_L = SECTOR_F
          ENDIF
        ENDIF
      ENDIF
C
      IF ( SECTOR_F.EQ.SECTOR_L ) THEN
        IF ( SECTOR_F.NE.-1 ) THEN
          IF(UNIT.EQ.0) THEN
            CALL PF_THETA_SECT_VIEW(HALF,QUAD_F,SECTOR_F)
          ELSEIF(UNIT.EQ.1) THEN
            CALL PF_PHI_SECT_VIEW(HALF,SECTOR_F)
          ENDIF
        ELSE
          CALL PF_PR_MISS(LAYER,XTRK,YTRK)
        ENDIF
      ENDIF
C
C  Done.
C
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
