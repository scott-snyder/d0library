      SUBROUTINE PF_ROAD_LAYERS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw the FDC sectors associated with a given
C-              tracking road.
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created   1-OCT-1992   Robert E. Avery
C-   Updated  11-MAR-1993   Robert E. Avery  Now based on PPHO and PELC banks,
C-                                              rather than on roads.
C-   Updated   2-APR-1993   Robert E. Avery  Draw boundary plot, if more
C-                                              appropriate. 
C-   Updated  16-AUG-1993   Robert E. Avery  Remove use of LUSER bank. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:FLOCAL.INC/LIST'
C
      INTEGER ICALL,JCALL
      INTEGER HALF,UNIT
      INTEGER QUAD_F, SECTOR_F, SECTOR
      INTEGER QUAD_L, SECTOR_L
      INTEGER LAYER,QD
      INTEGER WMAX
      INTEGER ROADNUM
C
      REAL    XWIRE,YWIRE,ZWIRE
      REAL    XTRK,YTRK 
      REAL    ZV,DX,DY
C
      SAVE ICALL,JCALL
      DATA ICALL,JCALL/0,3/
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
C  List all of the CD roads choose road desired.
C
      IF (JCALL.EQ.1) CALL PFPICK_ROAD(HALF,ZV,DX,DY)
      IF (HALF.LT.0) THEN
        GOTO 999
      ENDIF
C
C Store road in temporary area:
C
      IF (JCALL.EQ.1) THEN
        CALL MZLINT(IXCOM,'/FLOCAL/',FLOCAL,LFLOC,FLOCAL)
        CALL MZBOOK(IXMAIN,LFLOC,0,2,'ROAD',1,1,5,3,0)
        Q(LFLOC+1) = HALF
        Q(LFLOC+2) = ZV
        Q(LFLOC+3) = DX
        Q(LFLOC+4) = DY
      ENDIF
C
C  Fetch segment ladder and obtain logical channel address.
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
      XTRK = DX * (ZWIRE-ZV)
      YTRK = DY * (ZWIRE-ZV)
      CALL FGET_SECTOR(XTRK,YTRK,HALF,LAYER,QUAD_F,SECTOR_F)
C
C Last wire:
C
      CALL GTFALH( HALF,UNIT,QD,0,WMAX,
     &               XWIRE,YWIRE,ZWIRE)
      XTRK = DX * (ZWIRE-ZV)
      YTRK = DY * (ZWIRE-ZV)
      CALL FGET_SECTOR(XTRK,YTRK,HALF,LAYER,QUAD_L,SECTOR_L)
C
C Draw appropriate cell:
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
C Drop user bank when done:
C
      IF(JCALL.EQ.3 ) THEN
        CALL MZDROP(IXCOM,LFLOC,'L')
        FLOCAL(1) = 0
        LFLOC = 0
      ENDIF
C
C  Done.
C
C----------------------------------------------------------------------
  999 RETURN
      END
