      SUBROUTINE FDC_DSTFIX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : On the DST level, loops through FDC tracks,
C-                         and correct/normalize dE/dX (MIPs) according
C-                         to the run 1B FDC Pulse Area changes, as  
C-                         functions of run numbers.
C-   Inputs  : None.
C-   Outputs : None, but Q(LFDCT+20) modified.
C-
C-   Created  19-JUL-1995   Yi-Cheng Liu, based on the schemes that 
C-                          Sue Blessing recommended as described below.
C-   Updated  12-SEP-1995   Yi-Cheng Liu, make the correction parameters 
C-                          RCP controlled.
C-   Updated  26-SEP-1995   Yi-Cheng Liu, added safety-check bit to
C-                          prevent doing FDC_DSTFIX more than once.
C-   Updated  19-DEC-1995   Brent May, Fix loop over IWIRE (24,32->31)
C-        
C***************** Correction/Normalization strategy ******************
C*
C*  1. From FDC_MON, find the time dependences of FDC pulse areas duriung
C*     Run 1B.  These are the averaged behavior sorted into :
C*
C*  NFDC :
C*        Theta : averaging over all 8 quadrants for :
C*                Cell 0     ( inner )
C*                Cell 1         .
C*                Cell 2         .
C*                Cell 3         .
C*                Cell 4         .
C*                Cell 5     ( outer )
C*
C*        Phi : averaging over all cells.
C*
C*  SFDC :
C*        the same way as above.
C*
C*  So, for each run number, there are 14 correction factors, 
C*  for the 14 "types". ( these are our "correction maps",
C*  or functions, so to speak. )
C*
C*  2. When working with DSTs, for every event:
C*
C*   (a) obtain the number of FDC tracks :
C*   (b) for each track, go through the 3 possible layers, and figure
C*       out which cells/sectors are on the track :
C+     Method :
C+       (1) get (X0, Y0, Z0), dX/dZ, dY/dZ from FDCT bank info. 
C+       (2) find the hit wires in the layer, and call GTFALH to 
C+           get ZWIRE. ( use "typical" QUAD, SECT numbers..)
C+       (3) find the XPOS, YPOS of the actuall hit on that wire plane
C+            by the following relationships :
C+              XPOS = X0 - (Z0-ZWIRE) * Q(LFDCT+7)
C+              YPOS = Y0 - (Z0-ZWIRE) * Q(LFDCT+8)
C+       (4) then, call FGET_SECTOR to get QUAD, SECT
C+       (5) use the obtained QUAD, SECT numbers, repeat (2)-(4) 
C+           again to get the most accurate values of XPOS, YPOS, 
C+           hence best QUAD, SECT.
C*   (c) if the sector/cell is hit, mark the cell type and increment
C*       the cell type counter.  
C*   Note : 1. segments which were used twice are skipped.
C*          2. cross-sector segments are taken care of by going though 
C*             all wires in a layer.
C*
C*   (d) for all cells used for the fitted track, the over-all correction
C*       factor for the track MIP should be a weighted mean of all the
C*       contributing cell types :
C*
C*            Sum[ Number_Type(i)_Cells * CorrectFactor_Type(i)_Cells ]
C*       M =  ---------------------------------------------------------
C*                       Sum[ Number_Type(i)_Cells ]
C*
C**********************************************************************
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER  TRACK,NTRACK
      INTEGER  STATUS, IER
      INTEGER  LFDCT ,GZFDCT
      INTEGER  LFTRH ,GZFTRH
      INTEGER  LADDER(0:2)
C
C 1st argument : 0->N, 1->S ; 2nd argument : 0-5 -> Theta, 6 -> Phi
C
      INTEGER  NUM_HIT_CELL_TYPE(0:1,0:6) 
      REAL  CORR_HIT_CELL_TYPE  ! a function to calculate the 
C                               ! correction factor for each type. 
      REAL  CORRECTION_FACTOR
C
      LOGICAL  FDC_DSTFIX_FLAG
      INTEGER  RUN_NUM
      INTEGER  HALF, ITYPE, TYPE_TOTAL
      INTEGER  IQTRAK(26) ! TRACK INFORMATION
      REAL  QTRAK(26),QHTRK(3,34)
      EQUIVALENCE  (QTRAK,IQTRAK)
C
      REAL SLOPE(0:1,0:6)
C
      INTEGER  IWIRE
      REAL  XWIRE, YWIRE, ZWIRE
      REAL  X0, Y0, Z0, XPOS, YPOS
C
C      INTEGER H,UNIT,QUAD,SECT,WIRE,UBIT
      INTEGER QUAD,SECT
C
      LOGICAL BTEST, FTRINI, FIRST
      logical XSECTOR, USED_TWICE
C
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C Initializing FASE stuff :
C     
      IF(FIRST) THEN
        IF (.NOT.FTRINI()) THEN
          PRINT *,' ***********  FTRINI failed !  *********** '
        ENDIF 
C Read correction parameters from FDC_DSTFIX_RCP 
C
        CALL INRCP('FDC_DSTFIX_RCP',IER)
        IF(IER .EQ. 0) THEN
          CALL EZPICK ('FDC_DSTFIX_RCP')
          CALL EZGET  ('SLOPE',SLOPE,IER)
          CALL EZRSET
        ELSE
          CALL ERRMSG('FDC_DSTFIX','FDC_DSTFIX',
     &                  'FDC_DSTFIX not found.','W')
        ENDIF
C
        FIRST = .FALSE.
      ENDIF
C
C ****  Look for FDC tracks  ****
C
      RUN_NUM =IQ(LHEAD+6)
      LFTRH = GZFTRH()
C
C Check bit 3 of status word to see if FDC_DSTFIX has been done before.
C Use bit 3 (4th bit) as the FDC_DSTFIX status bit..
C
      FDC_DSTFIX_FLAG = BTEST(IQ(LFTRH),3)
      IF (FDC_DSTFIX_FLAG) GOTO 999
C
      IF (LFTRH.LE.0) GOTO 999  ! No FDC track.
      NTRACK = IQ(LFTRH+2)
      DO TRACK = 1, NTRACK
C
        CALL VZERO(NUM_HIT_CELL_TYPE,14)
C
        LFDCT = GZFDCT(TRACK)
        CALL GTFDCT(TRACK,QTRAK,QHTRK,LADDER)
        STATUS = IQTRAK(1)
        HALF=IAND(1,STATUS)
C
C Get a reference point on the track. 
C
        X0 = QTRAK(4)
        Y0 = QTRAK(5)
        CALL FGETZ0(TRACK,Z0)  
C
C** LAYER 0 : inner Theta chamber :
C** Skip segment if used twice.
        USED_TWICE = BTEST(STATUS,8)
        IF (.NOT.USED_TWICE) THEN
          XSECTOR = BTEST(STATUS,11) 
          DO IWIRE = 0, 7 
            IF (BTEST(IQ(LFDCT+3),IWIRE)) THEN 
              CALL GTFALH(HALF,0,0,0,IWIRE,XWIRE,YWIRE,ZWIRE)
              XPOS = X0 - (Z0-ZWIRE) * Q(LFDCT+7)
              YPOS = Y0 - (Z0-ZWIRE) * Q(LFDCT+8)
              CALL FGET_SECTOR(XPOS,YPOS,HALF,0,QUAD,SECT)
C Do GTFALH again to guarantee more acurate X, Y positions. 
              CALL GTFALH(HALF,0,QUAD,SECT,IWIRE,XWIRE,YWIRE,ZWIRE)
              XPOS = X0 - (Z0-ZWIRE) * Q(LFDCT+7)
              YPOS = Y0 - (Z0-ZWIRE) * Q(LFDCT+8)
              CALL FGET_SECTOR(XPOS,YPOS,HALF,0,QUAD,SECT)
C Each hit wire used for track segments will count as 1. 
C
              NUM_HIT_CELL_TYPE(HALF,SECT) =
     &             NUM_HIT_CELL_TYPE(HALF,SECT) + 1
            ENDIF
          ENDDO
        ENDIF
C
C** LAYER 1 : outer Theta chamber :
C** Skip segment if used twice. 
        USED_TWICE = BTEST(STATUS,9)
        IF (.NOT.USED_TWICE) THEN
          XSECTOR = BTEST(STATUS,12)
          DO IWIRE = 24, 31
            IF (BTEST(IQ(LFDCT+3),IWIRE)) THEN
              CALL GTFALH(HALF,0,4,0,IWIRE-24,XWIRE,YWIRE,ZWIRE)
              XPOS = X0 - (Z0-ZWIRE) * Q(LFDCT+7)
              YPOS = Y0 - (Z0-ZWIRE) * Q(LFDCT+8)
              CALL FGET_SECTOR(XPOS,YPOS,HALF,1,QUAD,SECT)
C Do GTFALH again to guarantee more acurate X, Y positions.
              CALL GTFALH(HALF,0,QUAD,SECT,IWIRE-24,XWIRE,YWIRE,ZWIRE)
              XPOS = X0 - (Z0-ZWIRE) * Q(LFDCT+7)
              YPOS = Y0 - (Z0-ZWIRE) * Q(LFDCT+8)
              CALL FGET_SECTOR(XPOS,YPOS,HALF,0,QUAD,SECT)
C
              NUM_HIT_CELL_TYPE(HALF,SECT) =
     &             NUM_HIT_CELL_TYPE(HALF,SECT) + 1
            ENDIF
          ENDDO
        ENDIF
C
C** LAYER 2 : Phi chamber :
C** Skip segment if used twice.
        USED_TWICE = BTEST(STATUS,10)
        IF (.NOT.USED_TWICE) THEN
          XSECTOR = BTEST(STATUS,13)
          DO IWIRE = 8, 23
            IF (BTEST(IQ(LFDCT+3),IWIRE)) THEN
              CALL GTFALH(HALF,1,0,0,IWIRE-8,XWIRE,YWIRE,ZWIRE)
              XPOS = X0 - (Z0-ZWIRE) * Q(LFDCT+7)
              YPOS = Y0 - (Z0-ZWIRE) * Q(LFDCT+8)
              CALL FGET_SECTOR(XPOS,YPOS,HALF,2,QUAD,SECT)
C Do GTFALH again to guarantee more acurate X, Y positions.
              CALL GTFALH(HALF,0,QUAD,SECT,IWIRE-8,XWIRE,YWIRE,ZWIRE)
              XPOS = X0 - (Z0-ZWIRE) * Q(LFDCT+7)
              YPOS = Y0 - (Z0-ZWIRE) * Q(LFDCT+8)
              CALL FGET_SECTOR(XPOS,YPOS,HALF,0,QUAD,SECT)
C
              NUM_HIT_CELL_TYPE(HALF,6) =
     &             NUM_HIT_CELL_TYPE(HALF,6) + 1
            ENDIF
          ENDDO
        ENDIF 
C

C** ! End of layer checks.
C
        CORRECTION_FACTOR = 0.0
        TYPE_TOTAL = 0
        DO ITYPE = 0, 6
          TYPE_TOTAL = TYPE_TOTAL + NUM_HIT_CELL_TYPE(HALF,ITYPE)
          CORRECTION_FACTOR = CORRECTION_FACTOR + 
     &            NUM_HIT_CELL_TYPE(HALF,ITYPE)*
     &            CORR_HIT_CELL_TYPE(RUN_NUM,HALF,ITYPE,SLOPE)
C
        ENDDO  ! End of loop over types.
C
        IF (TYPE_TOTAL.EQ.0) GOTO 999
        CORRECTION_FACTOR = CORRECTION_FACTOR / FLOAT(TYPE_TOTAL)
C
C Now, actually apply the correction factor
C
      Q(LFDCT+20) = Q(LFDCT+20) * CORRECTION_FACTOR  
C
      ENDDO  ! End of loop over tracks.
C          
C Set status bit to signal that FDC_DSTFIX has been done once !
C
      IQ(LFTRH) = IBSET(IQ(LFTRH),3)
C
  999 RETURN
      END
