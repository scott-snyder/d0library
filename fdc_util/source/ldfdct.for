      SUBROUTINE LDFDCT(QTRAK,QHSEC,HALF,LADDER)
C-----------------------------------------------------------------------
C
C    Purpose and Methods : Load a FDC track by 
C                          1) Increment the number of FDC tracks in FTRH 
C                          2) Store FDC track in Zebra bank FDCT 
C                          3) Store the associated hits and fit residuals 
C                             in a FDTH Zebra bank 
C
C  Input:  QTRAK(1:26)   contains information on the fitted track
C                        candidate
C           QTRAK(1)     status word (currently = FDC Half of track)
C           QTRAK(2)     number of wire hits on track
C           QTRAK(3)     bit pattern for used sense wires (0:31) in x-y 
C           QTRAK(4)     XG  x of center of gravity of x-y projection
C           QTRAK(5)     YG  y of center of gravity of x-y projection
C           QTRAK(6)     phi =arctan(dy/dx)
C           QTRAK(7)     dx/dz     
C           QTRAK(8)     dy/dz
C           QTRAK(9)     COV(1,1) 
C           QTRAK(10)    COV(1,2) 
C           QTRAK(11)    COV(1,3) 
C           QTRAK(12)    COV(1,4) 
C           QTRAK(13)    COV(2,2) 
C           QTRAK(14)    COV(2,3) 
C           QTRAK(15)    COV(2,4) 
C           QTRAK(16)    COV(3,3) 
C           QTRAK(17)    COV(3,4) 
C           QTRAK(18)    COV(4,4) 
C           QTRAK(19)    CHISQ
C           QTRAK(20)    ionization 
C           QTRAK(21)    error of ionization
C           QTRAK(22)    theta angle = arctan(dr/dz),(dr^2=dx^2 + dy^2)
C           QTRAK(23)    error on phi
C           QTRAK(24)    error on theta
C           QTRAK(25)    spare
C           QTRAK(26)    spare
C
C          QHSEC(1:3,IHSEC)  contains information on wire hits on track
C          QHSEC(1,IHSEC)    address
C          QHSEC(2,IHSEC)    hit# at this address
C          QHSEC(3,IHSEC)    fit residual in x-y view
C
C          HALF
C          LADDER(0:2)   list of segments used in track
C
C-   Created  xx-DEC-1988   Daria Zieminska 
C-   Updated  28-FEB-1990   Jeffrey Bantly  clean up & make path dep.
C-   Updated   7-MAY-1990   Jeffrey Bantly  add theta angle 
C-   Updated   8-NOV-1990   Jeffrey Bantly  add segment ladder to FDTH
C-   Updated  24-JAN-1991   Jeffrey Bantly  remove bank booking to utilities
C-                                          fill words 5 & 6 in FTRH
C-   Updated   9-MAY-1991   Susan K. Blessing  Add reference links to 
C-    segments used to build track.
C-   Updated  10-JUN-1991   Susan K. Blessing  Incrementing number of tracks
C-    in FTRH bank is done in BKFDCT, shouldn't do again here.
C-   Updated  17-SEP-1991   Susan K. Blessing  Change size of (I)QTRAK
C-   Updated  20-SEP-1991   Susan K. Blessing  Use TRKNUM in the call to 
C-    BKFDTH rather than the link of the supporting FDCT bank since that 
C-    can be corrupted after the MZFORM call.
C-   Updated  19-NOV-1991   Susan K. Blessing  Fill status bit 3 if a segment
C-    is on two tracks and store link to second track.
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER LAYER,HALF
      INTEGER LKFTRH,LKFDCT,LKFDTH,LOC
      INTEGER LSEG,GZFSEG
      INTEGER LZFIND
      INTEGER NHSEC,LADDER(0:2),ILADDER(0:2)
      INTEGER TRKNUM
      INTEGER GZFTRH
      INTEGER STAT
      INTEGER USED,USED2
C
      REAL QTRAK(26),QHSEC(3,34),FHSEC,FLADDER(0:2)
C
      EQUIVALENCE (NHSEC,FHSEC)
      EQUIVALENCE (ILADDER,FLADDER)
C------------------------------------------------------------------------
C
      LKFTRH=GZFTRH()
      IF (LKFTRH.LE.0) CALL BKFTRH(LKFTRH) 
C
C  Book FDCT bank 
C
      CALL BKFDCT(LKFDCT)
      IF(LKFDCT.LE.5) GOTO 999
C
C  Update FTRH bank
C
      LKFTRH=GZFTRH()
      IF (LKFTRH.LE.0) GOTO 999
      IF(HALF.EQ.0) THEN
        IQ(LKFTRH+5)=IQ(LKFTRH+5)+1     ! increment number of tracks in
C                                       ! FDC Half 0
      ELSE 
        IQ(LKFTRH+6)=IQ(LKFTRH+6)+1     ! increment number of tracks in
C                                       ! FDC Half 1
      ENDIF
C
C  Store track info in FDCT bank
C
      TRKNUM=IQ(LKFTRH+2)
      IQ(LKFDCT-5)=TRKNUM
      CALL UCOPY(QTRAK,Q(LKFDCT+1),26)
      FHSEC=QTRAK(2)
C
C  Book FDTH bank and store associated hits and segment ladder in FDTH bank
C
      CALL BKFDTH(TRKNUM,LKFDTH)
      IF(LKFDTH.LE.5) GOTO 999
      CALL UCOPY(QHSEC,Q(LKFDTH+1),3*NHSEC)
      ILADDER(0)=LADDER(0)
      ILADDER(1)=LADDER(1)
      ILADDER(2)=LADDER(2)
      CALL UCOPY(FLADDER,Q(LKFDTH+103),3)        ! Put in ladder
C
C Find links to segments and store in FDCT.
C Store link of FDCT bank in segment banks.
C Set status word in FSGx banks to show segment has been used for a track.
      DO LAYER = 0, 2
        IF (LADDER(LAYER).NE.0) THEN
          LSEG = GZFSEG(HALF,LAYER)
          IF (LSEG.GT.0) THEN
            LOC = LZFIND(IXCOM,LSEG,LADDER(LAYER),-5)
            LQ(LKFDTH-LAYER-1) = LOC
            STAT = IQ(LOC)
C
C Has segment been used before?
            USED = IBITS (STAT,2,1)
            IF (USED.EQ.0) THEN
              IQ(LOC) = IBSET(STAT,IUSED)
              LQ(LOC-1) = LKFDCT
            ELSE
              USED2 = IBITS(STAT,3,1)
              IF (USED2.EQ.0) THEN
                IQ(LOC) = IBSET(STAT,3)
                LQ(LOC-2) = LKFDCT
              ELSE
                CALL ERRMSG('FDC-segment-used-three-times','LDFDCT',
     &            ' Single segment used in more than two tracks','W')
              END IF
            END IF
C
          ELSE
            LQ(LKFDTH-LAYER-1) = 0
          END IF
        ELSE
          LQ(LKFDTH-LAYER-1) = 0
        END IF
      END DO
C
C------------------------------------------------------------------------
  999 RETURN
      END
