      SUBROUTINE PFPTRAK(ITRACK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Use tracking by planes in 3-D to supplement
C-                         current FTRAKS
C-
C-   Inputs  : ITRACK = FDC Track number to be used
C-
C-   Created   7-JUN-1990   Jeffrey Bantly
C-   Updated  23-JAN-1991   Jeffrey Bantly  add bank check, redo Z0 readin 
C-   Updated  30-APR-1991   Jeffrey Bantly  cleanup using new Compack 
C-   Updated  17-JUN-1991   Susan K. Blessing  Change size of SCONT array.
C-   Updated  17-SEP-1991   Susan K. Blessing  Change size of (I)QTRAK to 
C-     26 (two errors and two spares).
C-   Updated  28-FEB-1992   Robert E. Avery  Change RCP parameter name.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER ICONT(10),ITRA,NTRACK,ITRACK,I
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE,UB,MMODULE,MLAYER,IMOD
      INTEGER FH,FU,FQ,FS,FW,FM,FL,SH,SU,SQ,SS,SW,SM,SL
      INTEGER IHSEC,IER,NHIT,LSEG,GZFSEG,NSEG,NZBANK,ISEG
      INTEGER ISATRA(34),IADD(34),IHIT(34),IADDS,NHITS
      INTEGER IQTRAK(26),ILYR,LADDER(0:2),IVIEW
      INTEGER BOVRLP(50,4),SOVRLP(50,4),LOCMATCH(50,4),IMATCH,NMATCH
      INTEGER NFSECT,NSSECT,NTEST,TESTSG(50),SKPIN,ICALL,DRALIN,DRAPLN
      INTEGER LKFTRH,GZFTRH
C
      REAL QTRAK(26),CONT(26),QHSEC(3,34),QHIT(18),RESID(34),SGCONT(62)
      REAL FIADDS,FNHITS,Z0(2),RTRK,PLNCUT
      REAL FX(3),FY(3),FZ(3)
      REAL SX(3),SY(3),SZ(3)
      REAL  X(3), Y(3), Z(3)
      REAL FA,FB,FC,FD,SA,SB,SC,SD,A(50),B(50),C(50),D(50)
      REAL X11,Y11,Z11,X21,Y21,Z21
      REAL X12,Y12,Z12,X22,Y22,Z22
      REAL X1S,Y1S,Z1S,X2S,Y2S,Z2S
      REAL X1FS,Y1FS,Z1FS,X2FS,Y2FS,Z2FS
      EQUIVALENCE(QTRAK,IQTRAK)
      EQUIVALENCE(IADDS,FIADDS)
      EQUIVALENCE(NHITS,FNHITS)
C
      LOGICAL MATCH,FOVCHK,SOVCHK,SGONLY,OK
C
      CHARACTER*4 L3DCLR
C
      SAVE ICALL, Z0, SGONLY
      DATA ICALL / 0 /
C----------------------------------------------------------------------
C
C ****  Initializations.
C
      IF(ICALL.EQ.0) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('SGONLY',SGONLY,IER)
        CALL EZGET('PLNCUT',PLNCUT,IER)
        CALL EZRSET
        ICALL=1
      ENDIF
      LKFTRH=GZFTRH()
      IF(LKFTRH.GT.0) THEN
        Z0(1)=Q(LKFTRH+3)
        Z0(2)=Q(LKFTRH+4)
      ELSE
        CALL OUTMSG(' No FDC track banks present')
        GOTO 999
      ENDIF
      CALL PUGETV('FDC 3D VIEW',IVIEW)
      CALL PUGETV('FDC DRAW 3DPLNS',DRAPLN)
      CALL PUGETV('FDC DRAW 3DLINS',DRALIN)
      CALL PUGETA('FDC COLR TRACK',L3DCLR)
C
C ****  Check input track number and number of tracks available.
C
      CALL GTFTRH(ICONT)
      NTRACK=ICONT(2)
      IF (NTRACK.LE.0) GO TO 999
      IF (ITRACK.GT.NTRACK) GO TO 999
C
C ****  Fetch track's ladder of segments.
C
      CALL GTFDCT(ITRACK,CONT,QHSEC,LADDER)
      CALL UCOPY(CONT,QTRAK,26)
      NHIT=IQTRAK(2)
      CALL FGETLDR2(ITRACK,LADDER)
      IF (NHIT.EQ.0) THEN
        GO TO 100   ! no hits on track!!!!
      ENDIF
C
C ****  Full 3-segment track so just draw the track with planes but
C ****  no matching.
C
      CALL FCODER((IADD(1)/2),FH,FU,FQ,FS,FW,UB,1)
      DO 10 ILYR=0,2
        FM=FH*3+ILYR
        IF(LADDER(ILYR).EQ.0) GOTO 10
        CALL FSGXYZ(FM,LADDER(ILYR),X,Y,Z,OK)
        IF(.NOT. OK) GOTO 100           ! Segment is missing
        CALL FPLANE(3,X,Y,Z,A(ILYR+1),B(ILYR+1),C(ILYR+1),D(ILYR+1),IER)
        IF(IER.NE.0) GOTO 100           ! Bad points along segment
        IF(DRAPLN.GE.1 .AND. IVIEW.NE.2)        ! Draw plane through hits.
     &           CALL PFPLAN(FH,A(ILYR+1),B(ILYR+1),C(ILYR+1),D(ILYR+1))
   10 CONTINUE
C
      IF(DRALIN.GE.1) THEN              ! Draw intersection lines for
C                                       ! each pair of planes.
        CALL PFILIN(FH,A(3),B(3),C(3),D(3),A(1),B(1),C(1),D(1),
     &                 X1FS,Y1FS,Z1FS,X2FS,Y2FS,Z2FS)
        CALL PFILIN(FH,A(2),B(2),C(2),D(2),A(1),B(1),C(1),D(1),
     &                 X11,Y11,Z11,X21,Y21,Z21)
        CALL PFILIN(FH,A(3),B(3),C(3),D(3),A(2),B(2),C(2),D(2),
     &                 X12,Y12,Z12,X22,Y22,Z22)
      ENDIF
C
      GOTO 100
C
C ****  Track has only two segments so try and find candidates for third.
C
  500 CONTINUE
C
C ****  Copy track information pertaining to hits on track.
C
      DO 200 IHSEC=1,IQTRAK(2)
        CALL UCOPY(QHSEC(1,IHSEC),IADD(IHSEC),1)
        CALL UCOPY(QHSEC(2,IHSEC),IHIT(IHSEC),1)
        CALL UCOPY(QHSEC(3,IHSEC),RESID(IHSEC),1)
  200 CONTINUE
C
C ****  For two segment track use first and last hit to determine
C ****  two sectors involved.
C
      CALL FCODER((IADD(1)/2),FH,FU,FQ,FS,FW,UB,1)
      FL=FU*2+FQ/4
      FM=FH*3+FL
      CALL FCODER((IADD(IQTRAK(2))/2),SH,SU,SQ,SS,SW,UB,1)
      SL=SU*2+SQ/4
      SM=SH*3+SL
      IF(FL.GE.SL .OR. FM.GE.SM) THEN ! FL<SL,FM<SM expected
        GOTO 100
      ENDIF
C
C ****  Find plane for first known segment
C
      CALL FTRXYZ(IADD(1),IHIT(1),IADD(6),IHIT(6),FX,FY,FZ,OK)
      IF(.NOT.OK) THEN                ! FTRXYZ returns false, no points
        GOTO 100
      ENDIF
      IMOD=FH*3+FU*2+FQ/4
      IF(SGONLY) THEN
        CALL FSGXYZ(IMOD,1,FX,FY,FZ,OK)
        IF(.NOT.OK) THEN              ! FSGXYZ returns false, no points
          GOTO 100
        ENDIF
      ENDIF
      CALL FPLANE(3,FX,FY,FZ,FA,FB,FC,FD,IER)
      IF(IER.NE.0) THEN               ! FPLANE returns false, bad points
        GOTO 100
      ENDIF
C
C ****  Find plane for second known segment
C
      CALL FCODER((IADD(NHIT)/2),SH,SU,SQ,SS,SW,UB,1)
      SKPIN=5
      IF(SU.EQ.1) SKPIN=13
      CALL FTRXYZ(IADD(NHIT),IHIT(NHIT),IADD(NHIT-SKPIN),
     &              IHIT(NHIT-SKPIN),SX,SY,SZ,OK)
      IF(.NOT.OK) THEN                ! FTRXYZ returns false, no points
        GOTO 100
      ENDIF
      IMOD=SH*3+SU*2+SQ/4
      IF(SGONLY) THEN
        CALL FSGXYZ(IMOD,1,FX,FY,FZ,OK)
        IF(.NOT.OK) THEN              ! FSGXYZ returns false, no points
          GOTO 100
        ENDIF
      ENDIF
      CALL FPLANE(3,SX,SY,SZ,SA,SB,SC,SD,IER)
      IF(IER.NE.0) THEN               ! FPLANE returns false, bad points
        GOTO 100
      ENDIF
C
C ****  Draw the two known planes on display
C
      IF(DRAPLN.GE.1) CALL PFPLAN(FH,FA,FB,FC,FD)
      IF(DRAPLN.GE.1) CALL PFPLAN(SH,SA,SB,SC,SD)
C
C ****  Check on which sectors overlap the first and second segments.
C
      CALL FOVRLP(FH,FU,FQ,FS,FH,0,0,0,3,NFSECT,BOVRLP,FOVCHK)
      IF(.NOT.FOVCHK) THEN            ! no first track layer overlaps
        GOTO 100
      ENDIF
      IF(NFSECT.GT.50) NFSECT=50
      CALL FOVRLP(SH,SU,SQ,SS,SH,0,0,0,3,NSSECT,SOVRLP,SOVCHK)
      IF(.NOT.SOVCHK) THEN            ! no second track layer overlaps
        GOTO 100
      ENDIF
      IF(NSSECT.GT.50) NSSECT=50
C
C ****  Compare overlap findings for common sectors.
C
      CALL FOCMPR(NFSECT,BOVRLP,NSSECT,SOVRLP,NMATCH,LOCMATCH,MATCH)
      IF(.NOT.MATCH) THEN             ! no matching overlap sectors
        GOTO 100
      ENDIF
      IF(NMATCH.GT.50) NMATCH=50
C
C ****  Find all segments in matching overlapping sectors.
C
      MLAYER=LOCMATCH(1,2)*2 + LOCMATCH(1,3)/4
      MMODULE=LOCMATCH(1,1)*3+MLAYER
      LSEG=GZFSEG(LOCMATCH(1,1),MLAYER)
      IF(LSEG.LE.0) THEN              ! no segments to match in layer
        GOTO 100
      ENDIF
      NSEG=NZBANK(0,LSEG)
      NTEST = 0
      DO 20 ISEG=1,NSEG
        CALL GTFSEG(MMODULE,ISEG,SGCONT)
        FIADDS=SGCONT(2)
        FNHITS=SGCONT(3)
        CALL FCODER(IADDS,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
        DO 30 IMATCH=1,NMATCH
          IF(LOCMATCH(IMATCH,1).EQ.HALF) THEN
            IF(LOCMATCH(IMATCH,2).EQ.UNIT) THEN
              IF(LOCMATCH(IMATCH,3).EQ.QUAD) THEN
                IF(LOCMATCH(IMATCH,4).EQ.SECTOR) THEN
                  MATCH=.TRUE.
                  NTEST=NTEST+1
                  TESTSG(NTEST)=ISEG
                  IF(NTEST.GE.50) THEN
                    NTEST=50
                    GOTO 20
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
   30   CONTINUE
   20 CONTINUE
C
C ****  Find plane for missing sectors then draw each one.
C
      DO 50 ISEG=1,NTEST
        CALL FSGXYZ(MMODULE,TESTSG(ISEG),X,Y,Z,OK)
        IF(.NOT.OK) THEN              ! FSGXYZ returns false, no points
          GOTO 50
        ENDIF
        CALL FPLANE(3,X,Y,Z,A(ISEG),B(ISEG),C(ISEG),D(ISEG),IER)
        IF(IER.NE.0) THEN             ! FPLANE returns false, bad points
          GOTO 50
        ENDIF
C
C ****  Draw the three intersection lines between the planes and
C ****  average the three sets of beginning and ending points
C
        X1S=0.
        X2S=0.
        Y1S=0.
        Y2S=0.
        Z1S=0.
        Z2S=0.
        IF(DRALIN.GE.1) CALL FDILIN(FH,FA,FB,FC,FD,SA,SB,SC,SD,
     &                           X1FS,Y1FS,Z1FS,X2FS,Y2FS,Z2FS)
        X1S=X1S+X1FS
        X2S=X2S+X2FS
        Y1S=Y1S+Y1FS
        Y2S=Y2S+Y2FS
        Z1S=Z1S+Z1FS
        Z2S=Z2S+Z2FS
        IF(DRALIN.GE.1) CALL FDILIN(FH,FA,FB,FC,FD,A(ISEG),B(ISEG),
     &           C(ISEG),D(ISEG),X11,Y11,Z11,X21,Y21,Z21)
        IF(ABS(X1FS-X11).GT.PLNCUT) GOTO 50     ! Cut on distance
        IF(ABS(Y1FS-Y11).GT.PLNCUT) GOTO 50     ! between each pair
        IF(ABS(X2FS-X21).GT.PLNCUT) GOTO 50     ! of intersection lines.
        IF(ABS(Y2FS-Y21).GT.PLNCUT) GOTO 50
        X1S=X1S+X11
        X2S=X2S+X21
        Y1S=Y1S+Y11
        Y2S=Y2S+Y21
        Z1S=Z1S+Z11
        Z2S=Z2S+Z21
        IF(DRALIN.GE.1) CALL FDILIN(SH,A(ISEG),B(ISEG),
     &           C(ISEG),D(ISEG),SA,SB,SC,SD,X12,Y12,Z12,X22,Y22,Z22)
        IF(ABS(X1FS-X12).GT.PLNCUT) GOTO 50
        IF(ABS(Y1FS-Y12).GT.PLNCUT) GOTO 50
        IF(ABS(X2FS-X22).GT.PLNCUT) GOTO 50
        IF(ABS(Y2FS-Y22).GT.PLNCUT) GOTO 50
        X1S=X1S+X12
        X2S=X2S+X22
        Y1S=Y1S+Y12
        Y2S=Y2S+Y22
        Z1S=Z1S+Z12
        Z2S=Z2S+Z22
C
        CALL PXCOLR(L3DCLR)
        CALL J3MOVE(X1FS,Y1FS,Z1FS)
        CALL J3DRAW(X2FS,Y2FS,Z2FS)
        CALL J3MOVE(X11,Y11,Z11)
        CALL J3DRAW(X21,Y21,Z21)
        CALL J3MOVE(X12,Y12,Z12)
        CALL J3DRAW(X22,Y22,Z22)
        RTRK=SQRT( ((FX(1)+SX(1))/2.)**2. + ((FX(2)+SX(2))/2.)**2. )
        CALL PF3DSGDR(MMODULE,TESTSG(ISEG),RTRK)
        IF(DRAPLN.GE.1) CALL PFPLAN(MMODULE/3,A(ISEG),B(ISEG),
     &                                              C(ISEG),D(ISEG))
C
   50 CONTINUE                          ! End of loop over sectors
C
  100 CONTINUE                          ! End of loop over tracks
C------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
