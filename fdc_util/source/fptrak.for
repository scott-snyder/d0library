      SUBROUTINE FPTRAK(ITRACK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Use tracking by planes in 3-D to supplement
C-                         current FTRAKS
C-
C-   Inputs  : ITRACK = FDC Track number to be used
C-
C-   Created   7-JUN-1990   Jeffrey Bantly
C-   Updated  17-JUN-1991   Susan K. Blessing  Change size of SCONT array.
C-   Updated  17-SEP-1991   Susan K. Blessing  Change size of (I)QTRAK
C-    to accomodate theta and phi errors and two spare words.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER ICONT(10),ITRA,NTRACK,ITRACK,I 
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE,UB,MMODULE,MLAYER,IMOD
      INTEGER FH,FU,FQ,FS,FW,FM,FL,SH,SU,SQ,SS,SW,SM,SL
      INTEGER IHSEC,IER,NHIT,LSEG,NSEG,ISEG
      INTEGER ISATRA(34),IADD(34),IHIT(34),IADDS,NHITS
      INTEGER IQTRAK(26),LADDER(0:2)
      INTEGER BOVRLP(50,4),SOVRLP(50,4),LOCMATCH(50,4),IMATCH,NMATCH
      INTEGER NFSECT,NSSECT,NTEST,TESTSG(50),SKPIN,ICALL
      INTEGER GZFSEG,NZBANK
      REAL QTRAK(26),CONT(26),QHSEC(3,34),QHIT(18),RESID(34),SGCONT(62)
      REAL FIADDS,FNHITS,Z0(2),RTRK
      REAL FX(3),FY(3),FZ(3)
      REAL SX(3),SY(3),SZ(3)
      REAL  X(3), Y(3), Z(3)
      REAL FA,FB,FC,FD,SA,SB,SC,SD,A,B,C,D
      REAL X1,Y1,Z1,X2,Y2,Z2
      REAL X1S,Y1S,Z1S,X2S,Y2S,Z2S
      LOGICAL MATCH,FOVCHK,SOVCHK,SGONLY,OK
      EQUIVALENCE(QTRAK,IQTRAK)
      EQUIVALENCE(IADDS,FIADDS)
      EQUIVALENCE(NHITS,FNHITS)
      SAVE ICALL, Z0, SGONLY
      DATA ICALL / 0 /
C----------------------------------------------------------------------
      IF(ICALL.EQ.0) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('Z0',Z0,IER)
        CALL EZGET('SGONLY',SGONLY,IER)
        CALL EZRSET
        ICALL=1
      ENDIF
      CALL GTFTRH(ICONT)
      NTRACK=ICONT(2)
      IF (NTRACK.LE.0) GO TO 999
      IF (ITRACK.GT.NTRACK) GO TO 999
C
C ****  Loop over the number of tracks in FDC
C
C      DO 100 ITRACK=1,NTRACK
        CALL GTFDCT(ITRACK,CONT,QHSEC,LADDER)
        CALL UCOPY(CONT,QTRAK,26)
        NHIT=IQTRAK(2)
        IF (NHIT.EQ.0) THEN
          GO TO 100   ! no hits on track!!!!
        ENDIF
        IF (NHIT.GE.26) GO TO 100  ! full 3-segment track
        DO 200 IHSEC=1,IQTRAK(2)
          CALL UCOPY(QHSEC(1,IHSEC),IADD(IHSEC),1)
          CALL UCOPY(QHSEC(2,IHSEC),IHIT(IHSEC),1)
          CALL UCOPY(QHSEC(3,IHSEC),RESID(IHSEC),1)
  200   CONTINUE
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
C ****  Check on which sectors overlap the first and second segments.
C
        CALL FOVRLP(FH,FU,FQ,FS,FH,0,0,0,3,NFSECT,BOVRLP,FOVCHK)
        IF(.NOT.FOVCHK) THEN            ! no first track layer overlaps
          GOTO 100
        ENDIF
        CALL FOVRLP(SH,SU,SQ,SS,SH,0,0,0,3,NSSECT,SOVRLP,SOVCHK)
        IF(.NOT.SOVCHK) THEN            ! no second track layer overlaps
          GOTO 100
        ENDIF
C
C ****  Compare overlap findings for common sectors.
C
        CALL FOCMPR(NFSECT,BOVRLP,NSSECT,SOVRLP,NMATCH,LOCMATCH,MATCH)
        IF(.NOT.MATCH) THEN             ! no matching overlap sectors
          GOTO 100
        ENDIF
C
C ****  Find all segments in matching overlapping sectors.
C
        MLAYER=LOCMATCH(1,2)*2 + LOCMATCH(1,3)/4
        MMODULE=LOCMATCH(1,1)*3+MLAYER
        LSEG=GZFSEG(LOCMATCH(1,1),MLAYER)
        IF(LSEG.LE.0) THEN              ! no segments to match in layer
          GOTO 100
        ENDIF
        NSEG=NZBANK(IXCOM,LSEG)
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
   30     CONTINUE
   20   CONTINUE
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
C ****  Find plane for missing sectors then draw set of three.
C
        DO 50 ISEG=1,NTEST
          CALL FSGXYZ(MMODULE,TESTSG(ISEG),X,Y,Z,OK)
          IF(.NOT.OK) THEN              ! FSGXYZ returns false, no points
            GOTO 50
          ENDIF
          RTRK=SQRT( ((FX(1)+SX(1))/2.)**2. + ((FX(2)+SX(2))/2.)**2. )
          CALL PF3DSGDR(MMODULE,TESTSG(ISEG),RTRK)
          CALL FPLANE(3,X,Y,Z,A,B,C,D,IER)
          IF(IER.NE.0) THEN             ! FPLANE returns false, bad points
            GOTO 50
          ENDIF
C
C ****  Draw the three planes on display
C
          CALL PFPLAN(FH,FA,FB,FC,FD)
          CALL PFPLAN(SH,SA,SB,SC,SD)
          CALL PFPLAN(MMODULE/3,A,B,C,D)
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
          CALL PFILIN(FH,FA,FB,FC,FD,SA,SB,SC,SD,X1,Y1,Z1,X2,Y2,Z2)
          X1S=X1S+X1
          X2S=X2S+X2
          Y1S=Y1S+Y1
          Y2S=Y2S+Y2
          Z1S=Z1S+Z1
          Z2S=Z2S+Z2
          CALL PFILIN(FH,FA,FB,FC,FD,A,B,C,D,X1,Y1,Z1,X2,Y2,Z2)
          X1S=X1S+X1
          X2S=X2S+X2
          Y1S=Y1S+Y1
          Y2S=Y2S+Y2
          Z1S=Z1S+Z1
          Z2S=Z2S+Z2
          CALL PFILIN(SH,A,B,C,D,SA,SB,SC,SD,X1,Y1,Z1,X2,Y2,Z2)
          X1S=X1S+X1
          X2S=X2S+X2
          Y1S=Y1S+Y1
          Y2S=Y2S+Y2
          Z1S=Z1S+Z1
          Z2S=Z2S+Z2
C
C ****  Scale the points to the Z0 location for comparison to track X0,Y0,
C ****  dxdz,dydz parameters, debug only.
C
C          WRITE(61,*) ' FPTRAK - AVGS X0,Y0=',
C     &             (X1S/3.)*Z0(FH+1)/100.,(Y1S/3.)*Z0(FH+1)/100.
C          WRITE(61,*) ' FPTRAK - SLOPES DXDZ,DYDZ=',
C     &             ((X2S-X1S)/(Z2S-Z1S)),((Y2S-Y1S)/(Z2S-Z1S))
C
   50   CONTINUE                        ! End of loop over sectors
C
  100 CONTINUE                          ! End of loop over tracks
C------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
