      SUBROUTINE FSGXYZ(MODULE,ISEG,X,Y,Z,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find three points to form a plane from a
C-                         found segment.
C-
C-   Inputs  : MODULE,ISEG = Module and segment number for that module
C-   Outputs : X,Y,Z = coordinates of the three points to define plane.
C-             OK    = TRUE if all three points found
C-
C-   Created   8-JUN-1990   Jeffrey Bantly
C-   Updated   8-NOV-1990   Jeffrey Bantly  add cross-sector possibility 
C-   Updated  29-APR-1991   Jeffrey Bantly  cleanup, FSTAGR external 
C-   Updated  17-JUN-1991   Susan K. Blessing  Change size of CONT, ICONT
C-    arrays.
C-   Updated   1-JUL-1991   Robert E. Avery  Replace call to DRIDIR 
C-                                              with FDRIFTDIR.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER MODULE,ISEG,IADDS,NHITS,HALF,UNIT,QUAD,SECTOR,WIRE,UB
      INTEGER NEL,NWORDS,IHIT1,IHIT2,LR1,LR2,SECTR,ISECTD
C
      REAL X(3),Y(3),Z(3),QHIT(18),STAGGER,DRIFTD
      REAL SDRIFT,CDRIFT,XC,YC,ZC,DELAY,RESIDUAL
      REAL CONT(62),FIADDS,FNHITS,FSECTD
      EQUIVALENCE(ISECTD,FSECTD)
      EQUIVALENCE(IADDS,FIADDS)
      EQUIVALENCE(NHITS,FNHITS)
      REAL FSTAGR
      EXTERNAL FSTAGR
C
      LOGICAL OK
C----------------------------------------------------------------------
      OK=.FALSE.
C
C  Get segment info from FSG bank
C
      CALL GTFSEG(MODULE,ISEG,CONT)
      FIADDS=CONT(2)
      FNHITS=CONT(3)
      IF(NHITS.LE.0) GOTO 999
      CALL FCODER(IADDS,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
      CALL FDRIFTDIR(HALF,UNIT,QUAD,SECTOR,WIRE,SDRIFT,CDRIFT)
C
C  Accumulate data on first hit and fill X,Y,Z for two points using
C  fake delay line distance of 0.,20.cm even for Phi chamber segments
C
      WIRE=INT(CONT(4)/2.)
      LR1=INT(CONT(4))-2*WIRE
      IF (UNIT.LE.0) THEN
        IHIT1=INT(CONT(12))
        RESIDUAL=CONT(22)
        CALL GTFTSC(HALF,QUAD,SECTOR,'HIT',IHIT1,NEL,
     X           NWORDS,QHIT)
      ELSE
        IHIT1=INT(CONT(20))
        RESIDUAL=CONT(39)
        CALL GTFPSC(HALF,SECTOR,'HIT',IHIT1,NEL,NWORDS,QHIT)
      END IF
      CALL GTFALH(HALF,UNIT,QUAD,SECTOR,WIRE,XC,YC,ZC)
      IF(ZC.EQ. 0.0) GOTO 999
      STAGGER=0.
      IF(UNIT.GT.0 .OR. SECTOR.GT.2) STAGGER=
     &                              FSTAGR(HALF,UNIT,QUAD,SECTOR,WIRE)
      DRIFTD=QHIT(2+LR1)-STAGGER-RESIDUAL
      DELAY=20.
      X(1)=XC+DRIFTD*CDRIFT
      X(2)=XC+DRIFTD*CDRIFT+DELAY*(-SDRIFT)
      Y(1)=YC+DRIFTD*SDRIFT
      Y(2)=YC+DRIFTD*SDRIFT+DELAY*CDRIFT
      Z(1)=ZC
      Z(2)=ZC
C
C  Accumulate data on last hit and fill X,Y,Z for third point using
C  fake delay line distance of 0.0 cm even for Phi chamber segments
C
      WIRE=INT(CONT(3+NHITS)/2.)
      LR2=INT(CONT(3+NHITS))-2*WIRE
      FSECTD=CONT(1)
      SECTR=SECTOR
      IF (ABS(ISECTD).GE.50) THEN
        SECTR=SECTOR+(ISECTD/ABS(ISECTD))
      ENDIF
      IF (UNIT.LE.0) THEN
        IHIT2=INT(CONT(11+NHITS))
        RESIDUAL=CONT(21+NHITS)
        CALL GTFTSC(HALF,QUAD,SECTR,'HIT',IHIT2,NEL,
     X           NWORDS,QHIT)
      ELSE
        IHIT2=INT(CONT(19+NHITS))
        RESIDUAL=CONT(38+NHITS)
        CALL GTFPSC(HALF,SECTR,'HIT',IHIT2,NEL,NWORDS,QHIT)
      END IF
      STAGGER=0.
      STAGGER=FSTAGR(HALF,UNIT,QUAD,SECTR,WIRE)
      DRIFTD=QHIT(2+LR2)-STAGGER-RESIDUAL
      CALL GTFALH(HALF,UNIT,QUAD,SECTR,WIRE,XC,YC,ZC)
      IF(ZC.EQ. 0.0) GOTO 999
      X(3)=XC+DRIFTD*CDRIFT
      Y(3)=YC+DRIFTD*SDRIFT
      Z(3)=ZC
C
      OK=.TRUE.
C----------------------------------------------------------------------
  999 RETURN
      END
