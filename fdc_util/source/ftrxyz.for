      SUBROUTINE FTRXYZ(IADDR1,IHIT1,IADDR2,IHIT2,FX,FY,FZ,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find three X,Y,Z points to define a plane
C-                         from one layer in a FDC track.
C-
C-   Inputs  : IADDR1 = FDTH Logical Address including LR bit 0
C-             IHIT1  = HIT location in FxSC bank
C-             IADDR2,IHIT2 = same for second hit
C-   Outputs : FX(1:3),FY(),FZ() = Locations of the three points of plane
C-             OK = TRUE if all three points found
C-
C-   Created   8-JUN-1990   Jeffrey Bantly
C-   Updated   1-JUL-1991   Robert E. Avery  Replace call to DRIDIR 
C-                                              with FDRIFTDIR.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IADDR1,IHIT1,IADDR2,IHIT2
      INTEGER FH,FU,FQ,FS,FW,FM,FL,UB,NEL,NWORDS,LR
C
      REAL FX(3),FY(3),FZ(3),QHIT(18),STAGGER,DRIFTD
      REAL SDRIFT,CDRIFT,XC,YC,ZC,DELAY
      REAL FSTAGR
C
      LOGICAL OK
C----------------------------------------------------------------------
      OK=.FALSE.
C
C  Accumulate data for first hit.
C
      CALL FCODER((IADDR1/2),FH,FU,FQ,FS,FW,UB,1)   ! decode address
      CALL FDRIFTDIR(FH,FU,FQ,FS,FW,SDRIFT,CDRIFT)  ! get drift direction
      CALL GTFALH(FH,FU,FQ,FS,FW,XC,YC,ZC)          ! get wire position
      IF(ZC.EQ. 0.0) GOTO 999
      IF (FU.LE.0) THEN                             ! get hit info 
        CALL GTFTSC(FH,FQ,FS,'HIT',IHIT1,NEL,NWORDS,QHIT)
      ELSE
        CALL GTFPSC(FH,FS,'HIT',IHIT1,NEL,NWORDS,QHIT)
      END IF
      LR=IADDR1-2*(IADDR1/2)            ! determine L/R of hit
      STAGGER=0.                        ! determine stagger
      STAGGER=FSTAGR(FH,FU,FQ,FS,FW)
      DRIFTD=QHIT(2+LR)-STAGGER         ! calculate drift distance from wire
      DELAY=20.                         ! use phoney delay distance
C
C  Calculate first two points based on first hit given using delay distances
C  of 0. and 20., both arbitrary, even for Phi segment
C
      FX(1)=XC+DRIFTD*CDRIFT
      FX(2)=XC+DRIFTD*CDRIFT+DELAY*(-SDRIFT)
      FY(1)=YC+DRIFTD*SDRIFT
      FY(2)=YC+DRIFTD*SDRIFT+DELAY*CDRIFT
      FZ(1)=ZC
      FZ(2)=ZC
C
C  Accumulate data for second hit.
C
      CALL FCODER((IADDR2/2),FH,FU,FQ,FS,FW,UB,1)
      IF (FU.LE.0) THEN
        CALL GTFTSC(FH,FQ,FS,'HIT',IHIT2,NEL,NWORDS,QHIT)
      ELSE
        CALL GTFPSC(FH,FS,'HIT',IHIT2,NEL,NWORDS,QHIT)
      END IF
      LR=IADDR2-2*(IADDR2/2)
      STAGGER=0.
      STAGGER=FSTAGR(FH,FU,FQ,FS,FW)
      DRIFTD=QHIT(2+LR)-STAGGER
      CALL GTFALH(FH,FU,FQ,FS,FW,XC,YC,ZC)
      IF(ZC .EQ. 0.0) GOTO 999
C
C  Calculate third point based on second hit given using delay distance
C  of 0., arbitrary, even for Phi segment
C
      FX(3)=XC+DRIFTD*CDRIFT
      FY(3)=YC+DRIFTD*SDRIFT
      FZ(3)=ZC
C
      OK=.TRUE.
C----------------------------------------------------------------------
  999 RETURN
      END
