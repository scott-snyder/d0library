      SUBROUTINE TCLIMB(IROOT,NNEXT,INEXT,IPASS)
C--------------------------------------------------------------------
C
C    Purpose: Climb chains away from the interaction point starting from
C             link IROOT. Chains are saved by the routine SAVCHT
C
C-   Created                Daria Zieminska
C-                     (ref: D.G.Cassel and H.Kowalski, N.I.M.185(1981)235)
C-   Updated  20-MAR-1990   Jeffrey Bantly  general cleanup
C-   Updated   3-MAY-1990   Jeffrey Bantly  tineff->theta only inefficiencies 
C
C--------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INTEGER IPASS
      INTEGER LLINK(NBTSEN-1),NBRNCH(NBTSEN-1),NNEXT(*),INEXT(8,*),IER
      INTEGER IDEPTH,IROOT,ILINK,IBRNCH,INEFF,MINDEP,ICALL
      SAVE ICALL,INEFF,MINDEP
      DATA ICALL/0/
C--------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('TINEFF',INEFF,IER)
        CALL EZRSET
        MINDEP=NBTSEN-1-INEFF
        ICALL=1
      END IF
      IDEPTH=0
      ILINK=IROOT
      CALL VZERO(NBRNCH,NBTSEN-1)
      CALL VZERO(LLINK,NBTSEN-1)
C
   10 CONTINUE     ! climb up ILINK
C
      IDEPTH=IDEPTH+1
      NBRNCH(IDEPTH)=0
      LLINK(IDEPTH)=ILINK
C
   20 CONTINUE
C
      NBRNCH(IDEPTH)=NBRNCH(IDEPTH)+1
      IBRNCH=NBRNCH(IDEPTH)
      IF (IBRNCH.GT.NNEXT(ILINK)) GO TO 30
      ILINK=INEXT(IBRNCH,ILINK)
      GO TO 10
C
   30 CONTINUE     ! climbing stopped at the top  of a chain - save it
C
      IF (IDEPTH.GE.MINDEP) CALL SAVCHT(IDEPTH,LLINK,IPASS)
C
   50 CONTINUE     ! climb down ILINK
      IDEPTH=IDEPTH-1
      IF (IDEPTH.EQ.0) GO TO 1000
      ILINK=LLINK(IDEPTH)
      IF (NBRNCH(IDEPTH).LT.NNEXT(ILINK)) GO TO 20
      GO TO 50
C-------------------------------------------------------------------------
 1000 RETURN
      END
