      SUBROUTINE PCLIMB(IROOT,NNEXT,INEXT,IPASS)
C--------------------------------------------------------------------
C
C  Climb chains away from the interaction point starting from
C  link IROOT. Chains are saved by the routine SAVCHP
C
C-   Created  xx-DEC-1988   Daria Zieminska (ref: D.G.Cassel 
C-                                 and H.Kowalski, N.I.M.185(1981)235)
C-   Updated  28-FEB-1990   Jeffrey Bantly  clean up 
C-   Updated   3-MAY-1990   Jeffrey Bantly  pineff->set phi only ineff 
C
C--------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER IPASS
      INTEGER LLINK(NBPSEN-1),NBRNCH(NBPSEN-1),NNEXT(*),INEXT(8,*)
      INTEGER IDEPTH,IROOT,ILINK,IBRNCH,INEFF,MINDEP,ICALL,IER
C
      SAVE ICALL,INEFF,MINDEP
      DATA ICALL/0/
C--------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('PINEFF',INEFF,IER)
        CALL EZRSET
        MINDEP=NBPSEN-1-INEFF
        ICALL=1
      END IF
      IDEPTH=0
      ILINK=IROOT
      CALL VZERO(NBRNCH,NBPSEN-1)
      CALL VZERO(LLINK,NBPSEN-1)
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
      IF (IDEPTH.GE.MINDEP) CALL SAVCHP(IDEPTH,LLINK,IPASS)
C
   50 CONTINUE     ! climb down ILINK
      IDEPTH=IDEPTH-1
      IF (IDEPTH.EQ.0) GO TO 999
      ILINK=LLINK(IDEPTH)
      IF (NBRNCH(IDEPTH).LT.NNEXT(ILINK)) GO TO 20
      GO TO 50
C------------------------------------------------------------------------
  999 RETURN
      END
