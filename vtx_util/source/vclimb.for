      SUBROUTINE VCLIMB(IROOT,NNEXT,INEXT,IPASS)
C--------------------------------------------------------------------
C             
C  Climb chains away from the interaction point starting from
C  link IROOT. Chains are saved by the routine SAVCHN
C
C  Daria Zieminska (ref: D.G.Cassel and H.Kowalski, N.I.M.185(1981)235)
C
C--------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IPASS
      INTEGER MAXLNK
      PARAMETER (MAXLNK=7)
      INTEGER LLINK(MAXLNK),NBRNCH(MAXLNK),NNEXT(*),INEXT(8,*)
      INTEGER IDEPTH,IROOT,ILINK,IBRNCH,INEFF,MINDEP,ICALL
      DATA ICALL/0/
      INTEGER IER 
C--------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('INEFF',INEFF,IER)
        CALL EZGET('INEFF',INEFF,IER)
        CALL EZRSET
        MINDEP=7-INEFF
        ICALL=1
      END IF
      IDEPTH=0
      ILINK=IROOT
      CALL VZERO(NBRNCH,MAXLNK)
      CALL VZERO(LLINK,MAXLNK)
C
   10   CONTINUE     ! climb up ILINK        
C
        IDEPTH=IDEPTH+1
        NBRNCH(IDEPTH)=0
        LLINK(IDEPTH)=ILINK
C
   20   CONTINUE     
C
        NBRNCH(IDEPTH)=NBRNCH(IDEPTH)+1
        IBRNCH=NBRNCH(IDEPTH)
        IF (IBRNCH.GT.NNEXT(ILINK)) GO TO 30
        ILINK=INEXT(IBRNCH,ILINK)
        GO TO 10
C
   30   CONTINUE     ! climbing stopped at the top  of a chain - save it
C
        IF (IDEPTH.GE.MINDEP) CALL SAVCHN(IDEPTH,LLINK,IPASS)
C
   50   CONTINUE     ! climb down ILINK
        IDEPTH=IDEPTH-1
        IF (IDEPTH.EQ.0) GO TO 1000
        ILINK=LLINK(IDEPTH)
        IF (NBRNCH(IDEPTH).LT.NNEXT(ILINK)) GO TO 20
        GO TO 50
 1000   RETURN
        END                            
