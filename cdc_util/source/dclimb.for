      SUBROUTINE DCLIMB(IROOT,NNEXT,INEXT,IPASS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Climb through the branches of a tree 
C-                         away from the interaction point to make
C-                         chains.  Chains are saved in DSVCHN.
C-
C-     MXSENS comes from CDPARA.INC and is the maximum wire number = 6
C-          This also is then the maximum number of links since we have
C-          seven wires...
C-           
C-   Inputs  : IROOT = integer ID of the root link
C-             NNEXT(J) = array containing the number 
C-                        of branchs for link ID number J
C-             INEXT(K(J),J) = K(J) is the branch ID for link 
C-                             ID number J.  This element contains
C-                             the Link ID number for this branch.
C-             IPASS = the pass number
C-   Outputs : none
C-
C-   Created   8-NOV-1989   joey thompson:  Based on VCLIMB
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET, EZERROR
C-                                                and SAVE statement
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:DLTPAR.INC'
      INTEGER IPASS
      INTEGER LINK(MXSENS),NBRNCH(MXSENS),NNEXT(MAXLNK)
      INTEGER INEXT(MXNEXT,MAXLNK)
      INTEGER IDEPTH,IROOT,ILINK,IBRNCH,INEFF,MINDEP,ICALL
      INTEGER IER
      LOGICAL EZERROR
C
      SAVE ICALL
      DATA ICALL/0/
C
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','DCLIMB',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('INEFF',INEFF,IER)
        CALL EZRSET
        MINDEP=MXSENS-INEFF
        ICALL=1
      END IF
      IDEPTH=0
      ILINK=IROOT
      CALL VZERO(NBRNCH,MXSENS)
      CALL VZERO(LINK,MXSENS)
C
   10   CONTINUE     ! climb up ILINK        
C
        IDEPTH=IDEPTH+1
        NBRNCH(IDEPTH)=0
        LINK(IDEPTH)=ILINK
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
        IF (IDEPTH.GE.MINDEP) CALL DSVCHN(IDEPTH,LINK,IPASS)
C
   50   CONTINUE     ! climb down ILINK
        IDEPTH=IDEPTH-1
        IF (IDEPTH.EQ.0) GO TO 999
        ILINK=LINK(IDEPTH)
        IF (NBRNCH(IDEPTH).LT.NNEXT(ILINK)) GO TO 20
        GO TO 50
  999   RETURN
        END                            
