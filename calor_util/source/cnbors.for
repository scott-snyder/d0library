      SUBROUTINE CNBORS(IET,IPH,ILY,IDET,IDPH,IDLY,NJ,JET,JPH,JLY,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find the neighbors of a cal cell in a given dir.
C-                         Note that this version calls CEXIST for each
C-                         neighbor cell.  Note also that due to EM3 &
C-                         the change in phi division some cells will be
C-                         returned as neighbors in several directions.
C-                         The given cell is assumed to exist;  results
C-                         are unpredictable otherwise.
C-
C-   Inputs  : IET,IPH,ILY     Physics indices of given cell
C-             IDET,IDPH,IDLY  Desired changes in indices (-1,0,+1)
C-   Outputs : NJ              Number of neighbor cells
C-             JET(4),JPH(4),JLY(4) Physics indices of neighbor cell(s)
C-             OK              0 if input OK, not 0 otherwise
C-   Controls: 
C-
C-   Created  19-MAY-1989   Michael W. Peters
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER IET,IPH,ILY,IDET,IDPH,IDLY,JET(4),JPH(4),JLY(4),OK
      INTEGER NJ
      LOGICAL CEXIST
      INTEGER KLYE(3:6),KETE(3:6),KLYP(3:6),KPHP(3:6)
      INTEGER KLYL(4),J,JJ
      DATA KLYE/5,6,3,4/,KETE/0,0,1,1/
      DATA KLYP/4,3,6,5/,KPHP/0,1,0,1/
      DATA KLYL/3,4,5,6/
C
      NJ=0
      IF(ABS(IDET).GT.1.OR.ABS(IDPH).GT.1.OR.ABS(IDLY).GT.1) THEN
        OK=1
        GO TO 999
      ENDIF
      NJ=1
      IF(ABS(IET).LE.26.AND.ILY.GE.3.AND.ILY.LE.6) THEN
C
C ****  Started inside subdivided EM3 region
C
        JET(1)=IET
        JPH(1)=IPH
        JLY(1)=ILY
        IF(IDET.NE.0) THEN
          JET(1)=IET+KETE(JLY(1))
          IF(IDET.EQ.-1) JET(1)=JET(1)-1
          JLY(1)=KLYE(JLY(1))
        ENDIF
        IF(IDPH.NE.0) THEN
          JPH(1)=IPH+KPHP(JLY(1))
          IF(IDPH.EQ.-1) JPH(1)=JPH(1)-1
          JLY(1)=KLYP(JLY(1))
        ENDIF
C
C ****       Moved out of subdivided EM3?
C
        IF(ABS(JET(1)).GT.26.AND.JLY(1).GE.3.AND.JLY(1).LE.6) JLY(1)=3
C
C ****       Moved out of EM3 completely?
C
        IF(IDLY.EQ.-1) THEN
          JLY(1)=2
        ELSEIF(IDLY.EQ.1) THEN
          JLY(1)=7
        ENDIF
      ELSE
C
C ****  Started outside subdivided EM3
C
        JET(1)=IET+IDET
        IF(ABS(IET).LE.32) THEN
          JPH(1)=IPH+IDPH
        ELSE
          JPH(1)=IPH+2*IDPH
        ENDIF
        JLY(1)=ILY+IDLY
C
C ****       Moved into normal phi region from coarse phi region?
C
        IF(ABS(JET(1)).EQ.32.AND.ABS(IET).EQ.33) THEN
          NJ=0
          IF(IDPH.GE.0) THEN
            NJ=NJ+1
            JPH(NJ)=JPH(1)
          ENDIF
          IF(IDPH.LE.0) THEN
            NJ=NJ+1
            JPH(NJ)=JPH(1)+1
          ENDIF
          DO 400 J=1,NJ
            JET(J)=JET(1)
            JLY(J)=JLY(1)
  400     CONTINUE
C
C ****       Moved into subdivided EM3?
C
        ELSEIF(ABS(JET(1)).LE.26.AND.JLY(1).GE.3.AND.JLY(1).LE.6) THEN
          NJ=0
          IF(IDET.GE.0.AND.IDPH.GE.0) THEN
            NJ=NJ+1
            JLY(NJ)=3
          ENDIF
          IF(IDET.GE.0.AND.IDPH.LE.0) THEN
            NJ=NJ+1
            JLY(NJ)=4
          ENDIF
          IF(IDET.LE.0.AND.IDPH.GE.0) THEN
            NJ=NJ+1
            JLY(NJ)=5
          ENDIF
          IF(IDET.LE.0.AND.IDPH.LE.0) THEN
            NJ=NJ+1
            JLY(NJ)=6
          ENDIF
          DO 500 J=1,NJ
            JET(J)=JET(1)
            JPH(J)=JPH(1)
  500     CONTINUE
        ENDIF
      ENDIF
C
C ****  Now make various corrections
C
      JJ=0
      DO 800 J=1,NJ
C
C ****  Correct for lack of eta=0
C
      IF(JET(J).EQ.0) JET(J)=ISIGN(1,IDET)
C
C ****  Correct for phi wrap around
C
      JPH(J)=MOD(JPH(J)+63,64)+1
C
C ****  Correct for lack of even phi values if eta>=33
C
      IF(ABS(JET(J)).GE.33.AND.MOD(JPH(J),2).EQ.0) JPH(J)=JPH(J)-1
C
C ****  Finally, check for existence of the cell
C
      IF(CEXIST(JET(J),JPH(J),JLY(J))) THEN
        JJ=JJ+1
        JET(JJ)=JET(J)
        JPH(JJ)=JPH(J)
        JLY(JJ)=JLY(J)
      ENDIF
  800 CONTINUE
      NJ=JJ
      OK=0
  999 RETURN
      END
