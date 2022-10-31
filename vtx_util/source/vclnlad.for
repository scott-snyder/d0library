      SUBROUTINE VCLNLAD(START,LAST,CAT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Loop over VTXTs with ID in the interval 
C-               [START,...,LAST] and look for pairs that share
C-               at least one segment in common. For each such pair, eliminate
C-               the one with the worse CHI2/DOF.  Then, renumber the surviving
C-               VTXTs and tag those segments used in the tracks as used.
C-
C-   Inputs  : START = Track id of first VTXT
C-             LAST  = Track id of last VTXT
C-             CAT   = 1 --> Layers 2-1-0
C-                     2 --> Layers 2-1
C-                     3 --> Layers 2-0
C-                     4 --> Layers 1-0
C-   Outputs : LAST = Track id of last VTXT after elimination process
C-   Controls: 
C-
C-   Created   3-SEP-1993   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
c I/O:
      INTEGER START,LAST,CAT
c Locals:
      LOGICAL FIRST
      INTEGER I,J,ERR,IUSED,LAYER,INNER(4),OUTER(4),SKIP(4)
      INTEGER LVTXT1,LVTXT2,LVTTH1,LVTTH2,LVTRH,LVSEG,SEG,ID,DROP
      REAL DELPHI,CLOSE,CHI1,CHI2
c Externals:
      INTEGER GZVTRH,GZVTXT,LZFIND
c Data:
      DATA FIRST/.TRUE./
      DATA INNER/0,1,0,0/
      DATA OUTER/2,2,2,1/
      DATA SKIP /1,1,2,1/
C----------------------------------------------------------------------
      IF (FIRST) THEN 
        FIRST = .FALSE.
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('CLOSE',CLOSE,ERR)
        CALL EZGET_i('IUSED',IUSED,ERR)
        CALL EZRSET
      ENDIF
      LVTRH = GZVTRH()
C
C ****  Loop over all pairs of tracks in interval [START,LAST]
C
      DROP = 0
      DO I = START,LAST-1
        LVTXT1 = GZVTXT(I)
        IF (LVTXT1 .EQ. 0) GO TO 30         ! THIS TRACK ALREADY DROPPED
        LVTTH1 = LQ(LVTXT1-1)
        DO J = I+1,LAST
          LVTXT2 = GZVTXT(J)
          IF (LVTXT2 .EQ. 0) GO TO 20       ! THIS TRACK ALREADY DROPPED
          LVTTH2 = LQ(LVTXT2-1)
C
C ****  Compare segment numbers if the tracks are close in PHI
C
          DELPHI =ABS(Q(LVTXT1+6)-Q(LVTXT2+6))
          IF (DELPHI .GT. 3.) DELPHI = ABS(DELPHI - TWOPI)
          IF (DELPHI .LT. CLOSE) THEN
            IF ( ((IQ(LVTTH1+1).EQ.IQ(LVTTH2+1)).AND.(CAT.NE.2)).OR.
     &           ((IQ(LVTTH1+2).EQ.IQ(LVTTH2+2)).AND.(CAT.NE.3)) .OR.
     &           ((IQ(LVTTH1+3).EQ.IQ(LVTTH2+3)).AND.(CAT.NE.4))) THEN
C
C ****  OK, two tracks share at least one segment -- eliminate one..
C
              IQ(LVTRH+2) = IQ(LVTRH+2) - 1
              IQ(LVTRH+5) = IQ(LVTRH+5) - 1
              CHI1 = Q(LVTXT1+12)/FLOAT(IQ(LVTXT1+2)-2)
              CHI2 = Q(LVTXT2+12)/FLOAT(IQ(LVTXT2+2)-2)
              DROP = DROP + 1
              IF (CHI1 .LT. CHI2) THEN
                CALL MZDROP(IXCOM,LVTXT2,' ')
              ELSE
                CALL MZDROP(IXCOM,LVTXT1,' ')
                GO TO 30
              ENDIF
            ENDIF
          ENDIF
   20     CONTINUE
        ENDDO
   30   CONTINUE
      ENDDO
C
C ****  Now, re-number VTXTs and tag used segment numbers
C
      LAST = LAST - DROP
      ID = LAST
      LVTXT1 = GZVTXT(0)
      DO WHILE (LVTXT1 .GT. 0)
        IF (ID .LT. START) GO TO 999
        IQ(LVTXT1-5) = ID
        LVTTH1 = LQ(LVTXT1-1)
        DO LAYER = INNER(CAT),OUTER(CAT),SKIP(CAT)
          SEG = IQ(LVTTH1+LAYER+1)
          LVSEG = LQ(LVTRH-3-LAYER)
          LVSEG = LZFIND(IXCOM,LVSEG,SEG,-5)
          IQ(LVSEG) = IBSET(IQ(LVSEG),IUSED)
          IQ(LVSEG+1) = ID
        ENDDO
        ID = ID - 1
        LVTXT1 = LQ(LVTXT1)
      ENDDO
  999 RETURN
      END
