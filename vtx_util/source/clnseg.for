      SUBROUTINE CLNSEG(LAYER,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Look at all segment in LAYER,SECTOR and eliminate the
C-               worse  (chi2/dof) of each pair that have SHARED or more hits in
C-               common
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  26-JAN-1993   Ed Oltman
C-   Updated  17-MAR-1993   Ed Oltman  Allow mirror hits to lie on different
C-             segments. Also drop the one with higher chi2/dof in a pair
C-   Updated   7-APR-1993   Ed Oltman  Allow 2nd pass if more then MAXSEG
C-             segments; replace INTMSG call w/ ERRMSG if 2-pass not enough
C-                          Liang-ping Chen require CHI2<CHI1, no longer 
C_                                     both CHI2<CHI1 and HIT2>hit1   
C-   Updated  10-MAY-1993   Ed Oltman  Fix "restore bank number" bug
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
c I/O:
      INTEGER LAYER,SECTOR
c Locals:
      INTEGER MAXSEG
      PARAMETER (MAXSEG = 100)
      LOGICAL FIRST,REDO
      REAL    CHI1,CHI2
      INTEGER LVTRH,LVSEG,NSEG,I,J,I1,ID1,I2,ID2,SAME,SHARED,SIDE1,SIDE2
      INTEGER LVSG(MAXSEG),NSAME(MAXSEG),LIST(MAXSEG,MAXSEG),SAVE,LNEXT
      INTEGER NDROP,DROP(MAXSEG),IDSG(MAXSEG),HITS(MAXSEG),HIT1,HIT2
      REAL    CHISQ(MAXSEG)
      CHARACTER*60 TXT
c Externals:
      INTEGER GZVTRH,LZFIND
c Data:
      DATA SHARED/2/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      REDO = .FALSE.
      LVTRH = GZVTRH()
  100 CONTINUE
      LVSEG = LQ(LVTRH-LAYER-3)
      NSEG = 0
C
C ****  Count up segments in LAYER,SECTOR, save pointers and phi values
C
      DO WHILE (LVSEG .GT. 0)
        IF( MOD(IQ(LVSEG+2),32) .NE. SECTOR) GO TO 10
        NSEG = NSEG + 1
        IF (NSEG .LE. MAXSEG) THEN
          LVSG(NSEG) = LVSEG
          IDSG(NSEG) = IQ(LVSEG-5)
          CHISQ(NSEG) = Q(LVSEG+24)/FLOAT(IQ(LVSEG+3)-2)
          HITS(NSEG) = IQ(LVSEG+3)
        ENDIF
   10   LVSEG = LQ(LVSEG)
      ENDDO
      IF (NSEG .LE. 1) GO TO 999
      IF (REDO .AND. NSEG .GT. MAXSEG) THEN
        WRITE(TXT,'(I4,A,2I3)') NSEG,
     &    ' segments survive after first pass for lay,sec = ',
     &    LAYER,SECTOR 
        CALL ERRMSG('Too many VTX segments','CLNSEG',TXT,'W')
        GO TO 500
      ENDIF
      REDO = NSEG .GT. MAXSEG
      NSEG = MIN0(NSEG,MAXSEG)
C
C ****  Loop over all pairs of segments in sector, 
C ****  lable the pairs with more than SHARED hits are common 
C
      
      DO I = 1,NSEG-1
        NSAME(I) = 0
        DO J = I+1,NSEG
          SAME = 0
          DO I1 = 1,IQ(LVSG(I)+3)
            ID1 = IQ(LVSG(I)+I1+11)
            SIDE1 = MOD(IQ(LVSG(I)+I1+3),2)
            DO I2 = 1,IQ(LVSG(J)+3)
              ID2 = IQ(LVSG(J)+I2+11)
              SIDE2 = MOD(IQ(LVSG(J)+I2+3),2)
              IF ((ID1 .EQ. ID2) .AND.
     &            (SIDE1 .EQ. SIDE2) ) SAME = SAME + 1
              IF (SAME .EQ. SHARED) GO TO 20
            ENDDO
          ENDDO
          GO TO 30
   20     CONTINUE
C
C ****  At least SHARED hits are common to segment I and J.  Increment NSAME and
C ****  update LIST
C
          NSAME(I) = NSAME(I) + 1
          LIST(I,NSAME(I)) = J
   30     CONTINUE
        ENDDO
   40   CONTINUE
      ENDDO
C
C ****  Now, get rid of duplicate segments:  first mark them with big chi2
C
      DO I = 1,NSEG-1
        IF (NSAME(I) .GT. 0) THEN
          CHI1 = CHISQ(I)
          HIT1 = HITS(I)
          SAVE = I
          DO I1 = 1,NSAME(I)
            J = LIST(I,I1)
            CHI2 = CHISQ(J)
            HIT2 = HITS(J)
            IF ((CHI2 .LT. CHI1) ) THEN 
              SAVE = J
              CHI1 = CHI2
            ELSE
              CHISQ(J) = 9999.
            ENDIF
          ENDDO
          IF (SAVE .NE. I) CHISQ(I) = 9999.
        ENDIF
      ENDDO
      NDROP = 0
      DO I = 1,NSEG
        IF (CHISQ(I) .GT. 999.) THEN
          NDROP = NDROP + 1
          DROP(NDROP) = IDSG(I)
        ENDIF
      ENDDO
C
C ****  drop 'em
C
      LVTRH = GZVTRH()
      DO I = 1,NDROP
        LVSEG = LQ(LVTRH - LAYER - 3)
        LVSEG = LZFIND(IXCOM,LVSEG,DROP(I),-5)
        CALL MZDROP(IXCOM,LVSEG,' ')
      ENDDO
      IF (REDO) GO TO 100
C
C ****  And restore the bank numbers so that LINSEG works 
C
  500 LVTRH = GZVTRH()
      LVSEG = LQ(LVTRH - LAYER - 3)
      I = 1
      DO WHILE (LVSEG .GT. 0)
        IQ(LVSEG-5) = I
        IQ(LVSEG+1) = I
        I = I + 1
        LVSEG = LQ(LVSEG)
      ENDDO
  999 RETURN
      END
