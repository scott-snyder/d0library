      SUBROUTINE VCLNTRK(START)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Loop over all pairs of tracks in in VTXT bank with
C-               track-ID > START.  For each pair of tracks that share more 
C-               then MAX hits, (mirror hits are treated as identical) this
C-               routine attempts to eliminate one of them:  The track with the
C-               SMALLER product of xy- and rz-hits is deleted.  If both tracks
C-               have the same product of xy- and rz-hits, no track is deleted 
C-
C-               This routine assumes that IDs are in decreasing order, 
C-               beginning with LQ(LVTRH-1) -- this is the order that they are 
C-               booked in
C-
C-   Inputs  : START
C-   Outputs :
C-   Controls:
C-
C-   Created  15-SEP-1993   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
c I/O:
      INTEGER START
c Locals:
      INTEGER    MAXTRK
      PARAMETER (MAXTRK=1000)
      INTEGER LVTRH,LVTXT,LAST,NTRK,LTRK(MAXTRK),LVTTH1,LVTTH2
      INTEGER I,J,I1,I2,HIT1,HIT2,DROP,WIRE1,WIRE2,ID1,ID2,ID,SAME
      REAL    CLOSE,DIFF,PHI1,PHI2
      INTEGER LVTTH
      INTEGER MAX,ERR
      LOGICAL FIRST
c Externals:
      INTEGER GZVTRH
c Data:
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('ALLOW',MAX,ERR)
        CALL EZGET('CLOSE',CLOSE,ERR)
        CALL EZRSET
      ENDIF
      LVTRH = GZVTRH()
      LVTXT = LQ(LVTRH-1)
      LAST = 0
      IF (LVTXT .GT. 0) LAST = IQ(LVTXT-5)
      IF (LAST .LE. START) GO TO 999
      DROP = 0
      NTRK = 0
      LVTXT = LQ(LVTRH-1)
      DO WHILE(LVTXT .GT. 0)
        NTRK = NTRK + 1
        LTRK(NTRK) = LVTXT
        IF (NTRK .GE. LAST - START + 1) GO TO 101
        LVTXT = LQ(LVTXT)
      ENDDO
  101 CONTINUE
C
C ****  loop over all pairs of tracks..
C
      DO  I = 1,NTRK-1
        IF (LTRK(I) .EQ. 0) GO TO 30
        LVTTH1 = LQ(LTRK(I)-1)
        PHI1 = Q(LTRK(I)+6)
        HIT1   = IQ(LTRK(I)+2)
        DO J = I+1,NTRK
          IF (LTRK(J) .EQ. 0) GO TO 20
          PHI2 = Q(LTRK(J)+6)
          DIFF = ABS(PHI1-PHI2)
          IF (DIFF .GT. 3.) DIFF = ABS(DIFF-TWOPI)
          IF (DIFF .LT. CLOSE) THEN
            LVTTH2 = LQ(LTRK(J)-1)
            HIT2   = IQ(LTRK(J)+2)
C
C ****  now, loop over all hits
C
            SAME = 0
            I2 = 0
            DO I1 = 0,HIT1-1
              ID1 = IBCLR(IQ(LVTTH1+4*I1+6),0)          ! L/R IS EQIVALENT
              WIRE1 = IBITS(ID1,1,3) + 8*IBITS(ID1,9,2)
              DO WHILE (I2 .LE. HIT2-1)
                ID2 = IBCLR(IQ(LVTTH2+4*I2+6),0)        ! L/R IS EQUIVALENT
                WIRE2 = IBITS(ID2,1,3) + 8*IBITS(ID2,9,2)
                IF (ID1 .EQ. ID2) THEN
                  ID1 = ID1 + 2048*IQ(LVTTH1+7+4*I1)
                  ID2 = ID2 + 2048*IQ(LVTTH2+7+4*I2)
                  IF (IQ(LVTTH1+7+4*I1) .EQ. 
     &                IQ(LVTTH2+7+4*I2)    ) SAME = SAME + 1
                  IF (SAME .GT. MAX) THEN
                    IF (IQ(LTRK(I)+2)*IQ(LTRK(I)+5) .GT. 
     &                  IQ(LTRK(J)+2)*IQ(LTRK(J)+5) ) THEN
                      CALL MZDROP(IXCOM,LTRK(J),' ')
                      DROP = DROP + 1
                      LTRK(J) = 0
                      GO TO 20
                    ELSEIF(IQ(LTRK(I)+2)*IQ(LTRK(I)+5) .LT. 
     &                     IQ(LTRK(J)+2)*IQ(LTRK(J)+5)) THEN
                      CALL MZDROP(IXCOM,LTRK(I),' ')
                      DROP = DROP + 1
                      LTRK(I) = 0
                      GO TO 30
                    ENDIF
                    GO TO 20
                  ENDIF
                ENDIF
                I2 = I2 + 1
                IF (WIRE2 .GE. WIRE1) GO TO 10
              ENDDO
   10         CONTINUE
            ENDDO
          ENDIF
   20     CONTINUE
        ENDDO
   30   CONTINUE
      ENDDO
C
C ****
C
      LAST = LAST - DROP
      IQ(LVTRH+2) = IQ(LVTRH+2) - DROP
      IQ(LVTRH+5) = IQ(LVTRH+5) - DROP
      ID = LAST
      LVTXT = LQ(LVTRH-1)
      DO WHILE(LVTXT .GT. 0)
        IF (ID .LT. START) GO TO 999
        IQ(LVTXT-5) = ID
        ID = ID - 1
        LVTXT = LQ(LVTXT)
      ENDDO
  999 RETURN
      END
