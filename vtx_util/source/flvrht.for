      SUBROUTINE FLVRHT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill VRHT banks from  VRHS banks
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  16-JUN-1994   Ed Oltman
C-
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LVRHT,WORDS_PER_HIT,PTR,PTS,WCNT,LAYER,WIRE,NH1,NH2,L1,L2
      INTEGER NHS1,NHS2
C Externals:
      INTEGER GZVRHT
C----------------------------------------------------------------------
      LVRHT = GZVRHT()
      IF (LVRHT .EQ. 0) CALL ERRMSG('No VRHT bank??','FLVRHT',' ','F')
      WORDS_PER_HIT = IQ(LVRHT+6)
C
C ****  This is positon of next word to be added to VRHT bank
C
      PTR = IQ(LVRHT+12) + IQ(LVRHT+13)*WORDS_PER_HIT + 1
      L1 = LQ(LVRHT-1)
      L2 = LQ(LVRHT-2)
      IF (L1 .EQ. 0 .AND. L2 .EQ. 0) GO TO 999
      IF (L1 .GT. 0) NHS1 = IQ(L1+1)
      IF (L2 .GT. 0) NHS2 = IQ(L2+1)
      IQ(LVRHT+13) = IQ(LVRHT+13) + NHS1+NHS2
      LAYER = IQ(L1-5)
      CALL VZERO(Q(LVRHT+7*LAYER+17),8)
      CALL VZERO(Q(LVRHT+7*LAYER+41),8)
C
C ****  IS VRHT BANK BIG ENOUGH?  MAKE SURE...
C
      IF ( IQ(LVRHT-1) .LT. PTR + (NHS1+NHS2)*WORDS_PER_HIT-1) THEN
        CALL ERRMSG('VRHT bank ran out of space -- MZPUSHed',
     &              'FLVRHT',
     &              'Chech road width...','W')
        CALL MZPUSH(IXCOM,LVRHT,0,120*WORDS_PER_HIT,'I')
        L1 = LQ(LVRHT-1)
        L2 = LQ(LVRHT-2)
      ENDIF
      DO WIRE = 0,7
        NH1 = 0
        NH2 = 0
        IF (L1 .GT. 0) NH1 = IQ(L1+6+WIRE)
        IF (L2 .GT. 0) NH2 = IQ(L2+6+WIRE)
        IF (NH1+NH2 .GT. 0) THEN
          IQ(LVRHT+17+7*LAYER+WIRE) = NH1+NH2
          IQ(LVRHT+41+7*LAYER+WIRE) = PTR
          IF (NH1 .GT. 0) THEN
            PTS = IQ(L1+14+WIRE)
            WCNT = NH1*WORDS_PER_HIT
            CALL UCOPY(Q(L1+PTS),Q(LVRHT+PTR),WCNT)
            PTR = PTR + WCNT
          ENDIF
          IF (NH2 .GT. 0) THEN
            PTS = IQ(L2+14+WIRE)
            WCNT = NH2*WORDS_PER_HIT
            CALL UCOPY(Q(L2+PTS),Q(LVRHT+PTR),WCNT)
            PTR = PTR + WCNT
          ENDIF
        ENDIF
      ENDDO
      IF (L1 .GT. 0) CALL MZDROP(IXCOM,L1,'L')
      IF (L2 .GT. 0) CALL MZDROP(IXCOM,L2,'L')
  999 RETURN
      END
