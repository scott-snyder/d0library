      SUBROUTINE BKFAKE(LFAKE)
C-----------------------------------------------------------------------
C  Subroutine BKFAKE books bank FAKE
C
C  Output:
C    LFAKE       location of the booked bank in ZEBCOM.
C
C  Daria Zieminska May  1987
C                  updated Oct 1988
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZFAKE.LINK/LIST'
      INTEGER LFAKE
C
      LFAKE = LQ( LHEAD - IZFAKE )
      IF( LFAKE .EQ. 0 ) THEN
        CALL MZBOOK(IXMAIN,LFAKE,LHEAD,-IZFAKE,'FAKE',3,3,1,2,0)
        IQ(LFAKE+1)=1     ! version number
      ENDIF
      RETURN
      END
