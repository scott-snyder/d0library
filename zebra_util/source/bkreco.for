      SUBROUTINE BKRECO(LRECO)
C-----------------------------------------------------------------------
C  Subroutine BKRECO books bank RECO
C
C  Output:
C    LRECO       location of the booked bank in ZEBCOM.
C
C  Daria Zieminska May  1987
C                  updated Oct 1988
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZRECO.LINK/LIST'
      INTEGER LRECO
C
      LRECO = LQ( LHEAD - IZRECO )
      IF( LRECO .EQ. 0 ) THEN
        CALL MZBOOK(IXMAIN,LRECO,LHEAD,-IZRECO,'RECO',3,3,1,2,0)
        IQ(LRECO+1)=1     ! version number
      ENDIF
      RETURN
      END
