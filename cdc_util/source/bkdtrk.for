      SUBROUTINE BKDTRK(LDTRK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : book a DTRK bank for CDC track
C-
C-   Inputs  : none
C-   Outputs : LDTRK: DTRK bank address
C-
C-   Created   5-JAN-1994   Qizhong Li-Demarteau
C-   Updated  20-MAY-1994   Srini Rajagopalan  Pass 0 to MZLIFT to reset bank 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZDTRK.LINK'
      INTEGER MPDTRK(5), LDTRH, GZDTRH, LDTRK
      INTEGER ISETVN
      LOGICAL FIRST
      DATA MPDTRK / 0, 2, 1, 27, 0 /
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL UCTOH( 'DTRK', MPDTRK(1), 4, 4 )
        CALL MZFORM('DTRK', '1B 4I 8F 2I 12F', MPDTRK(5) )
      ENDIF
      LDTRK = 0
      LDTRH = GZDTRH()
      IF (LDTRH .LE. 0) THEN
        CALL BKDTRH
        LDTRH = GZDTRH()
      ENDIF
      IF (LDTRH .LE. 0) GOTO 999
      LDTRK = LQ(LDTRH - IZDTRK)
      IF (LDTRK .EQ. 0) THEN
C
C ****  first track. Book from DTRH
C
        CALL MZLIFT( IXMAIN, LDTRK, LDTRH, -1, MPDTRK, 0 )
        IQ( LDTRK-5) = 1
      ELSE
        CALL MZLIFT( IXMAIN, LDTRK, LDTRK, 0, MPDTRK, 0 )
      ENDIF
      IQ(LDTRK) = ISETVN(IQ(LDTRK),0)
C
C ****  Update track counter
C
      IQ(LDTRH+2) = IQ(LDTRH+2) + 1
      IQ(LDTRH+7) = IQ(LDTRH+7) + 1
C
  999 RETURN
      END
