      SUBROUTINE ZFDTSG( TRASEG, LTRSEG )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book and fill DTSG bank
C-
C-   Inputs  : TRASEG( ltrseg ) : segment information
C-             LTRSEG[I] : length of one segment
C-
C-   Created  22-SEP-1987   Olivier Callot
C-   Updated   9-FEB-1988   Olivier Callot
C-   Updated  13-JUL-1989   Qizhong Li-Demarteau   put in version # 
C-   Updated  27-JUL-1989   Qizhong Li-Demarteau  use modified bank DTRH 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:CDLTRK.INC'
      INCLUDE 'D0$INC:CDLOCA.INC'
      INTEGER NBTRSG, LTRSEG, MPDTSG(5), ISETVN
      REAL    TRASEG( LTRSEG )
      LOGICAL FIRST

      INTEGER NBSEGM, IOF
      PARAMETER( NBSEGM= 50)
      DATA FIRST / .TRUE. /
      DATA MPDTSG / 0, 0, 0, 0, 0 /
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL UCTOH( 'DTSG', MPDTSG(1), 4, 4 )
        MPDTSG( 4 ) = 2 + NBSEGM * LTRSEG
        CALL MZFORM('DTSG', '2I/ 1B 1I 6F 7B 7F', MPDTSG(5) )
      ENDIF
C
C ****  Book the first time
C
      IF (LDTRH .LE. 0) CALL BKDTRH
      IF ( LDTSG(LAYER) .EQ. 0 ) THEN
        CALL MZLIFT( IXMAIN, LDTSG(LAYER), LDTRH, -(LAYER+2),
     &               MPDTSG, -1)
        IQ( LDTSG(LAYER) + 1 ) = 0
        IQ( LDTSG(LAYER) + 2 ) = LTRSEG
      ENDIF
      IOF = 3 + IQ( LDTSG(LAYER)+1 ) * IQ( LDTSG(LAYER) + 2 )
C
C ****  Increase size if needed
C
      IF ( IQ(LDTSG(LAYER)-1) .LT. IOF + LTRSEG ) THEN
        CALL MZPUSH( IXCOM,  LDTSG(LAYER), 0, NBSEGM*LTRSEG, 'I' )
      ENDIF
C
C ****  Store the track segment
C
      CALL UCOPY( TRASEG(1), Q( LDTSG(LAYER) + IOF), LTRSEG)
      IQ( LDTSG(LAYER) +1 )   = IQ( LDTSG(LAYER) + 1 ) + 1
      IQ( LDTRH + LAYER + 3 ) = IQ( LDTSG(LAYER) + 1 )
      IQ(LDTSG(LAYER)) = ISETVN(IQ(LDTSG(LAYER)),0)
  999 RETURN
      END
