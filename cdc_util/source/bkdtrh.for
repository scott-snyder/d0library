      SUBROUTINE BKDTRH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the structure up to and including DTRH,
C-                         CDC track head bank
C-
C-   Inputs  : none
C-   Outputs : none ( bank created )
C-
C-   Created   8-FEB-1988   Olivier Callot
C-   Updated  12-JUL-1989   Qizhong Li-Demarteau  fix bug in MZLINT and
C-                                                set version number
C-   Updated   4-OCT-1989   Qizhong Li-Demarteau  add reference link to
C-                                                history bank HSTR 
C-   Updated   8-NOV-1989   Qizhong Li-Demarteau  MZLINT moved to DTRINI 
C-   Updated  11-FEB-1993   Qizhong Li-Demarteau  added three words in
C-                                                DTRH bank 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDLTRK.INC'
      INCLUDE 'D0$LINKS:IZZTRH.LINK'
      INCLUDE 'D0$LINKS:IZDTRH.LINK'
      INTEGER  LZTRH, GZZTRH, MPDTRH(5), ISETVN, GZHSTR
      INTEGER PLCDCH, GZCDCH, PLCDD2, GZCDD2
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /
      DATA MPDTRH / 0, 7, 6, 9, 2 /
C----------------------------------------------------------------------
C
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL UCTOH( 'DTRH', MPDTRH(1), 4, 4 )
      ENDIF
      LZTRH = GZZTRH()
      IF (LZTRH.EQ.0) CALL BKZTRH(LZTRH)
C
C ****  Bank DTRH, header for CDC Tracks and segments
C
      LDTRH = LQ( LZTRH - IZDTRH )
      IF ( LDTRH .EQ. 0 ) THEN
        CALL MZLIFT( IXMAIN, LDTRH, LZTRH, -IZDTRH, MPDTRH, 0 )
      ENDIF
      LQ(LDTRH - 7) = GZHSTR()        ! Reference Link to latest History
      IQ(LDTRH) = ISETVN(IQ(LDTRH),0)
      IQ(LDTRH + 1) = 0                 ! version number
C
      PLCDCH = GZCDCH()
      IF (PLCDCH .GT. 0) THEN
        IQ(LDTRH+8) = IQ(PLCDCH+1) 
      ENDIF
      PLCDD2 = GZCDD2()
      IF (PLCDD2 .GT. 0) THEN
        IQ(LDTRH+9) = IQ(PLCDD2-1)  
      ENDIF
C
  999 RETURN
      END
