      SUBROUTINE CDD3FL(VERSION,LNSKP,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill Header or Trailer Blocks for the
C-                         specfied Version in CDD3 bank.
C-
C-   Inputs  : VERSION = Input Version Number
C-             LNSKP   = Number of words to skip relative to LCDD3 before
C-             filling bank.
C-   Outputs : none
C-   Controls: IFL = 1, fill Header
C-                 = 2, fill Trailer
C-
C-   Created  10-OCT-1989   Srini Rajagopalan
C-   Updated  29-OCT-1989   Qizhong Li-Demarteau   correct trailer word
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$LINKS:IZCDD3.LINK'
C
      INTEGER VERSION,IFL,LNSKP,LCDD3,ISETVN
      INTEGER WRDVAL,CWORD(0:5),EVTID,MASK
      DATA CWORD / 0,0,3,2,15,3/         ! Controller word
      PARAMETER (MASK = 'FFFF'X)
C
      CHARACTER*80 MSGSTR
C----------------------------------------------------------------------
C
      LCDD3 = LQ(LHEAD-IZCDD3)
      IF (LCDD3.EQ.0) GOTO 999
      IF (VERSION.EQ.0) RETURN
      IF (VERSION.GT.2) THEN
        WRITE(MSGSTR,5)VERSION
    5   FORMAT(' Version = ',I2,'not yet implemented. Bank Not filled')
        CALL ERRMSG('Illegal Version','CDD3FL',MSGSTR,'W')
        RETURN
      ENDIF
C
      EVTID = IQ(LHEAD + 9)
      IF (IFL.EQ.2) GO TO 100
C
      IQ(LCDD3) = ISETVN( IQ(LCDD3), VERSION )  ! Put Version number into
C                                               ! Status word
      CALL MVBITS(MASK,0,16,WRDVAL,0)
      CALL MVBITS(EVTID,0,16,WRDVAL,16)
C
      IF (VERSION.EQ.1) THEN
        IQ(LCDD3+LNSKP+1) = 1
        IQ(LCDD3+LNSKP+2) = WRDVAL
      ELSE IF (VERSION.EQ.2) THEN
        IQ(LCDD3+LNSKP+1) = 5           ! Header Longword count
        IQ(LCDD3+LNSKP+2) = WRDVAL      ! SYNC Word
C
        IF (SFDC(5).EQ.0) THEN
          CWORD(1) = 0
        ELSE
          CWORD(1) = 3
        ENDIF
        CALL MVBITS(CWORD(0),0,4,WRDVAL,0)
        CALL MVBITS(CWORD(1),0,2,WRDVAL,4)
        CALL MVBITS(CWORD(2),0,2,WRDVAL,6)
        CALL MVBITS(CWORD(3),0,4,WRDVAL,8)
        CALL MVBITS(CWORD(4),0,4,WRDVAL,12)
        CALL MVBITS(CWORD(5),0,16,WRDVAL,16)
        IQ(LCDD3+LNSKP+3) = WRDVAL      ! Controller Word
C
        IQ(LCDD3+LNSKP+4) = 0           ! Spare
        CALL MVBITS(VERSION,0,29,WRDVAL,0)
        CALL MVBITS(1,0,3,WRDVAL,29)
        IQ(LCDD3+LNSKP+5) = WRDVAL      ! Version Number
        IQ(LCDD3+LNSKP+6) = 24          ! Header Byte count
      ENDIF
      GO TO 999
C
  100 CONTINUE
C
      IF (VERSION.EQ.1) THEN
        IQ(LCDD3+LNSKP+1) = 0
        IQ(LCDD3+LNSKP+2) = LNSKP
      ELSE IF (VERSION.EQ.2) THEN
        IQ(LCDD3+LNSKP+1) = LNSKP
        CALL MVBITS(3,0,16,WRDVAL,0)
        CALL MVBITS(EVTID,0,16,WRDVAL,16)
        IQ(LCDD3+LNSKP+2) = WRDVAL
        IQ(LCDD3+LNSKP+3) = 0
        IQ(LCDD3+LNSKP+4) = 0
      ENDIF
C
  999 RETURN
      END
