      SUBROUTINE BKL0SC( LKL0SC, IBUNCH )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the L0SC (Level 0 SCALER) Bank
C-
C-   Inputs  : IBUNCH - bunch number of bank's future contents
C-   Outputs : LKL0SC - Bank link value
C-   Controls: NONE
C-
C-   Created  18-JUL-1992   Freedy Nang - Version 0
C-   Updated  26-JAN-1994   Jeffrey Bantly  make Run 1a and Run 1b compatible 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:LV0LNK.INC'
      INCLUDE 'D0$LINKS:IZL0SC.LINK'
C
      INTEGER GZLV0H
      INTEGER LKL0SC, IVERS
      INTEGER ISETVN
      INTEGER IXL0SC
      INTEGER IBUNCH
C
      REAL    FORM
C
      CHARACTER*80 VARMSG
C
      SAVE IVERS, IXL0SC
      DATA IVERS /0/
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
C ****  Assemble form of L0SC
C
      IF ( FIRST ) THEN     ! Only for first event
        CALL MZFORM( 'L0SC',' 41I ',IXL0SC)
        CALL L0_GET_FORMAT(FORM)
        IVERS=1
        IF ( FORM.GE.1.5 ) IVERS=2
        FIRST=.FALSE.
      ENDIF
C
C ****  Get name of the bank to hang on, 'GEAN' or 'RECO'
C
      IF ( LHEAD.EQ.0 ) THEN
        WRITE (VARMSG,10)
   10   FORMAT('  **BKL0SC  ** Cannot book L0SC because HEAD bank',
     &    ' does not exist')
        CALL ERRMSG('LEVEL0-LHEAD=0','BKL0SC',VARMSG,'F')
      ENDIF
C
C ****  Book LV0H bank if needed
C
      LLV0H=GZLV0H(0)
      IF ( LLV0H.LE.0 ) CALL BKLV0H(LLV0H)
      IF ( LLV0H.LE.0 ) GOTO 999
C
C ****  Book L0SC bank and store bank lengths inside if needed
C
        CALL MZBOOK(IXMAIN,LL0SC,LLV0H,-IZL0SC,'L0SC',0,0,41,IXL0SC,0)
        LKL0SC=LL0SC
        IQ(LKL0SC) = ISETVN ( IQ(LKL0SC), IVERS)    ! Version Number
C  set bunch number, number of scalers, number of words per scaler
C  number of scalers includes scaler 0.
        IQ(LKL0SC+1) = IBUNCH
        IQ(LKL0SC+2) = 15
        IF ( FORM.GE.2.0 ) IQ(LKL0SC+2) = 19
        IQ(LKL0SC+3) =  2
c      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
