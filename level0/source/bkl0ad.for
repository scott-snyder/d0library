      SUBROUTINE BKL0AD( LKL0ADH, IBUNCH )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the L0AD (LV0 ADC) Bank
C-
C-   Inputs  : IBUNCH - Bunch number
C-   Outputs : LKL0AD - Bank link value
C-   Controls: NONE
C-
C-   Created  14-JUL-1992   Freedy Nang - Version 0
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:LV0LNK.INC'
      INCLUDE 'D0$LINKS:IZL0AD.LINK'
C
      INTEGER LKL0ADH, LKL0AD, IVERS
      INTEGER IXL0AD
      INTEGER IBUNCH
      INTEGER GZLV0H, ISETVN
      EXTERNAL GZLV0H, ISETVN
C
      CHARACTER*80 VARMSG
      CHARACTER*4  PATH
C
      LOGICAL FIRST
C
      SAVE FIRST, IVERS, IXL0AD
      DATA IVERS /0/
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
C ****  Assemble form of L0AD
C
      IF ( FIRST ) THEN     ! Only for first event
        CALL MZFORM( 'L0AD',' 323I ',IXL0AD)
        CALL L0_GET_VERSION(IVERS)
        IF ( IVERS.NE.2 ) IVERS=1
        FIRST=.FALSE.
      ENDIF
C
C ****  Get name of the bank to hang on, 'GEAN' or 'RECO'
C
      IF ( LHEAD.EQ.0 ) THEN
        WRITE (VARMSG,10)
   10   FORMAT('  **BKL0AD  ** Cannot book L0AD because HEAD bank',
     &    ' does not exist')
        CALL ERRMSG('LEVEL0-LHEAD=0','BKL0AD',VARMSG,'F')
      ENDIF
C
C ****  Book LV0H bank if needed
C
      LLV0H=GZLV0H(0)
      IF ( LLV0H.LE.0 ) CALL BKLV0H(LLV0H)
      IF ( LLV0H.LE.0 ) GOTO 999
C
C ****  Book L0AD bank and store bank lengths inside if needed
C
      CALL MZBOOK(IXMAIN,LL0AD,LLV0H,-IZL0AD,'L0AD',0,0,323,IXL0AD,0)
      LKL0AD=LL0AD
      IQ(LKL0AD) = ISETVN ( IQ(LKL0AD), IVERS)    ! Version Number
      IQ(LKL0AD+1)= IBUNCH                        ! bunch number
      IQ(LKL0AD+2)= 80                            ! number of channels
      IQ(LKL0AD+3)= 4                             ! words per channel
C----------------------------------------------------------------------
  999 RETURN
      END
