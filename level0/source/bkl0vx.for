      SUBROUTINE BKL0VX( LKL0VX, IBUNCH )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the L0VX (Level 0 VERTEX) Bank
C-
C-   Inputs  : IBUNCH - Bunch number of bank's future contents
C-   Outputs : LKL0VX - Bank link value
C-   Controls: None
C-
C-   Created  18-JUL-1992   Freedy Nang  - Version 0
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:LV0LNK.INC'
      INCLUDE 'D0$LINKS:IZL0VX.LINK'
C
      INTEGER LKL0VX, IVERS
      INTEGER IXL0VX
      INTEGER IBUNCH
      INTEGER GZLV0H, ISETVN
      EXTERNAL GZLV0H, ISETVN
C
      CHARACTER*80 VARMSG
      CHARACTER*4  PATH
C
      LOGICAL FIRST
C
      SAVE FIRST, IVERS, IXL0VX
      DATA IVERS /0/
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
C ****  Assemble form of L0VX
C
      IF ( FIRST ) THEN
        CALL MZFORM('L0VX',' 52I ',IXL0VX)
        CALL L0_GET_VERSION(IVERS)
        IF ( IVERS.NE.2 ) IVERS=1
        FIRST=.FALSE.
      ENDIF
C
C ****  Get name of the bank to hang on, 'GEAN' or 'RECO'
C
      IF ( LHEAD.EQ.0 ) THEN
        WRITE (VARMSG,10)
   10   FORMAT('  **BKL0VX  ** Cannot book L0VX because HEAD bank',
     &    ' does not exist')
        CALL ERRMSG('LEVEL0-LHEAD=0','BKL0VX',VARMSG,'F')
      ENDIF
C
C ****  Book LV0H bank if needed
C
      LLV0H=GZLV0H(0)
      IF ( LLV0H.LE.0 ) CALL BKLV0H(LLV0H)
      IF ( LLV0H.LE.0 ) GOTO 999
C
C ****  Book L0VX bank and store bank lengths inside if needed
C
      CALL MZBOOK(IXMAIN,LL0VX,LLV0H,-IZL0VX,'L0VX',0,0,52,IXL0VX,0)
      LKL0VX = LL0VX
C  set version number and bunch number for both boards
      IQ(LKL0VX)   = ISETVN ( IQ(LKL0VX), IVERS)
      IQ(LKL0VX+1) = IBUNCH
      IQ(LKL0VX+27)= IBUNCH
C----------------------------------------------------------------------
  999 RETURN
      END
