       SUBROUTINE BKLV0H( LKLV0H )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the LV0H Bank - Verson 0
C-
C-   Inputs  : None
C-   Outputs : LKLV0H - Bank link value
C-   Controls: None
C-
C-   Created  13-JUL-1992   Freedy Nang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:LV0LNK.INC'
      INCLUDE 'D0$LINKS:IZLV0H.LINK'
      INTEGER IXLV0H, LHITS, LKLV0H, IVERS
      INTEGER GZHITS, GZLV0H, ISETVN
      EXTERNAL GZHITS, GZLV0H, ISETVN
C
      CHARACTER*80 VARMSG
      CHARACTER*4 PATH
C
      LOGICAL FIRST
C
      SAVE FIRST, IVERS, IXLV0H
      DATA IVERS /0/
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
C ****  Assemble form of LV0H
C
      IF (FIRST) THEN       ! Only for first event
        CALL MZFORM( 'LV0H', '10I 10F',IXLV0H)
        CALL L0_GET_VERSION(IVERS)
        IF ( IVERS.NE.2 ) IVERS=1
        FIRST=.FALSE.
      ENDIF
C
C ****  Get name of the bank to hang on, 'GEAN' or 'RECO'
C
      IF ( LHEAD.EQ.0 ) THEN
        WRITE (VARMSG,10)
   10   FORMAT('  ** BKLV0H ** Cannot book LV0H because HEAD bank',
     &    ' does not exist')
        CALL ERRMSG('LEVEL0-LHEAD=0','BKLV0H',VARMSG,'F')
      ENDIF
C
C ****  Book HITS bank, if needed
C
      LHITS = GZHITS(0)
      IF (LHITS.LE.0) CALL BKHITS(LHITS)

C
C ****  Book LV0H bank and store hit bank lengths inside if needed
C
      LKLV0H = GZLV0H()
      IF ( LKLV0H.EQ.0 ) THEN
        CALL MZBOOK(IXMAIN,LLV0H,LHITS,-IZLV0H,'LV0H',4,4,20,IXLV0H,0)
        LKLV0H = LLV0H
        IQ(LKLV0H) = ISETVN( IQ(LKLV0H), IVERS)   ! Version Number
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
