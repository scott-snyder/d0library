      SUBROUTINE BKPLV0(LKPLV0)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the PLV0 Zebra Bank.  It contains the
C-                         processed hit and z vertex information from
C-                         the Level 0 detector.
C-
C-   Inputs  : none
C-   Outputs : LKPLV0 = Link to bank
C-   Controls: none
C-
C-   Created  20-JUL-1992   Jeffrey Bantly - Version 0
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:LV0LNK.INC'
      INCLUDE 'D0$LINKS:IZPLV0.LINK'
      INTEGER IXPLV0, LPROC, LKPLV0, IVERS
      INTEGER GZPROC, GZPLV0, ISETVN
      EXTERNAL GZPROC, GZPLV0, ISETVN
C
      CHARACTER*80 VARMSG
      CHARACTER*4 PATH
C
      LOGICAL FIRST
C
      SAVE FIRST, IVERS, IXPLV0
      DATA IVERS /0/
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
C ****  Assemble form of PLV0
C
      IF (FIRST) THEN       ! Only for first event
        CALL MZFORM( 'PLV0', '1B 5F 4I 3B 7F',IXPLV0)
        CALL L0_GET_VERSION(IVERS)
        IF ( IVERS.NE.2 ) IVERS=1
        FIRST=.FALSE.
      ENDIF
C
C ****  Book PROC bank, if needed
C
      LPROC = GZPROC(0)
      IF (LPROC.LE.0) CALL BKPROC(LPROC)
C
C ****  Get name of the bank to hang on, 'GEAN' or 'RECO'
C
      IF ( LPROC.EQ.0 ) THEN
        WRITE (VARMSG,10)
   10   FORMAT('  ** BKPLV0 ** Cannot book PLV0 because PROC bank',
     &    ' does not exist')
        CALL ERRMSG('LEVEL0-LPROC=0','BKPLV0',VARMSG,'F')
      ENDIF
C
C ****  Book PLV0 bank and store hit bank lengths inside if needed
C
      LKPLV0 = GZPLV0()
      IF ( LKPLV0.EQ.0 ) THEN
        CALL MZBOOK(IXMAIN,LPLV0,LPROC,-IZPLV0,'PLV0',0,0,20,IXPLV0,0)
        LKPLV0 = LPLV0
        IQ(LKPLV0) = ISETVN( IQ(LKPLV0), IVERS)   ! Version Number
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
