      INTEGER FUNCTION READPF()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read and return number of PF key struck
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PF1,POS1
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER ISTAT,LIBKEY,TCODE
C----------------------------------------------------------------------
      PF1=0
      ISTAT=LIBKEY(KEYID,MAINID,TCODE)
      IF(MOD(ISTAT,2).NE.0) THEN
        CALL CHKCOM(TCODE,PF1,POS1,1,1)
        IF(TCODE.EQ.270) THEN           !ENTER key used
          CALL REPSCR
        ENDIF
      ELSEIF(ISTAT.NE.44.AND.TCODE.NE.26) THEN  !ABORT signalled from BROAST
C                                               !or CTRL/Z hit 
        CALL MSGSCR(ISTAT,'READ_KEYSTROKE-->')
      ENDIF
      READPF=PF1
      RETURN
      END
