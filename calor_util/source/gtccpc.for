      SUBROUTINE GTCCPC(ICRATE,ICHAN,CAP,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the CELL capacitance value for a given 
C-                         channel.
C-
C-   Inputs  : ICRATE   [I]     ADC Crate number
C-             ICHAN    [I]     Channel number to fetch capacitance for,
C-                                ICHAN = 1 - 4608
C-                                ICHAN = ADC*384+ BLS*48+ ROTOW*12 + DEPTH
C-   Outputs : CAP      [I]     Cell capacitance for channel ICHAN in
C-                              picofarads
C-             IER      [I]     equals 0 if no error
C-   Controls: None
C-
C-   Created  31-JUL-1991   Jan Guida, Chip Stewart
C-   Updated  17-Mar-1992   Herbert Greenlee
C-      Fix byte order
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER LCCPC,ICHAN,CAP,IER
      INTEGER ICRT,JCRATE(12),ICRATE,CCPC_LINK(12)
      INTEGER NHEAD,GZCCPC,LINK,LZFIND,PCRATE,POINT
      INTEGER JCAP
      INTEGER*2 ICAP(2)
      EQUIVALENCE (JCAP,ICAP(1))
      LOGICAL FIRST
      DATA JCRATE/7,17,27,37,47,57,8,18,28,38,48,58/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IER = 0
      IF (FIRST) THEN
C
C ****  RESERVE LINK IN STP_ZLINKA FOR LINK TO NCAD,NCRATE
C
        LCCPC = GZCCPC ()
        DO ICRT = 1,12
          CALL STP_GSLINK('CUNPAK',CCPC_LINK(ICRT) )
          LINK = LZFIND(IDVSTP,LCCPC,JCRATE(ICRT),6)
          STP_LSLINK(CCPC_LINK(ICRT)) = LINK
        END DO
        PCRATE = -1
        NHEAD = 7
        FIRST = .FALSE.
      ENDIF
C
      IF(ICRATE.NE.PCRATE) THEN
        ICRT = (ICRATE/10 + 1) +6*(MOD(ICRATE,10)-7)
        PCRATE = ICRATE
      END IF
C
      LCCPC = STP_LSLINK(CCPC_LINK(ICRT)) 
      POINT = NHEAD + (ICHAN+1)/2
      JCAP = IC(LCCPC+POINT)
      IF (LCCPC.GT.0 .AND. POINT.LE.IC(LCCPC-1)) THEN
        IF (MOD(ICHAN,2).EQ.0) THEN       ! EVEN NUMBER
          CAP = ICAP(WORD2)
        ELSE                              ! ODD NUMBER
          CAP = ICAP(WORD1)
        ENDIF
      ELSE                              ! ZEBRA POINTER ERROR
        IER = -1
      ENDIF
C
  999 RETURN
      END
