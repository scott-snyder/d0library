      SUBROUTINE GTCCUA(ICRATE,ICHAN,CAP,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the EFFECTIVE URANIUM AREA for a given 
C-                         channel 
C-
C-   Inputs  : ICRATE   [I]     ADC Crate number
C-             ICHAN    [I]     Channel number to fetch area for,
C-                                ICHAN = 1 - 4608
C-                                ICHAN = ADC*384+ BLS*48+ ROTOW*12 + DEPTH
C-   Outputs : CAP      [I]     EFFECTIVE URANIUM AREA  for channel ICHAN in
C-                              (in cm**2)
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
      INTEGER LCCUA,ICHAN,CAP,IER
      INTEGER ICRT,JCRATE(12),ICRATE,CCUA_LINK(12)
      INTEGER NHEAD,GZCCUA,LINK,LZFIND,PCRATE,POINT
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
        LCCUA = GZCCUA ()
        DO ICRT = 1,12
          CALL STP_GSLINK('CUNPAK',CCUA_LINK(ICRT) )
          LINK = LZFIND(IDVSTP,LCCUA,JCRATE(ICRT),6)
          STP_LSLINK(CCUA_LINK(ICRT)) = LINK
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
      LCCUA = STP_LSLINK(CCUA_LINK(ICRT)) 
      POINT = NHEAD + (ICHAN+1)/2
      JCAP = IC(LCCUA+POINT)
      IF (LCCUA.GT.0 .AND. POINT.LE.IC(LCCUA-1)) THEN
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
