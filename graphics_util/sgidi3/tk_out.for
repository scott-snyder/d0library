      SUBROUTINE TK_OUT(ARRAY,NBYTES)
C  Write NBYTES from ARRAY to Textronix output device.
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      CHARACTER*1 ARRAY(NBYTES)
C  DELAY FACTOR (SECONDS PER BYTE) FOR NETWORK OPERATIONS
C  THIS IS NEEDED TO AVOID PILEUP AT THE TERMINAL DUE TO LONG
C  ^S/^Q PROPAGATION DELAYS WHEN SETTING HOST.
      DATA DELFAC/.001/
      IF(IDV.EQ.2) THEN
C  TK_ HARDCOPY: WRITE TO DISK FILE (UNIT OPENED IN JDEVON)
        IF(NBYTES.GT.0) WRITE(IDRUNI)ARRAY
        RETURN
      ELSEIF(IDV.EQ.3) THEN
C  TERMINAL OUTPUT.
C!!!MAY NEED A WAIT HERE.
        IF(NBYTES.GT.0) THEN
          DO I=1,NBYTES
            WRITE(6,10) ARRAY(I)
   10       FORMAT(1H+,A1,$)
          ENDDO
        ENDIF
      ENDIF
      END
