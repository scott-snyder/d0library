      SUBROUTINE PRRTST ( PRUNIT )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of banks 'RTST'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  23-OCT-1995 09:20:30.95  Jadwiga Warchol
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PRUNIT
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZRTST.LINK'
C----------------------------------------------------------------------
      INTEGER LRTST, GZRTST
      INTEGER I, J, NTDATA, IDATA, NTFDATA, LP, L, IPULSE, IFDATA
      INTEGER RADDR
      DATA RADDR /'1C0D'X/
C----------------------------------------------------------------------
      LRTST = GZRTST()
      IF ( LRTST .LE. 0 ) THEN
        WRITE( PRUNIT, 1005 )
 1005   FORMAT(/' No RTST bank FOUND')
        GOTO 999
      ENDIF
C
C  ***  Print the content of the bank pointed by LRTST
C

      WRITE( PRUNIT, 1006 )  IQ( LRTST - 1)
      NTDATA = IQ( LRTST - 1) - 3
 1006 FORMAT(1X,'Number of data words in RTST bank=',I8)
      WRITE( PRUNIT, 1007 ) ( IQ( LRTST + J ), J=1,3 )
 1007 FORMAT(1X,'Number of data words in FADCs:',3I8)
C
      I = LRTST + 3
      IDATA = 1
      DO J = 1,3
        NTFDATA = IQ( LRTST + J )
        IF( NTFDATA .LT. 1) GOTO 10
        WRITE(PRUNIT,1008) RADDR+J-1
 1008   FORMAT (1X,'FADC label',5X,Z4.4)
        IFDATA = 1
        IPULSE = 1
    9   CONTINUE
        LP = IAND(IQ(I+IDATA),'0000FFFF'X)
        WRITE(PRUNIT,1009) IPULSE,IQ(I+IDATA)/65536,LP
        IDATA = IDATA + 1
        IFDATA = IFDATA + 1
        DO L=1,LP/4
          WRITE(PRUNIT,1010) IAND(IQ(I+IDATA),'000000FF'X),
     &                    IAND(IQ(I+IDATA),'0000FF00'X)/256,
     &                    IAND(IQ(I+IDATA),'00FF0000'X)/65536,
     &                    ISHFTC(IAND(IQ(I+IDATA),'FF000000'X),8,32)
          IDATA = IDATA + 1
          IFDATA = IFDATA + 1
          IF( IFDATA .GT. NTFDATA) GOTO 10
          IF( IDATA .GT. IQ(LRTST -1)) GOTO 11
        ENDDO
        IPULSE = IPULSE + 1
        GOTO 9
   10   CONTINUE
      ENDDO
   11 CONTINUE
 1009 FORMAT(1X,'Pulse number ',I2,':',/,
     &       ' First bin = ',I3,'; length = ',I3)
 1010 FORMAT(4(1X,I4))
C
  999 RETURN
      END
