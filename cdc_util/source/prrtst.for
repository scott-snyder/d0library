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
C-   Updated  29-Jan-1996   sss - compile with g77.
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
      DATA RADDR /z'1C0D'/
C
      integer mask_000000ff
      data mask_000000ff / z'000000ff' /
      integer mask_0000ff00
      data mask_0000ff00 / z'0000ff00' /
      integer mask_00ff0000
      data mask_00ff0000 / z'00ff0000' /
      integer mask_ff000000
      data mask_ff000000 / z'ff000000' /
      integer mask_0000ffff
      data mask_0000ffff / z'0000ffff' /
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
        LP = IAND(IQ(I+IDATA),mask_0000FFFF)
        WRITE(PRUNIT,1009) IPULSE,IQ(I+IDATA)/65536,LP
        IDATA = IDATA + 1
        IFDATA = IFDATA + 1
        DO L=1,LP/4
          WRITE(PRUNIT,1010) IAND(IQ(I+IDATA),mask_000000FF),
     &                    IAND(IQ(I+IDATA),mask_0000FF00)/256,
     &                    IAND(IQ(I+IDATA),mask_00FF0000)/65536,
     &                    ISHFTC(IAND(IQ(I+IDATA),mask_FF000000),8,32)
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
