C----------------------------------------------------------------------
      SUBROUTINE PRT0RW( LUN, IFADC )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print bank T0RW
C-
C-   Inputs  : LUN    [I] : The fortran output unit
C-             IFADC  [I] : Bank (FADC) number; if 0 then all banks are
C-                          printed
C-   Outputs : Dump T0RW bank(s) on the specified unit
C-
C-   Created   3-MAY-1992   Gregory L. Landsberg
C-   Updated  27-Jan-1996   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INTEGER       LUN, IFADC
      INTEGER       LT0RW, GZT0RW, I, J, ISTR, IEND, K, L, LP
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
C
      LT0RW = GZT0RW(IFADC)
      IF (LT0RW .EQ. 0) THEN
        WRITE (LUN,'(''0PRT0RW: no T0RW bank for this event'')')
        RETURN
      ENDIF
C
      IF ((IFADC.LT.0) .OR. (IFADC.GT.20)) THEN
        WRITE (LUN,'(''0PRT0RW: Invalid bank number '',I6)') IFADC
        RETURN
      ELSE IF ( IFADC .EQ. 0 ) THEN
        ISTR = 1
        IEND = 20
      ELSE
        ISTR = IFADC
        IEND = IFADC
      ENDIF
C
      DO K = ISTR,IEND
        LT0RW = GZT0RW(K)
        IF (LT0RW .GT. 0) THEN
          WRITE(LUN,1000) IQ(LT0RW-5),(IQ(LT0RW+I),I=-1,2)
          J = 2 + LT0RW
          DO I = 1,IQ(LT0RW+2)
            LP = IAND(IQ(J+1),mask_0000FFFF)
            WRITE(LUN,1010) I,IQ(J+1)/65536,LP
            DO L=1,LP/4
              WRITE(LUN,1020) IAND(IQ(J+L+1),mask_000000FF),
     &                        IAND(IQ(J+L+1),mask_0000FF00)/256,
     &                        IAND(IQ(J+L+1),mask_00FF0000)/65536,
     &                        ISHFTC(IAND(IQ(J+L+1),mask_FF000000),8,32)
            ENDDO
            J = J+LP/4+1
          ENDDO
        ENDIF
      ENDDO
C
      RETURN
C----------------------------------------------------------------------
 1000 FORMAT('0Dump of the T0RW bank number ',I2,':',/,
     &       ' Length of bank = ',I3,'. Status = ',Z8.8,/,
     &       ' CDD1 label = ',Z4.4,'. Number of pulses = ',I2)
 1010 FORMAT('0Pulse number ',I2,':',/,
     &       ' First bin = ',I3,'; length = ',I3)
 1020 FORMAT(4(1X,I3,','))
      END
