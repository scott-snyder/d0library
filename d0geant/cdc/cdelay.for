      SUBROUTINE CDELAY(IADR, X, Z, TIMPOS, TIMNEG, ERTPOS, ERTNEG)
C======================================================================
C
C   Purpose and Methods : Give the time and the pulse height at one
C                         end of the delay line
C
C   Inputs  :     IADR : sense wire #
C                     X : Local drift distance
C                     Z : Local Z
C
C   Outputs :     TIMPOS : time at +Z end of the DL
C                 TIMNEG : time at -Z end of the DL
C                 ERTPOS : Error on TIMPOS
C                 ERTNEG : Error on TIMNEG
C
C   Created  30-MAR-1987   Ghita Rahal-Callot
C-   Updated  11-FEB-1988   Ghita Rahal-Callot  : Various modifications,
C-                                                and add the errors
C
C======================================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDRFT.LINK'
      INCLUDE 'D0$INC:GCUNIT.INC'
C
      INTEGER LDTMD, LDTMW, LPOS, LNEG
      REAL X, Z, TIMPOS, TIMNEG, ERTPOS, ERTNEG
      REAL   DRFT, ERDRFT, DRTVEL, OFFS
      INTEGER IZP, IZN, IADR(*)
C======================================================================
      IZP = 0
      IF ( IADR(3) .GE. 5 ) IZP = 2
      IZN = IZP + 1
C
C ****  Calculate drift time
C
      CALL CDRIFT ( X, IADR, DRFT, ERDRFT )
      LDTMW = LC ( LDTMH - IADR(1) - 1 )
      IF ( LDTMW .LE. 0 ) THEN
        WRITE(LOUT,*)'**** CDRIFT: Bank DTMW not defined'
        CALL EXIT(1)
      ENDIF
      LDTMW = LDTMW + (IADR(2)*IC(LDTMW+4) + IADR(3))*IC(LDTMW+3) + 4
C
C ****  Suppress the offset on the Drift time
C
      DRFT  = DRFT - C ( LDTMW + 1 )
C
C ****  Calculate Delay Line time
C
      LDTMD = LC ( LDTMH - 5 - IADR(1) )
      IF ( LDTMD .LE. 0 ) THEN
        WRITE ( LOUT, * ) '**** CDELAY : bank DTMD not defined'
        CALL EXIT(1)
      ENDIF
      LPOS = LDTMD + (IADR(2)*IC(LDTMD+4)+IZP)*IC(LDTMD+3)+4
      OFFS = C(LPOS + 1)
      DRTVEL = C ( LPOS + 2)
      TIMPOS = Z  / DRTVEL + OFFS + DRFT
      ERTPOS = TIMPOS * ERDRFT / DRFT
      LNEG = LDTMD + (IADR(2)*IC(LDTMD+4)+IZN)*IC(LDTMD+3)+4
      OFFS = C(LNEG + 1)
      DRTVEL = C ( LNEG + 2)
      TIMNEG = Z  / DRTVEL + OFFS + DRFT
      ERTNEG = TIMNEG * ERDRFT / DRFT
  999 RETURN
      END

