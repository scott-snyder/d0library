      SUBROUTINE TB90_INSPILL_BAD(ICRD,CHN,PED,SIG,NBADFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compares inspill peds to database peds
C-                         then sets bad channel flag, if appropriate
C-
C-   Inputs  : ICRD - ADC card number 0-11
C-             CHN  - Channel number 1-384
C-             PED  - Inspill pedestal for channel chan
C-             SIG  - Inspill sigma for channel chan
C-
C-   Outputs : NBADFL - Bad channel flag
C-                      0 - good channel
C-                      1 - mean too small (<100)
C-                      2 - mean too large (>500)
C-                      3 - mean different from double-digitized mean
C-                      4 - sigma different from double-digitized sigma
C-                      5 - mean and sigma different from double-digitized sigma
C-
C-   Controls: 
C-
C-   Created  25-OCT-1990   Jan Guida
C-   Updated  21-JAN-1991   Jan Guida  If returning because of error, reset
C-                                      LCPDH variable 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSCAL.LINK'
      INCLUDE 'D0$LINKS:IZCPDH.LINK'
      REAL PED,SIG,VAL(2)
      INTEGER CHN,ICRD,IBLS,ITWR,IDEP,IADDR
      INTEGER NBADFL,CRATE
      INTEGER KSTPC,KSCAL,LNKOLD
C----------------------------------------------------------------------
      LNKOLD = LCPDH
      KSTPC = LC(LSTPH-IZSTPC)
      KSCAL = LC(KSTPC-IZSCAL)
      LCPDH = LC(KSCAL-IZCPDH)
      CRATE = IC(LCPDH+9)
C
      NBADFL = 0
      IF (PED.LT.100.) THEN
        NBADFL = 1
        GO TO 999
      ENDIF
      IF (PED.GT.500.) THEN
        NBADFL = 2
        GO TO 999
      ENDIF
      IBLS = (CHN-1)/48
      ITWR = ((CHN -1) - IBLS*48)/12
      IDEP = (CHN -1) - IBLS*48 - ITWR*12
      CALL CADPAK(ICRD,IBLS,ITWR,IDEP,0,0,IADDR)
      CALL GT_PED_GNS1(1,CRATE,IADDR,VAL)
      IF (ABS(VAL(1)-PED) .GT. 2.5*VAL(2)) NBADFL =3
      IF (ABS(VAL(2)-SIG) .GT. 4.0) THEN
        IF (NBADFL.EQ.3) THEN
          NBADFL = 5
        ELSE
          NBADFL = 4
        ENDIF
      ENDIF
C
  999 LCPDH=LNKOLD
C
      RETURN
      END
