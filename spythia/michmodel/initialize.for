      SUBROUTINE initialize

C       This subroutine is called to initialize the common blocks in the
C       include file D0$SPYTHIA$INC:VARS.INC. It initializes all Standard Model parameters
C       to the accepted values (couplings evaluated at Mw) and reads in
C       from a file MODEL.CHO the choice of Ross models (X or Z) for the
C       SUSY-GUT parameters. If MODEL.CHO contains the character 'A' then
C       the basic parameters will be read from a separate input file MODEL$IN.
C       All mass units are in GeV.

      IMPLICIT NONE
      INCLUDE 'D0$SPYTHIA$INC:VARS.INC'

      mq(1)= 5.6e-03
      mq(2)= 9.9e-03
      mq(3)= 1.35
      mq(4)= 199.e-03
      mq(5)= 135.         ! modify top mass as desired. This is set
                          ! to topmass in main.f
      mq(6)= 4.90

      ml(1)= 0.
      ml(2)= 0.511e-03
      ml(3)= 0.
      ml(4)= 105.65e-03
      ml(5)= 0.
      ml(6)= 1.784

      mw= 80.6
      mz= 91.176

      sw2= 0.2336
      alphaw= 1./127.9                    !fine structure const at Mw
      alpha(1)=(5./3.)*alphaw/(1.-sw2)    !U(1)y const at Mw (normed at Mx)
      alpha(2)=alphaw/sw2                 !SU(2) const at Mw (MSbar scheme)
      alpha(3)=0.118                      !SU(3) const at Mz (= at Mw)
      alphax=0.04

      pi=2.*asin(1.0)

      vev=2.*mw/sqrt(4.*pi*alpha(2))

C       Minimal SUSY parameters
C
      msl(7)= 0.                  !i.e., RH sneutrinos don't exist.
      msl(9)= 0.
      msl(11)= 0.

      RETURN
      END
