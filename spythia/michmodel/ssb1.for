      COMPLEX FUNCTION SSB1(S,MI,MJ)
      implicit none
      REAL S,MI,MJ,A0MI,A0MJ
      COMPLEX SSB0
      include 'D0$SPYTHIA$INC:SSINF.INC'

      IF(S.GT.1.E-4*(MI**2+MJ**2)) THEN
        IF(MI.GE.1.E-10) THEN
          A0MI = MI**2*( 1. - LOG(MI**2) + XLAM )
        ELSE
          A0MI = 0.0
        ENDIF
        IF(MJ.GE.1.E-10) THEN
          A0MJ = MJ**2*( 1.0 - LOG(MJ**2) + XLAM )
        ELSE
          A0MJ = 0.0
        ENDIF
        SSB1 = ( (S+MI**2-MJ**2)*SSB0(S,MI,MJ) + A0MJ - A0MI )/2./S
      ELSE IF(ABS(MI-MJ).GT.1.E-4*MJ) THEN
        IF (MI.GT.0.0) THEN
        SSB1 = -(LOG(MJ)*(MJ**4-2.*MJ**2*MI**2) + MI**4*LOG(MI)
     $    -MJ**4/4. -.75*MI**4 + MI**2*MJ**2) / (MI**2-MJ**2)**2
     $    + XLAM/2
        ELSE
          SSB1 = -(LOG(MJ) - .25)
        ENDIF
      ELSE IF(MI.NE.0.) THEN
        SSB1 = -LOG(MI) + XLAM/2.
      ENDIF
      RETURN
      END
