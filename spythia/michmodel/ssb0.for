      COMPLEX FUNCTION SSB0(QSQ,M1,M2)
      implicit none
      COMPLEX SSF0
      REAL QSQ,M1,M2
      include 'D0$SPYTHIA$INC:SSINF.INC'

      SSB0=XLAM*(1.0,0.0)-SSF0(QSQ,M1,M2)
      RETURN
      END
