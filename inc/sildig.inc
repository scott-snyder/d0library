C
      INTEGER NTMMAX,NDIGTEM,IDIGOFF,IP
      INTEGER NPHMAX,IPHOFF
C maximum number of pulse heights to save in QBPH bank
      PARAMETER (NPHMAX = 15000)
C maximumum number of digitizations/ladder for SIW2 temp bank
      PARAMETER (NDIGTEM = 1280)
C maximum number of tracks to associate to each fired strip
      PARAMETER (NTMMAX = 5)
C row length for SIW2 bank
      PARAMETER (IDIGOFF=3+NTMMAX)
C row length for QBPH bank
      PARAMETER (IPHOFF=1)
C global offset to store row number
      PARAMETER(IP=1)
C
