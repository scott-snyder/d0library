C++++	This is Aprxeq.Inc
C
C		It contains arrays for the first approximations
C		which are to be equivalenced into /WAMCOM/
C
      COMMON  /APRXEQ/ WSAPR, DIRC, P, PDOTN, 
     1         IT, IVFAIL, LICON, LNMISS, LNOTG, LTQUAL, LVQUAL, NMISST
      REAL*8            WSAPR(25)
      REAL*8            DIRC(5,5),      P(5),           PDOTN(5)
      INTEGER*4         IT,             IVFAIL,         LICON(18),
     1  LNMISS(6),      LNOTG(5),       LTQUAL(18),     LVQUAL(6),
     2  NMISST
C
C----	End of Aprxeq.Inc
