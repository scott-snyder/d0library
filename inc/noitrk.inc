C----------------------------------------------------------------------
C-
C-   Created   28-JAN-1994   Allen I. Mincer
C-
C----------------------------------------------------------------------
      INTEGER MXLABL
      PARAMETER (MXLABL = 32768)
c
c INISHT = signal event offset and detector type
c
      COMMON/NOISHT/INISHT(0:MXLABL-1,2),NWDS(4),SVERSION(4)
      INTEGER INISHT
c
c NWDS = size of CDDn bank
c
      INTEGER NWDS
c
c SVERSION = type of raw data format
c
      INTEGER SVERSION
c
c signal event CDDn bank origin
c
      COMMON/NOISDD/ NSGDDN(4)
      INTEGER NSGDDN
c
c pileup event offset and detector type
c
      COMMON/NOIPHT/INIPHT(0:MXLABL-1,2),PVERSION(4)
      INTEGER INIPHT
c
c PVERSION = type of raw data format for pileup event
c
      INTEGER PVERSION
c
c pileup event CDDn bank origin
c
      COMMON/NOICDD/ NSCDDN(4)
      INTEGER NSCDDN
