C  COMMON ENETRD:CONTAINS THE TOTAL ENERGIES DEPOSITED IN THE TRD
      INTEGER NMSH
      PARAMETER (NMSH=50)
      COMMON/ENETRD/EGENRX,EGENRD,EGRDRI,EGSDRI, EGENRT,EGENST,
     +ETOTWI(10),IHITWN(10),IHITSN(NMSH),ETOTSN(NMSH)
      REAL          EGENRX,EGENRD,EGRDRI,EGSDRI, EGENRT,EGENST,
     +ETOTWI                            ,ETOTSN
      INTEGER    IHITWN    ,IHITSN

C  ----------------------------------------------------------------|
C  EGENRX   = Total Unsmeared generated X RAYS energy              |
C  EGENRD   = Total Unsmeared generated DELTAS energy              |
C  EGENRT   = Total Unsmeared generated        energy              |
C  EGENST   = Total   Smeared generated        energy              |
C  EGRDRI   = Total Unsmeared generated        energy in Drift zone|
C  ETOTWI(I)= Total   Smeared generated  energy on I th hit wire   |
C  IHITWN(I)= Wire number for                      I th hit wire   |
C  ETOTSN(I)= Total   Smeared generated  energy on I th hit strip  |
C  IHITSN(I)= Strip number for                     I th hit strip  |
C  ----------------------------------------------------------------|
C-   Updated  19-NOV-1988   JRH/AZ
