C TRHITW.INC
C---------------------------------------------
C-
C-   Created  13-JAN-1988   A. ZYLBERSTEJN
C-   Updated  24-NOV-1988   A. ZYLBERSTEJN Modified to accomodate cathode
C-                                                                strips
C-
C---------------------------------------------
      INTEGER NMWHIT
      PARAMETER(NMWHIT = 256 )
      COMMON /TRHITW/ NBTHIT(3,2),NUMTWH(NMWHIT,3,2),ENTWH(NMWHIT,3,2)
      INTEGER         NBTHIT
      REAL                      NUMTWH          ,ENTWH
C
C  NBTHIT(I,K)    = NB. OF DIFFERENT WIRES IN TRD LAYER I
C  NUMTWH(J,I,K)  = WIRE NUMBER FOR THE J th HIT IN LAYER I
C  ENTWH (J,I,K)  = TOTAL ENERGY ""     ""    ""     ""
C   K=1 anodes     K=2 cathodes strip
