C TRHITW.INC
C---------------------------------------------
C-
C-   Created  13-JAN-1988   A. ZYLBERSTEJN
C-   Updated  24-NOV-1988   A. ZYLBERSTEJN Modified to accomodate cathode
C-                                                                strips
C-   Updated  29-OCT-1993   A. Zylberstejn  Accomodate for 512 channels 
C-
C---------------------------------------------
      INTEGER NMWHIT
      COMMON /TRHITW/ NBTHIT(3,2),NUMTWH(NTOT_WIRE_TRD),
     &  ENTWH(NTOT_WIRE_TRD),first_info(NTOT_WIRE_TRD)
      INTEGER         NBTHIT,first_info,NUMTWH          
      REAL                      ENTWH
C
C  NBTHIT(I,K)    = NB. OF DIFFERENT WIRES IN TRD LAYER I
C  NUMTWH(J,I,K)  = WIRE NUMBER FOR THE J th HIT IN LAYER I
C  ENTWH (J,I,K)  = TOTAL ENERGY ""     ""    ""     ""
C   K=1 anodes     K=2 cathodes strip
