C----------------------------------------------------------------------
C-
C-   Created  27-APR-1989   A. Zylberstejn
C-
C----------------------------------------------------------------------
      COMMON /TRDWIR/ ADDRWI(0:1983),FADDR(256,6),FADCNB(256,6)
      INTEGER         ADDRWI      ,FADDR       ,FADCNB       
C
C  ADDRWI(J)= WIRE NUMBER +512*(CHAMBER NB. -1) FOR FADC WITH ADDRESS J
C  FADDR (I,ICH)=FADC ADDRESS        FOR WIRE I,CHAMBER ICH
C  FADCNB(I,ICH)=FADC CHANNEL NUMBER FOR WIRE I CHAMBER ICH
