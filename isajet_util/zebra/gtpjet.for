C DEC/CMS REPLACEMENT HISTORY, Element GTPJET.FOR
C *3    30-JAN-1990 17:51:54 SERBAN "use GZPJET"
C *2    22-DEC-1989 12:10:48 CSTEWART "Chip Stewart: GET PJET DATA FROM ZEBRA"
C *1    21-DEC-1989 14:40:32 SERBAN "PJET subroutines"
C DEC/CMS REPLACEMENT HISTORY, Element GTPJET.FOR
      SUBROUTINE GTPJET(LSUP,LPJET,ET,P,MASS,PHI,TH,ETA) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Fetch information from next bank in PJET linear structure
C-
C-   Inputs  : 
C-     LSUP= supporting link, should be 0 to get information
C-           from first bank in linear structure, 
C-           and LPJET from preceding bank otherwise.
C-   Outputs : 
C-     LPJET= structural link to PJET providing information
C-     ET   = jet transverse energy
C-     P(4) = 4-momentum (px, py, pz, E)
C-     MASS = jet mass
C-     TH   = theta
C-     PHI  = phi
C-     ETA  = eta (pseudo-rapidity)
C-
C-   Created   7-DEC-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPJET.LINK'
      INTEGER LSUP,LPJET,LQPJET,GZPJET
      REAL    ET,P(4),MASS,TH,PHI,ETA
C----------------------------------------------------------------------
      IF ( LSUP.EQ.0 ) THEN
        LQPJET=GZPJET()
      ELSE
        LQPJET=LQ(LSUP)
      ENDIF
      IF(LQPJET.NE.0) THEN
        ET = Q(LQPJET+2)
        P(1)=Q(LQPJET+3)
        P(2)=Q(LQPJET+4)
        P(3)=Q(LQPJET+5)
        P(4)=Q(LQPJET+6)
        MASS=Q(LQPJET+7)
        PHI=Q(LQPJET+8)
        TH=Q(LQPJET+9)
        ETA=Q(LQPJET+10)
      ENDIF
      LPJET=LQPJET
  999 RETURN
      END
