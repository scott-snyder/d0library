C DEC/CMS REPLACEMENT HISTORY, Element GTISJT.FOR
C *2    30-JAN-1990 18:02:48 SERBAN "use GZISJT"
C *1     9-DEC-1988 10:55:33 SERBAN "fetch contents of bank ISJT"
C DEC/CMS REPLACEMENT HISTORY, Element GTISJT.FOR
      SUBROUTINE GTISJT(LSUP,LISJT,EJT,P,MASS,PHI,TH,ETA) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Fetch information from next bank in ISJT linear structure
C-
C-   Inputs  : 
C-     LSUP= supporting link, should be 0 to get information
C-           from first bank in linear structure, 
C-           and LISJT from preceding bank otherwise.
C-   Outputs : 
C-     LISJT= structural link to ISJT providing information
C-     EJT   = jet transverse energy
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
      INTEGER LSUP,LISJT,LQISJT,GZISJT
      REAL    EJT,P(4),MASS,TH,PHI,ETA
C----------------------------------------------------------------------
C
      IF ( LSUP.EQ.0 ) THEN
        LQISJT=GZISJT()
      ELSE
        LQISJT=LQ(LSUP)
      ENDIF
      IF(LQISJT.NE.0) THEN
        EJT=Q(LQISJT+1)
        P(1)=Q(LQISJT+2)
        P(2)=Q(LQISJT+3)
        P(3)=Q(LQISJT+4)
        P(4)=Q(LQISJT+5)
        MASS=Q(LQISJT+6)
        PHI=Q(LQISJT+7)
        TH=Q(LQISJT+8)
        ETA=Q(LQISJT+9)
      ENDIF
      LISJT=LQISJT
  999 RETURN
      END
