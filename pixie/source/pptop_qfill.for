      SUBROUTINE PPTOP_QFILL(ARRAY,IARRAY,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill lego plot array from ISAQ
C-
C-   Inputs  : none
C-   Outputs : ARRAY - ET in ETA-PHI bins
C-             IARRAY - ID code for bins
C-                      code=4 for W+ from t (ISAJ_ID=80)
C-                      code=2 for b from  t (IDAJ_ID=6)
C-                      code=3 for W- from tbar (ISAJ_ID=-80)
C-                      code=4 for bbar from tbar (ISAJ_ID=-6)
C-                      code=5 for IR (ISAJ_ID=0)
C-             IOK - 0 if banks are OK, 1 if do not exist
C-
C-   Created  26-APR-1993   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS/LIST'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER IARRAY(NPHIL,2*NETAL),J
      INTEGER IT,IOK
      INTEGER JP,JE,JP1,JE1,IRAT     !! 23-12-91
      INTEGER IER,GZISAQ,LISAJ,LISAQ,ISAJ_ID,ISAQ_ID
      REAL    ARRAY(NPHIL,2*NETAL),PTMIN,P(4),PTOT
      REAL    ETMIN,THETA,TWOP,PHII,PHIS
      REAL    ETA,PHI,ET
      REAL    XI,YI,ZI
      INTEGER IP,IETA,NJGOOD
      INTEGER IDP,IDE
      INTEGER ICOL(6)
      CHARACTER*3  UCOL
C---------------------------------------------------------------------
      CALL VZERO(ARRAY,NPHIL*2*NETAL)
      CALL VZERO(IARRAY,NPHIL*2*NETAL)
      IOK=0
      TWOP=TWOPI
      CALL PUGETA('TOPDIS WCOL',UCOL)
      CALL PXCOLCTOI(UCOL,ICOL(1))
      CALL PUGETA('TOPDIS BCOL',UCOL)
      CALL PXCOLCTOI(UCOL,ICOL(2))
      CALL PUGETA('TOPDIS WBCOL',UCOL)
      CALL PXCOLCTOI(UCOL,ICOL(3))
      CALL PUGETA('TOPDIS BBCOL',UCOL)
      CALL PXCOLCTOI(UCOL,ICOL(4))
      CALL PUGETA('TOPDIS IRCOL',UCOL)
      CALL PXCOLCTOI(UCOL,ICOL(5))
      CALL PUGETA('TOPDIS UNCOL',UCOL)
      CALL PXCOLCTOI(UCOL,ICOL(6))
C-
C--- Start ISAQ loop...
C-
      LISAQ = GZISAQ()
C-
C--- Start ISAQ loop...
  200 IF (LISAQ .EQ. 0)              GO TO 900
      P(1) =  Q(LISAQ+2)
      P(2) =  Q(LISAQ+3)
      P(3) =  Q(LISAQ+4)
      ETA =  Q(LISAQ+9)
      PHI =  Q(LISAQ+7)
      PTOT = SQRT ( P(1)**2 + P(2)**2 + P(3)**2 )
      ET   = SQRT ( P(1)**2 + P(2)**2 )
      IF (ET.LT. PTMIN) GOTO 700
C-   Top colors
      LISAJ = LQ(LISAQ-1)
      ISAJ_ID = 0
      IF(LISAJ.GT.0) ISAJ_ID = IQ(LISAJ+1)
      ISAQ_ID = IQ(LISAQ+1)
C-   Plot track
      IETA=(ETA+3.7)*10.+1.
      IP=(PHI/TWOPI)*64 +1
      IETA=MAX(1,IETA)
      IETA=MIN(2*NETAL,IETA)
      IF(ISAJ_ID.EQ.80) THEN
        IF(ARRAY(IP,IETA).LT.ET) IARRAY(IP,IETA) =ICOL(1) ! W+ from t quark
      ELSE IF(ISAJ_ID.EQ.6) THEN
        IF(ARRAY(IP,IETA).LT.ET) IARRAY(IP,IETA) =ICOL(2) ! b quark from t
      ELSE IF(ISAJ_ID.EQ. -80) THEN
        IF(ARRAY(IP,IETA).LT.ET) IARRAY(IP,IETA) =ICOL(3) ! W- from t bar
      ELSE IF(ISAJ_ID.EQ. -6) THEN
        IF(ARRAY(IP,IETA).LT.ET) IARRAY(IP,IETA) =ICOL(4) ! b bar from t bar
      ELSE IF(ISAQ_ID.NE.0) THEN
        IF(ARRAY(IP,IETA).LT.ET) IARRAY(IP,IETA) =ICOL(5)
      ELSE
        IF(ARRAY(IP,IETA).LT.ET) IARRAY(IP,IETA) = ICOL(6)
      END IF
      ARRAY(IP,IETA) = ARRAY(IP,IETA) + ET
C
  700 LISAQ = LQ(LISAQ)
      GO TO 200
  900 CONTINUE
C-
  999 RETURN
      END
