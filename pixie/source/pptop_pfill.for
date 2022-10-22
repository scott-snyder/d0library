      SUBROUTINE PPTOP_PFILL(ARRAY,IARRAY,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill lego plot array from ISP1
C-
C-   Inputs  : none
C-   Outputs : ARRAY - ET in ETA-PHI bins
C-             IARRAY - ID code for bins
C-                      code=4 for W+ from t (ISAJ_ID=80)
C-                      code=2 for b from  t (IDAJ_ID=6)
C-                      code=3 for W- from tbar (ISAJ_ID=-80)
C-                      code=4 for bbar from tbar (ISAJ_ID=-6)
C-                      code=5 for IR (ISAJ_ID=0)
C-                      code=6 underlying event
C-             IOK - 0 if banks are OK, 1 if do not exist
C-
C-   Created  10-SEP-1991   Sharon Hagopian
C-   Updated  13-JAN-1992   j.f. Det÷uf: jet size coded in mod(iarray,20)
C-                           ratio EM/ET coded in iarray/100
C-   Updated  24-FEB-1992   S. Hagopian, Interactive variable DRAWTK added
C-   Updated  17-MAR-1992   Nobu. Oshima - Modified for new JETS Bank.
C-   Updated  26-SEP-1992   Nobu. Oshima - Use GTJETS and GTJETS_NOEP.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS/LIST'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INTEGER IARRAY(NPHIL,2*NETAL),J
      INTEGER IT,IOK
      INTEGER JP,JE,JP1,JE1,IRAT     !! 23-12-91
      INTEGER IER,LISP1,GZISP1,LISV1,GZISV1,LISAJ,LISAQ,ISAJ_ID,ISAQ_ID
      REAL    ARRAY(NPHIL,2*NETAL),PTMIN,P(4),PTOT
      REAL    ETMIN,THETA,TWOP,PHII,PHIS
      REAL    ETA,PHI,ET
      REAL    XI,YI,ZI
      INTEGER IP,IETA,NJGOOD,NONU
      INTEGER IDP,IDE,ID
      INTEGER ICOL(6)
      CHARACTER*3  UCOL
      LOGICAL LNU
C---------------------------------------------------------------------
      CALL VZERO(ARRAY,NPHIL*2*NETAL)
      CALL VZERO_i(IARRAY,NPHIL*2*NETAL)
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
      CALL PUGETV('TOPDIS NEUTRINOS',NONU)
      LISV1 = GZISV1()
      IF (LISV1 .LE. 0) THEN
        CALL ERRMSG('NO ISV1','PPTOP_PFILL',
     &              'ISV1 bank does not exist','W')
        GO TO 999
      ENDIF
C-
C--- Start ISP1 loop...
C-
  100 IF (LISV1 .EQ. 0)             GO TO 900
      LISP1 = LQ(LISV1-IZISP1)
C-
C--- Start ISP1 loop...
  200 IF (LISP1 .EQ. 0)              GO TO 800
      ID =  ABS(IQ(LISP1+1))
      LNU = (ID.EQ.11).OR.(ID.EQ.13).OR.(ID.EQ.15)
      IF((NONU.EQ.0).AND.LNU) GOTO 700
      P(1) =  Q(LISP1+2)
      P(2) =  Q(LISP1+3)
      P(3) =  Q(LISP1+4)
      ETA =  Q(LISP1+9)
      PHI =  Q(LISP1+7)
      PTOT = SQRT ( P(1)**2 + P(2)**2 + P(3)**2 )
      ET   = SQRT ( P(1)**2 + P(2)**2 )
      IF (ET.LT. PTMIN) GOTO 700
C-   Top colors
      LISAJ = LQ(LISP1-3)
      LISAQ = LQ(LISP1-2)
      ISAJ_ID = 0
      ISAQ_ID = 0
      IF(LISAJ.GT.0) ISAJ_ID = IQ(LISAJ+1)
      IF(LISAQ.GT.0) ISAQ_ID = IQ(LISAQ+1)
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
      ELSE IF(ISAQ_ID.GT.0) THEN
        IF(ARRAY(IP,IETA).LT.ET) IARRAY(IP,IETA) =ICOL(5)
      ELSE
        IF(ARRAY(IP,IETA).LT.ET) IARRAY(IP,IETA) = ICOL(6)
      END IF
      ARRAY(IP,IETA) = ARRAY(IP,IETA) + ET
C
  700 LISP1 = LQ(LISP1)
      GO TO 200
  800 LISV1 = LQ(LISV1)
      GO TO 100
  900 CONTINUE
C-
  999 RETURN
      END
