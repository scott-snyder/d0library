      SUBROUTINE PPJET_LFILL(ARRAY,IARRAY,XPMUO,NTYP,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill lego plot array from ISAE etc.
C-
C-   Inputs  : none
C-   Outputs : ARRAY  - ET in ETA-PHI bins
C-             IARRAY - ID code for bins
C-                      code=9  - pjet
C-                      code=11 - nue
C-                      code=12 - e
C-                      code=13 - num
C-                      code=14 - mu
C-                      code=15 - nut
C-                      code=16 - tau
C-             XPMUO  - Array for mu
C-             NTYP   - Number of objects
C-             IOK - 0 if banks are OK, 1 if do not exist
C-
C-   Created  05-MAR-1994 Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INCLUDE 'D0$LINKS:IZISAQ.LINK'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$LINKS:IZISV1.LINK'
      INCLUDE 'D0$LINKS:IZPJHD.LINK'
      INCLUDE 'D0$LINKS:IZPJET.LINK'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
C-
      INTEGER IARRAY(NPHIL,2*NETAL),NTYP(9),IOK
      INTEGER NJET,NMUO,NELE,NTAU,NPHO,KK,IDD,IDABS
      INTEGER LISAE,LPJHD,LISV1,LISP1,LPJET,LISAQ
      INTEGER IP,IETA
      REAL    ARRAY(NPHIL,2*NETAL),XPMUO(3,50)
      REAL    ETA,PHI,ET,PX,PY
C---------------------------------------------------------------------
      IOK=0
      CALL VZERO(ARRAY,NPHIL*2*NETAL)
      CALL VZERO(XPMUO,150)
      CALL VZERO(IARRAY,NPHIL*2*NETAL)
C- zero variables
      NJET  = 0
      NMUO  = 0
      NELE  = 0
      NTAU  = 0
      DO 3 KK=1,9
    3 NTYP(KK)=0
C-
      LISAE = LQ(LHEAD-IZISAE)
      IF (LISAE .EQ. 0)             GO TO 999
      LISV1  = LQ(LISAE-IZISV1)
      IF (LISV1 .EQ. 0)             GO TO 999
C====== Process PJET ================================================
C-
C--- Start PJET loop...
C-
      LPJHD = LQ(LISAE-IZPJHD)
      IF (LPJHD .EQ. 0)              GO TO 300
C.N.O NJET  = IQ(LPJHD+3)
      LPJET = LQ(LPJHD-IZPJET)
  150 IF (LPJET .EQ. 0)              GO TO 300
      ETA = Q(LPJET+10)
      PHI = Q(LPJET+8)
      ET  = Q(LPJET+2)
      IETA=(ETA+3.7)*10.+1.
      IP=(PHI/TWOPI)*64 +1
      IF(IP.GT.0.AND.IP.LE.NPHIL  .AND.
     +   IETA.GT.0.AND.IETA.LE.2*NETAL) THEN
        NJET = NJET + 1
        ARRAY(IP,IETA)  = ET
        IARRAY(IP,IETA) = 9
      ELSE
        WRITE(6,*)' ERROR - IP= ',IP,' IETA= ',IETA
      ENDIF
C-
      LPJET   =  LQ(LPJET)
      GO TO 150
C-
C====== Process ISAQ ================================================
C-
  300 CONTINUE
      LISAQ = LQ(LISAE-IZISAQ)

C
C--- Start ISAQ loop...
C-
   20 IF (LISAQ .LE. 0)     GO TO 900
      IDD = IQ(LISAQ+1)
      IDABS = ABS(IDD)
      PX   = Q(LISAQ+2)
      PY   = Q(LISAQ+3)
      ETA  = Q(LISAQ+9)
      PHI  = Q(LISAQ+7)
      ET   = SQRT(PX*PX + PY*PY)
C
      IP=(PHI/TWOPI)*64. +1.
      IETA=10.*(ETA+3.7)+1.
      IF(IP.GT.0.AND.IP.LE.NPHIL  .AND.
     &   IETA.GT.0.AND.IETA.LE.2*NETAL) THEN
        IF (IDABS .EQ. 14) THEN
          NMUO = NMUO + 1
          IF(NMUO .LE. 50) THEN
            XPMUO(1,NMUO) = IP
            XPMUO(2,NMUO) = IETA
            XPMUO(3,NMUO) = ET
          ENDIF
        ELSEIF (IDABS .EQ. 10) THEN
          NPHO = NPHO + 1
          IARRAY(IP,IETA) = 10
        ELSEIF (IDABS .EQ. 12) THEN
          NELE = NELE + 1
          IARRAY(IP,IETA) = 12
        ELSEIF (IDABS .EQ. 16) THEN
          NTAU = NTAU + 1
          IARRAY(IP,IETA) = 16
        ENDIF
      ENDIF
C--- GO TO THE NEXT ISAQ BANK
C-
      LISAQ = LQ(LISAQ)
      GO TO 20
  900 CONTINUE
C
      NTYP(2)= NMUO
      NTYP(4)= NELE
      NTYP(5)= NJET
      NTYP(6)= NJET
      NTYP(7)= NPHO
      NTYP(8)= NTAU
C-
  999 CONTINUE
      RETURN
      END
C ===================================================== end pcdst_lfill
