       SUBROUTINE PCCAEH(LSUP,ARRAY,IARRAY,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Select ETA,PHI,ET from CAEH bank
C-                         according to cluster pointers
C-
C-   Inputs  : LSUP - SUPPORT LINK to first cluster header bank (CACL or JETS)
C-   Outputs : ARRAY - ET in ETA-PHI bins
C-             IARRAY - cluster number (bank number) for ETA-PHI bin
C-             IOK - 0 if banks are OK, 1 if do not exist
C-
C-   Modified 12-AUG-1993   C. Stewart - Added ENTRY PCCAEH_PT(NCLUS1,PT1).
C-   Modified 10-JAN-1992   Nobuaki Oshima
C-        Use true eta instead of detector index IETA for ET.
C-   Modified 27-JUN-1990   Nobu(Remove CAEHFL and protect from bad ICD data.)
C-   Created  29-JUN-1989   S. Hagopian
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS/LIST'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER LSUP,LCLUS,IOK
      INTEGER IARRAY(NPHIL,2*NETAL)
      REAL    ET,E(4),PHI,THETA,ETA,PT(100),PT1(100)
      REAL    ARRAY(NPHIL,2*NETAL)
      INTEGER IP,IE,IETA,IL
      INTEGER I,INX
      INTEGER NREP
      INTEGER NCLUS,NCELL,NCLUS1
      INTEGER LHITS
      INTEGER LCAEH,GZCAEH,ICAEH
      INTEGER ICOLOR
C----------------------------------------------------------------------
      LCLUS=LSUP
      IOK=0
      LCAEH=GZCAEH()
      IF(LCAEH.LE.0)GO TO 900
      NREP=IQ(LCAEH+2)
      NCLUS=0
      CALL VZERO(ARRAY,NPHIL*2*NETAL)
      CALL VZERO(IARRAY,NPHIL*2*NETAL)
   20 IF(LCLUS.LE.0)GO TO 999
      LHITS=LQ(LCLUS-1)
      IF(LHITS.LE.0)GO TO 150
      NCELL=IQ(LHITS+2)
      IF(NCELL.LE.0)GO TO 150
      NCLUS=NCLUS+1   
      PT(NCLUS) = Q(LCLUS+6) !JET PT
      IF(IQ(LCLUS-1).EQ.33) PT(NCLUS) = Q(LCLUS+8) ! CACL PT
C-
C--- FILL ET BY TRUE ETA,PHI HERE...
C- 
        DO 100 I=1,NCELL
        INX = LCAEH + NREP*(IQ(LHITS+I+2)-1)
        E(1) = Q(INX+4)
        E(2) = Q(INX+5)
        E(3) = Q(INX+6)
        E(4) = Q(INX+7)
        ET   = Q(INX+8)
        IL   =IQ(INX+14)
        IF (ET .LE. 0.)                     GO TO 100
        IF (IL.LT.MNLYEM .OR. IL.GT.MXLYCH) GO TO 100
        CALL ETOETA(E,PHI,THETA,ETA)
        IF(ETA.GT.3.7)THEN
          ETA=3.7
        ELSE IF(ETA.LT.-3.7)THEN
          ETA=-3.7
        ENDIF
        IE = 10.*ETA
        IP = (PHI/TWOPI)*64 + 1
        IF (IE.LT.-NETAL .OR. IE.GT.NETAL)  GO TO 100
        IF (IP.LT.1 .OR. IP.GT.NPHIL)       GO TO 100
        IF (IE.LT.0) THEN
           IETA = IE + NETAL + 1
        ELSE
           IETA = IE + NETAL
        ENDIF
        ARRAY(IP,IETA) = ARRAY(IP,IETA) + ET
C MAKE COLOR =CLUSTER NUMBER MODULO 14
       ICOLOR=MOD(NCLUS,14)
        IF(ICOLOR.EQ.0)ICOLOR=14   
        IARRAY(IP,IETA)=ICOLOR
  100   CONTINUE
C-
C---
C GO TO THE NEXT BANK
   21 LCLUS=LQ(LCLUS)
      GO TO 20
  150 CONTINUE
      GO TO 999
C error - CAEH BANK does not exist
  900 IOK=1 
  999 RETURN
C-
C---  ENTRY PCCAEH_PT(NCLUS1,PT1)
C-
      ENTRY PCCAEH_PT(NCLUS1,PT1)
      NCLUS1 = MIN(NCLUS,100)
      CALL UCOPY(PT,PT1,NCLUS1)
C-
      RETURN
      END
