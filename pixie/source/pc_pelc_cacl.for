       SUBROUTINE PC_PELC_CACL(LSUP,NBANK,ARRAY,IARRAY,IOK)
C-
C-   Purpose and Methods : Select ETA,PHI,ET from CAEH bank
C-                         according to cluster pointers
C-
C-   Inputs  : LSUP - SUPPORT LINK to first cluster header bank (CACL or JETS)
C-             NBANK - bank number for PELC
C-   Outputs : ARRAY - ET in ETA-PHI bins
C-             IARRAY - cluster number (bank number) for ETA-PHI bin
C-             IOK - 0 if banks are OK, 1 if do not exist
C-
C-   Created  21-MAY-1990 S. Hagopian (from SUBROUTINE PCCAEH)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS/LIST'
      INTEGER LSUP,LCLUS,IOK,NBANK
      REAL ARRAY(NPHIL,2*NETAL)
      INTEGER IARRAY(NPHIL,2*NETAL)
      REAL ET
      INTEGER IP,IE,IETA,IL
      INTEGER I,INX
      INTEGER NREP
      INTEGER NCLUS,NCELL
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
   20 IF(LCLUS.LE.0)GO TO 999
      LHITS=LQ(LCLUS-1)
      IF(LHITS.LE.0)GO TO 150
      NCELL=IQ(LHITS+2)
      IF(NCELL.LE.0)GO TO 150
      NCLUS=NCLUS+1   
        DO 100 I=1,NCELL
        INX=LCAEH+NREP*(IQ(LHITS+I+2)-1)
        IE=IQ(INX+12)
        IP=IQ(INX+13)
        IL=IQ(INX+14)
        ET=Q(INX+8)
        IF(ET.LE.0)GO TO 100
        IF(IL.LT.MNLYEM.OR.IL.GT.MXLYCH) GO TO 100
        IF(IE.LT.-NETAL.OR.IE.GT.NETAL)GO TO 100
        IF(IP.LT.1.OR.IP.GT.NPHIL)GO TO 100
        IF(IE.LT.0)THEN
           IETA=IE+NETAL+1
        ELSE
           IETA=IE+NETAL
        ENDIF
        ARRAY(IP,IETA)=ARRAY(IP,IETA)+ET
C MAKE COLOR =BANK NUMBER MODULO 14
       ICOLOR=MOD(NBANK,14)
        IF(ICOLOR.EQ.0)ICOLOR=14   
        IARRAY(IP,IETA)=ICOLOR
  100   CONTINUE
  150 CONTINUE
      GO TO 999
C error - CAEH BANK does not exist
  900 IOK=1 
  999 RETURN
      END
