      SUBROUTINE INITAB
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C  COMPUTE TABLES FOR ABSORPTION LENGTHS FOR ALL THE TRD MATERIALS AS A
C  FUCTION OF THE ENERGY
C-
C-   Inputs  :
C      ED : ENERGY IN KEV
C      XSTEP : BIN WIDTH
C      NSTEP : NB. IF BINS
C-   Outputs :
C-   Controls:
C-
C-   Created                A. ZYLBERSTEJN
C-   Updated  19-SEP-1988   J.R. HUBBARD/A. ZYLBERSTEJN
C-
C----------------------------------------------------------------------
C
C --------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ABSOR.INC/LIST'
      INCLUDE 'D0$INC:TECDSC.INC/LIST'
      INCLUDE 'D0$INC:RADDCC.INC/LIST'
      INCLUDE 'D0$INC:TECDCC.INC/LIST'
      INCLUDE 'D0$INC:WINDCC.INC/LIST'
      INCLUDE 'D0$INC:XRAY.INC/LIST'
C
      INTEGER I
      REAL    E
      REAL    TRDFMY
C
      E=EDOWN
      NABS=NSTEP
      DO 4 I=1,NSTEP
        E=EDOWN+(FLOAT(I)-.5)*XSTEP
        EABS(I)=E
        ED(I)=E
C      WRITE(LOUT,*)I,' ED',ED(I),' EDOWN,XSTEP',EDOWN,XSTEP
C  COMPUTE ABSORPTION LENGTHS FOR ALL MATERIALS
        ABSKI(I) =TRDFMY(E, SKIN)
        ABMET(I) =TRDFMY(E,MET)
        ABAIR(I) =TRDFMY(E,'AIR')
        ABSAN(I) =TRDFMY(E,SAND)
        ABGAS(I) =TRDFMY(E,GAS)*PERC
        ABFOIL(I)=TRDFMY(E,FOIL)
        ABGAP(I) =TRDFMY(E,GAP)
        ABSTUF(I)=TRDFMY(E,STUFF)
    4 CONTINUE
C      IF (PTRD.GT.1) THEN
C        WRITE(LOUT,*)'EXIT INITAB'
C      ENDIF
      RETURN
      END
