      SUBROUTINE CLUSMR
C ----------------------------------------------------------------------
C-
C-   PURPOSE AND METHODS : SMEARING OF THE ENERGY OF THE TRD CLUSTERS
C-
C-   INPUTS  :
C-   OUTPUTS :
C-
C-   CREATED  11-MAY-1987   A. ZYLBERSTEJN
C-   UPDATED  11-DEC-1987   A. ZYLBERSTEJN
C-   UPDATED  19-SEP-1988   J.R. HUBBARD/A. ZYLBERSTEJN
C-   UPDATED  13-OCT-1988   J.R. HUBBARD          Include Cathodes     
C-
C----------------------------------------------------------------------
C
C
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:CLUSM.INC/LIST'
      INCLUDE 'D0$INC:CLUSTR.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:ENETRD.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:INFO.INC/LIST'
      INCLUDE 'D0$INC:TECDSC.INC/LIST'
C
      INTEGER I,J
      REAL XINT,EX,EX1
      REAL YAUX(300),ATT
C
      J=0
      DO 300 I=1,NSMEAR
        XINT=XCLES(I)
        EX1=0.
C  -----------------
C  ENERGY RESOLUTION
C  ------------------
C  THE SMEARING ON MEASURED CLUSTER ENERGIE HAVE BEEN PUT IN UPSTREAM
C SUBROUTINES
C  IN "CLURAD" FOR X RAYS AND IN "CLUDEL" FOR DELTA RAYS
        EX=ECLES(I)
C
C ----------
C  ATTACHMENT
C  ---------
        ATT=0.
        EX1=EX*(1.-ATTACH*CLUDIS(I,1)/XDER)
        IF(PTRD.GE.10) THEN
          WRITE(LOUT,*)ECLES(I),' EX,EX1',EX,EX1,'CLUDIS',CLUDIS(I,1)
        ENDIF
        IF(EX1.LT.ECLMIN)GO TO 300
        J=J+1
        ECLES(J)=EX1
        IWIRE(J)=IWIRE(I)
        XCLES(J)=XCLES(I)
        CLUDIS(J,1)=CLUDIS(I,1)
        CLUDIS(J,2)=CLUDIS(I,2)
        ZCLES(J)=ZCLES(I)
        TIMEC(J)=TIMEC(I)
        ISTRIP(J)=ISTRIP(I)
        DSTRIP(J)=DSTRIP(I)
        EGENST=EGENST+ECLES(J)
        IF(CLUDIS(I,1).GT.0.001)EGSDRI=EGSDRI+ECLES(J)
  300 CONTINUE
      NSMEAR=J
C ------------
C SPACE CHARGE
C-------------
C  TO BE IMPLEMENTED
C
      RETURN
      END
