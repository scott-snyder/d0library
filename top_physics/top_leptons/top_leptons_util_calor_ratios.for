      SUBROUTINE TOP_LEPTONS_UTIL_CALOR_RATIOS(LCLUS,EMFRAC,EMCORE,
     1  EISOL,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns best available estimates of 
C-                         Calorimeter energy ratios for electron
C-                         or photon 
C-   Inputs  : 
C-              LCLUS  - electron/photon Bank pointer
C- 
C-   Outputs : 
C-              EMFRAC - em fraction
C-              EMCORE - em fraction of energy outside core tower
C-              EISOL  - energy fraction outside core tower
C-              IER    - flag = 1/-1 for CACL+PELC/pure PELC
C-                       determination
C-
C-   Controls:  None
C-
C-   Created   6-SEP-1992   Stephen J. Wimpenny
C-   Modified 16-Oct-1992   CLUS_EM bug fixed
C-   Modified 28_Dec-1992   Uses D0 standard defn of cluster isolation
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      INTEGER IER,LCLUS,LCACL
      REAL EMFRAC,EMCORE,EISOL,CLUS_EM
C
      IER=1
      EMFRAC=2.0
      EMCORE=2.0
      EISOL=-1.0
C
C *** Go to CACL Bank to make FH correction - if bank is available
C
      LCACL=LQ(LCLUS-2)
        IF(LCACL.GT.0) THEN
          CLUS_EM=Q(LCACL+7)-Q(LCACL+19)
          IF(Q(LCACL+17).GT.1.0E-5)
     1      EMFRAC=CLUS_EM/Q(LCACL+17)
          IF(CLUS_EM.GT.1.0E-5)
     1      EMCORE=1.-(Q(LCLUS+14)/CLUS_EM)
        ELSE
C
C *** Otherwise use PELC/PPHO Bank to get approx values
C
            IER=-1
            IF(Q(LCLUS+18).GT.1.0E-5) THEN
              EMFRAC=Q(LCLUS+17)/Q(LCLUS+18)
              EMCORE=1.-(Q(LCLUS+14)/Q(LCLUS+18))
            ENDIF
        ENDIF
        IF(Q(LCLUS+17).GT.1.0E-5)
     1    EISOL=(Q(LCLUS+16)-Q(LCLUS+17))/Q(LCLUS+17)
C        IF(Q(LCLUS+16).GT.1.0E-5)
C     1    EISOL=(Q(LCLUS+16)-Q(LCLUS+15))/Q(LCLUS+16)
C----------------------------------------------------------------------
  999 RETURN
      END
