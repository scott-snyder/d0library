      INTEGER FUNCTION NOI_HIST_ID(IETA,IPHI,ILAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Translate eta, phi, and layer into histogram
C-                         id corresponding to pedestal distributions.
C-
C-   Inputs  : CELL id
C-   Outputs : a histogram id
C-   Controls:
C-
C-   Created  23-OCT-1991   Amber S. Boehnlein
C-   The histogram ids correspond to pedestal histograms generated
C-   by Joan Guida from commisioning run 021875.
C-
C-   MODIFIED 27-APR-1993   Ian Adam
C-   Use simplified histogram scheme.  Include ECs.  Pad size
C-   differences are ignored, as are MR channels for now.  The noise
C-   data from all phi's for a given eta,layer are merged.
C-   The package used to generate the histograms should take the IDs 
C-   from this routine.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INTEGER IETA, IPHI, ILAYER
      LOGICAL CEXIST
C----------------------------------------------------------------------
C     Histograms are divided up by layer, eta, and pad size

      IF(CEXIST(IETA,IPHI,ILAYER)) THEN
        NOI_HIST_ID = 10000 + 100*ILAYER + ABS(IETA)
      ENDIF

  999 RETURN
      END
