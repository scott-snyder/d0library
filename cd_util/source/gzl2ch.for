C VAX/DEC CMS REPLACEMENT HISTORY, Element GZL2CH.FOR
C *1     9-NOV-1993 18:03:36 AVERY "fdc changes for v12 reco"
C VAX/DEC CMS REPLACEMENT HISTORY, Element GZL2CH.FOR
      INTEGER FUNCTION GZL2CH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find pointer to L2CH bank (header for
C-   level-2 CD hit-finding results).
C-
C-   Returned value  : integer GZL2CH = pointer to bank L2CH
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  26-APR-1993   Chris Klopfenstein
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      include 'd0$inc:zebcom.inc'
      include 'd0$links:izl2ch.link'
      integer gzfres, lfres
C----------------------------------------------------------------------
      gzl2ch = 0
      lfres = gzfres()
      if (lfres .gt. 0) gzl2ch = LQ(lfres - izl2ch)
  999 RETURN
      END
