C VAX/DEC CMS REPLACEMENT HISTORY, Element GZCDH3.FOR
C *1     9-NOV-1993 18:02:32 AVERY "fdc changes for v12 reco"
C VAX/DEC CMS REPLACEMENT HISTORY, Element GZCDH3.FOR
      FUNCTION GZCDH3()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find pointer to CDH3 bank (result bank for
C-   level-2 FDC hit-finding).
C-
C-   Returned value  : integer GZCDH3 = pointer to bank
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  26-APR-1993   Chris Klopfenstein
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCDH3.LINK'
      INTEGER GZCDH3
      INTEGER GZL2CH, LL2CH
C----------------------------------------------------------------------
      GZCDH3 = 0
      LL2CH = GZL2CH()
      IF (LL2CH .GT. 0) GZCDH3 = LQ(LL2CH - IZCDH3)
  999 RETURN
      END
