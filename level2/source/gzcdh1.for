      INTEGER FUNCTION GZCDH1()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find pointer to CDH1 bank (result bank for
C-                         level-2 VTX hit-finding).
C-   Returned value  :     integer GZCDH1 = pointer to bank
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  11-FEB-1994   Liang-Ping Chen
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------

       INCLUDE 'D0$INC:ZEBCOM.INC'
       INCLUDE 'D0$LINKS:IZCDH1.LINK'
       INTEGER GZL2CH, LL2CH
C----------------------------------------------------------------------
       GZCDH1 = 0
       LL2CH = GZL2CH()
       IF (LL2CH .GT. 0) GZCDH1 = LQ(LL2CH - IZCDH1)
  999 RETURN                    
      END
