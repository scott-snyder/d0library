      SUBROUTINE BKVLAY(ILAYV,LKVLAY)
C-----------------------------------------------------------------------
C-  Subroutine BKVLAY books the bank "VLAY" for a specified
C-  vertex detector layer.
C-  Input:
C-    ILAYV is the VTX layer.
C-  Output:
C-    LKVLAY is the link to the booked "VLAY" bank, 0 if error detected.
C-
C-   Created   4-JAN-1987  T. Trippe for VTX
C-   Updated  12-OCT-1988   Ghita Rahal-Callot  : Book the upper level banks
C-                          elsewhere 
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZVLAY.LINK/LIST'
      INTEGER ILAYV,LKVLAY
C
C ****  Book 'VLAY'
C
      CALL MZBOOK(IXMAIN,LVLAY(ILAYV),LVTXH,-IZVLAY-ILAYV,'VLAY',
     +                 32,32,4,2,0)
      IQ(LVLAY(ILAYV)-5)=ILAYV                 ! set numeric bank id
C
      RETURN
      END
