      SUBROUTINE D0HLAB(HID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Label hsitogram plot with number of entries
C-
C-   Inputs  : HID - id of histogram to be labeled
C-   Outputs : drawing of label
C-   Controls: 
C-
C-   Created  28-FEB-1990   Sharon Hagopian
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER HID      
C---------------------------------------------------------------------
      INTEGER IY,NOENT
      REAL YLIN
      CHARACTER*20 IMESS2
      CHARACTER*8 IMESS1
      CHARACTER*12 CENTRY
C-----------------------------------------------------------------------
      DATA IY/-1/
      DATA IMESS1/'ENTRIES '/
C-----------------------------------------------------------------------
      CALL HNOENT(HID,NOENT)
      IY=IY+1
      IY=MOD(IY,10)
      WRITE(CENTRY,101)NOENT
  101 FORMAT(I8)
      IMESS2=IMESS1//CENTRY
      YLIN=17.-FLOAT(IY)
      CALL HPLCOM(10.,YLIN,IMESS2)
      CALL JUPDAT
  999 RETURN
      END
