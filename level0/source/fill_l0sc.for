      SUBROUTINE FILL_L0SC(IBUNCH,GOODZ_SCALER,NGOODZ_SCALER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills out L0SC Zebra banks
C-
C-   Inputs  : NBUNCH - bunch number
C-             GOODZ_SCALER
C-             NGOODZ_SCALER
C-   Outputs : None
C-   Controls: None
C-
C-   Created  18-JUL-1992   Freedy Nang
C-   Updated  28-JAN-1994   Jeffrey Bantly  add 4 scalers for Run 1B data 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:LV0CON.INC'
C
      INTEGER GOODZ_SCALER(6,0:NL0_SCALERS)
      INTEGER NGOODZ_SCALER(6,0:NL0_SCALERS)
      INTEGER I, IBUNCH
      INTEGER LKL0SC
      INTEGER GZL0SC_BUNCH
      EXTERNAL GZL0SC_BUNCH
C
C----------------------------------------------------------------------
C
C ****  Fetch L0SC link
C
      LKL0SC=GZL0SC_BUNCH(IBUNCH)
      IF ( LKL0SC.LE.0 ) CALL BKL0SC(LKL0SC,IBUNCH)
C
C ****  Fill L0SC bank
C
      DO I = 0, NL0_SCALERS
        IQ(LKL0SC+4+I)=GOODZ_SCALER(IBUNCH,0)
        IQ(LKL0SC+4+I+NL0_SCALERS)=NGOODZ_SCALER(IBUNCH,0)
      ENDDO
C----------------------------------------------------------------------
  999 RETURN
      END
