      SUBROUTINE GTL0SC(BUNCH,IBUNCH,NSCALERS,NWORDS,GOODZ_SCALER,
     &  NGOODZ_SCALER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fetch contents of Zebra bank L0SC
C-
C-   Inputs  : BUNCH - desired buncher number
C-   Outputs : IBUNCH - ith bunch number
C-             NSCALERS - number of scalers
C-             NWORDS - number of words per scaler
C-             GOODZ_SCALER - good z value per scaler
C-             NGOODZ_SCALER - complement of good z value per scaler
C-   Controls: None
C-
C-   Created  21-JUL-1992   Freedy Nang
C-   Updated  28-JAN-1994   Jeffrey Bantly  to include 4 extra Run 1B scalers 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:LV0CON.INC'
C
      INTEGER BUNCH
      INTEGER IBUNCH, NSCALERS, NWORDS
      INTEGER GOODZ_SCALER(0:NL0_SCALERS)
      INTEGER NGOODZ_SCALER(0:NL0_SCALERS)
      INTEGER LKL0SC
      INTEGER I
      INTEGER GZL0SC_BUNCH, LZFIND
      EXTERNAL GZL0SC_BUNCH, LZFIND
C----------------------------------------------------------------------
C
C ****  Fetch bank link of desired bunch
C
      LKL0SC=GZL0SC_BUNCH(BUNCH)
C
C ****  Load arrays from bank
C
      IF ( LKL0SC.NE.0 ) THEN
        IBUNCH=IQ(LKL0SC+1)
        NSCALERS=IQ(LKL0SC+2)
        NWORDS=IQ(LKL0SC+3)
        DO 10 I = 0 ,NL0_SCALERS
          GOODZ_SCALER(I)=IQ(LKL0SC+4+I)
          NGOODZ_SCALER(I)=IQ(LKL0SC+4+NL0_SCALERS+I)
   10   CONTINUE
      ELSE
C
C ****  else zero everything
C
        IBUNCH=0
        NSCALERS=0
        NWORDS=0
        CALL VZERO(GOODZ_SCALER,19)
        CALL VZERO(NGOODZ_SCALER,19)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
