      SUBROUTINE HEAD_CHECK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : check if the raw data has an old HEAD bank
C-                         increase size if needed
C-   Inputs  : none
C-   Outputs : none
C-   Controls: should be called before any reconstruction for every event
C-
C-   Created 31-MAR-1992   Serban Protopopescu  
C-   Updated   5-MAY-1994   Serban Protopopescu  increase to 32 words 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER   NDATA, NLINKS, GZTRGR, IRET
      LOGICAL MICRO_BLANK
      PARAMETER( NDATA = 32 )
      INTEGER MORED
C----------------------------------------------------------------------
C
      IF (LHEAD .GT. 0) THEN
C
C  check if this HEAD has an old format HEAD bank 
C
        MORED=NDATA-IQ(LHEAD-1)
        IF (MORED.GT.0) THEN
          CALL MZPUSH(IXCOM,LHEAD,0,MORED,' ')
        ENDIF
        IF(IQ(LHEAD+14).LT.5) THEN
          IF(GZTRGR().NE.0) THEN
            IF(MICRO_BLANK(IRET)) IQ(LHEAD+30)=1
          ENDIF
        ENDIF
        IF(IQ(LHEAD+14).LT.6) IQ(LHEAD+14)=6
      ENDIF
C
  999 RETURN
      END
