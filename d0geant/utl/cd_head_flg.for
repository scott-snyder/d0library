      SUBROUTINE CD_HEAD_FLG(DETECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set flags in CD word in HEAD bank for 
C-                              STPFILE version.
C-
C-   Inputs  : DETECTOR     = 'TRD','FDC','VTX' or 'CDC'
C-   Outputs : 
C-
C-   Created   4-DEC-1992   Robert E. Avery   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:D0LOG.INC' 
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
C  Input:
      CHARACTER*3       DETECTOR
C  Local:
      INTEGER LASTTRDSTP,LASTFDCSTP
      INTEGER IHEAD_FLGS 
      BYTE BHEAD_FLGS(4)
      EQUIVALENCE(BHEAD_FLGS,IHEAD_FLGS)
      INTEGER FLAG_WORD
      PARAMETER ( FLAG_WORD = 13 )
C----------------------------------------------------------------------
      IHEAD_FLGS = IQ(LHEAD+FLAG_WORD) 
      IF     ( DETECTOR.EQ.'TRD' ) THEN
        BHEAD_FLGS(BYTE1) = LASTTRDSTP()              ! TRD 
C
      ELSEIF ( DETECTOR.EQ.'FDC' ) THEN
        BHEAD_FLGS(BYTE2) = LASTFDCSTP()              ! FDC
C
      ELSEIF ( DETECTOR.EQ.'VTX' ) THEN
        BHEAD_FLGS(BYTE3) = 0                         ! VTX
        IF (SVTX(6).EQ.1) BHEAD_FLGS(BYTE3) = 1       
C
      ELSEIF ( DETECTOR.EQ.'CDC' ) THEN
        BHEAD_FLGS(BYTE4) = 0                         ! CDC (nothing yet)
C
      ENDIF
      IQ(LHEAD+FLAG_WORD) = IHEAD_FLGS 
C      
  999 RETURN
      END
