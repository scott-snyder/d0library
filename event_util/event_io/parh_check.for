      SUBROUTINE PARH_CHECK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : check if the raw data has a illegal PARH bank
C-       ( Most of the old MC data generated before Feb-1991 have the 
C-        old format PARH bank in the raw data file, which screws up the
C-        reconstruction) This subroutine should eventually be dropped.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: should be called before any reconstruction for every event
C-
C-   Created  12-MAR-1992   Qizhong Li-Demarteau
C-   Updated  16-MAR-1992   Serban Protopopescu  use MZPUSH instead of MZDROP 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER  LPARH, GZPARH
      INTEGER  ILINK, NDATA, NLINKS
      PARAMETER( NDATA = 60 )
      PARAMETER( NLINKS = 8 )
      INTEGER MORED,MOREL
C----------------------------------------------------------------------
C
      LPARH = GZPARH()
      IF (LPARH .GT. 0) THEN
C
C  check if this PARH has an old/illegal format PARH bank 
C
        MOREL=NLINKS-IQ(LPARH-3)
        MORED=NDATA-IQ(LPARH-1)
        IF (MOREL.GT.0.OR.MORED.GT.0) 
     &    CALL MZPUSH(IXCOM,LPARH,MOREL,MORED,' ')
      ENDIF
C
  999 RETURN
      END
