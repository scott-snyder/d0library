      SUBROUTINE MKPATH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Make header banks for a given path (RECO, GEAN or FAKE)
C-
C-   Created  28-JUN-1989   Serban D. Protopopescu
C-   Updated   2-JUN-1994   Serban Protopopescu  skip for MDST 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LDUMY
      CHARACTER*4 PATH
C----------------------------------------------------------------------
C
      CALL PATHGT(PATH)
      IF ( PATH .EQ. 'MDST') RETURN
C
      CALL BKHSUM(LDUMY)
      CALL PATHBK(LDUMY)
      IF(LDUMY.NE.0) THEN
        CALL BKHITS(LDUMY)
        CALL BKPROC(LDUMY)
        CALL PARHFL        ! does not make new one if it exists
      ENDIF
  999 RETURN
      END
