      SUBROUTINE RECOFL
C  ************************************************************** 
C  *                                                            *
C  *  Create Zebra bank RECO for event reconstruction           * 
C  *                                                            *
C  *                                      Marcel Demarteau      *
C  *                                      Serban Protopopescu   *
C  *                                             Sept. 1987     *
C  *                                                            *
C  ************************************************************** 
      IMPLICIT NONE
      INTEGER LRECO
C  --------------------------------------------------------------
C 
      CALL BKRECO(LRECO)
  999 RETURN
      END
