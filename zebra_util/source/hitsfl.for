      SUBROUTINE HITSFL
C  ************************************************************** 
C  *                                                            *
C  *  Create general Zebra bank HITS, header for detector hits. *
C  *  Use PATHST to specify whether to hang from 'GEAN' or 'RECO'
C  *                                                            *
C  *                                      Marcel Demarteau      *
C  *                                      Serban Protopopescu   *
C  *                                             Sept. 1987     *
C  *                                      mod. APR.,1989
C  *                                                            *
C  ************************************************************** 

      IMPLICIT NONE
      INTEGER LHITS
C----------------------------------------------------------------------
      CALL BKHITS(LHITS)
  999 RETURN
      END
