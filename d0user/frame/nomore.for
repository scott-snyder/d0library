      LOGICAL FUNCTION NOMORE()
C------------------------------------------------------------
C-                                                          -
C-     handle flag NOMORE                                   -
C-     after one call NOMORE is false unless reset          -
C-     with a call to SETNMR                                -
C-                                                          -
C-     ENTRY SETNMR                                         -
C-                                                          -
C-     INPUT:                                               -
C-     FL = .TRUE. in next call NOMORE will be true         -
C-                                                          -
C-          SDP Feb.,1987                                   -
C-                                                          -
C------------------------------------------------------------
C
      IMPLICIT NONE
      LOGICAL FL,NOMRFL,SETNMR
C
      NOMORE=NOMRFL
      NOMRFL=.FALSE.
      RETURN
C
      ENTRY SETNMR(FL)
      NOMRFL=FL
      RETURN                                                 
C
      END      
