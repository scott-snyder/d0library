      LOGICAL FUNCTION GOON  
C------------------------------------------------------------
C-                                                          -
C-     handle flag GOON                                     -
C-                                                          -
C-     ENTRY SETGON                                         -
C-                                                          -
C-     INPUT:                                               -
C-     FL = .TRUE. in next call GOON will be true           -
C-                                                          -
C-          SDP Feb.,1987                                   -
C-                                                          -
C------------------------------------------------------------
C
      IMPLICIT NONE
      LOGICAL FL,GOONFL,SETGON
C
      GOON=GOONFL
      RETURN
C
      ENTRY SETGON(FL)
      GOONFL=FL
      RETURN                                                 
C
      END         
