      SUBROUTINE LO_DSIG_DT_SEL(a,b,DS_DT_2,DSIG_DT_abTERM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To select the differential cross sections appropriate
C-                         to the current parton selections
C-
C-   Inputs  :    a and b        - These are the parton numbers for hadron A
C-                                  (proton) and hadron B (anti-proton) (-6:6)
C-                DS_DT_2(1:12)  - The twelve subprocess differential cross
C-                                  sections, dsigma/dt
C-
C-   Outputs :    DSIG_DT_abTERM - The portion of the term for partons a and b
C-                                  arising from the sum of subprocess
C-                                  differential cross sections
C-   Controls: None
C-
C-   Created  18-Oct-1993   Sandor Feher and Patrick Mooney
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      INTEGER       a,b
      REAL*8        DS_DT_2(12),DSIG_DT_abTERM
C
C----------------------------------------------------------------------
C
C   Determine which subprocesses contribute and sum their contributions
C
      IF( (a*b).NE.0 ) THEN
        IF( abs(a).NE.abs(b) ) DSIG_DT_abTERM = DS_DT_2(1)
        IF(     a .EQ.    b  ) DSIG_DT_abTERM = DS_DT_2(2)
        IF(     a .EQ.  (-b) ) DSIG_DT_abTERM = 4.0*DS_DT_2(3) +
     &                                 DS_DT_2(4) + DS_DT_2(6)
      ELSE
        IF( a.NE.0 .OR. b.NE.0 ) THEN
          DSIG_DT_abTERM = DS_DT_2(5)
        ELSE
          DSIG_DT_abTERM = 5.0*DS_DT_2(7) + DS_DT_2(8)
        ENDIF
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
