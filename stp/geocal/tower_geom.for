      SUBROUTINE TOWER_GEOM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write out Tower level ECEM and ECIH pad 
C-                         geometry. 
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   10-MAY-1990   Natalie Roe
C-   Updated  27-NOV-1990   Andrew Milder  Added ECIH pad stuff, renamed from
C-                                         ECEM_TOWER_GEOM. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE      
C  Local Variables
      INTEGER ICODE,IER,I,J,K          
      LOGICAL FIRST/.TRUE./       
C----------------------------------------------------------------------
      CALL EZPICK('ENDGEN')
      CALL WRITE_ZDIV('EM1_DIVISIONS+Z')
      CALL WRITE_ZDIV('EM2_DIVISIONS+Z')
      CALL WRITE_ZDIV('EM3_DIVISIONS+Z')
      CALL WRITE_ZDIV('EM4_DIVISIONS+Z')
      CALL WRITE_ZDIV('IFH_DIVISIONS+Z')
      CALL WRITE_ZDIV('ICH_DIVISIONS+Z')
C----------------------------------------------------------------------
      RETURN
      END
