      SUBROUTINE ECEM_TOWER_GEOM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write out Tower level ECEM pad geometry 
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   10-MAY-1990   Natalie Roe
C----------------------------------------------------------------------
      IMPLICIT NONE      
C  Local Variables
      INTEGER ICODE,IER,I,J,K          
      LOGICAL FIRST/.TRUE./       
C----------------------------------------------------------------------
D     WRITE(6,*)'IN ECEM_TOWER_GEOM'
C----------------------------------------------------------------------

      CALL EZPICK('ENDCAP')
      CALL WRITE_ZDIV('EM1+Z_PADS')
      CALL WRITE_ZDIV('EM2+Z_PADS')
      CALL WRITE_ZDIV('EM3A+Z_PADS')
      CALL WRITE_ZDIV('EM3B+Z_PADS')
      CALL WRITE_ZDIV('EM3C+Z_PADS')
      CALL WRITE_ZDIV('EM4A+Z_PADS')
      CALL WRITE_ZDIV('EM4B+Z_PADS')

C----------------------------------------------------------------------
      RETURN
      END
