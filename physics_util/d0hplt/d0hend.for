      SUBROUTINE D0HEND
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : End D0HPLT and clear screen of PLOTS
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   6-MAR-1990   S. Hagopian
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      LOGICAL FLGVAL
C----------------------------------
      IF(FLGVAL('DI3INT'))THEN
         IF(.NOT.FLGVAL('D0HAUT'))THEN
           CALL HPLNUL
C TURN STATISTICS ON FOR REGULAR PLOTS
           CALL HPLOPT('STA',1)
           CALL D0HCOM
        ENDIF
      ENDIF 
  999 RETURN
      END
