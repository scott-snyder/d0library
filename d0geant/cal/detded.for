      SUBROUTINE DETDED
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Definition of hit and
C-                         digitization parameters for the calorimeter
C-                         volume MCAL
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  1985 (?)      Rajendran Raja
C-                          A.M.Jonckheere
C-   Updated  13-SEP-1988   Rajendran Raja
C-   Updated  18-NOV-1988   Elliott A. Treadwell, Harrison B. Prosper
C-                          Change cryostat set names
C-   Updated  27-DEC-1988   Stuart Fuess  Added new CC names
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INTEGER ISET,IDET
C----------------------------------------------------------------------
      IF(DCRY.NE.0.AND.DUCA.NE.0)THEN
        CALL SETDET('IUSET_CC_CRY_WARM_TUBE+Z',SCAL,ISET,IDET)
        CALL SETDET('IUSET_CC_CRY_WARM_SIDE1+Z',SCAL,ISET,IDET)
        CALL SETDET('IUSET_CC_CRY_WARM_SIDE2+Z',SCAL,ISET,IDET)
        CALL SETDET('IUSET_CC_CRY_COLD_TUBE+Z',SCAL,ISET,IDET)
        CALL SETDET('IUSET_CC_CRY_COLD_SIDE1+Z',SCAL,ISET,IDET)
        CALL SETDET('IUSET_CC_CRY_COLD_SIDE2+Z',SCAL,ISET,IDET)
C
        CALL SETDET('IUSET_CC_CRY_WARM_TUBE-Z',SCAL,ISET,IDET)
        CALL SETDET('IUSET_CC_CRY_WARM_SIDE1-Z',SCAL,ISET,IDET)
        CALL SETDET('IUSET_CC_CRY_WARM_SIDE2-Z',SCAL,ISET,IDET)
        CALL SETDET('IUSET_CC_CRY_COLD_TUBE-Z',SCAL,ISET,IDET)
        CALL SETDET('IUSET_CC_CRY_COLD_SIDE1-Z',SCAL,ISET,IDET)
        CALL SETDET('IUSET_CC_CRY_COLD_SIDE2-Z',SCAL,ISET,IDET)
      ENDIF
      IF(DCRY.NE.0.AND.DECA.NE.0)THEN
        CALL SETDET('IUSET_EC_CRY_WARM+Z',SCAL,ISET,IDET)
        CALL SETDET('IUSET_EC_CRY_COLD+Z',SCAL,ISET,IDET)
        CALL SETDET('IUSET_EC_CRY_WARM-Z',SCAL,ISET,IDET)
        CALL SETDET('IUSET_EC_CRY_COLD-Z',SCAL,ISET,IDET)
      ENDIF
C----------------------------------------------------------------------
C  DEFINE CC ENDPLATES SET AND CC CRACKS SET
C  Modified 27-DEC-1988  Stuart Fuess
C----------------------------------------------------------------------
      IF(DUCA.NE.0)THEN
        CALL SETDET('IUSET_CC_ENDPLATES',SCAL,ISET,IDET)
        CALL SETDET('IUSET_CC_CRACKS',SCAL,ISET,IDET)
      ENDIF
C
C  DEFINE SET 'DEP+/-' = Dead Ec End Plates
C
      IF(DECA.NE.0)THEN
        CALL SETDET('IUSET_END_PLATES+Z',SCAL,ISET,IDET)
        CALL SETDET('IUSET_END_PLATES-Z',SCAL,ISET,IDET)
        CALL SETDET('IUSET_END_CRACKS+Z',SCAL,ISET,IDET)
        CALL SETDET('IUSET_END_CRACKS-Z',SCAL,ISET,IDET)
      ENDIF
C
      END
