      SUBROUTINE CAEQ_CHECK(DOCAEQ_TO_CAEP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Tests for existence of CAD1, CAD2, or CAEP
C-                         banks - if they are not found then tests
C-                         for CAEQ. If CAEQ exists then sets CAHITS
C-                         default to pick up CAEQ for CAHITS input.
C-                         This should make DST processing switches
C-                         same as default.
C-
C-   Inputs  : DOCAEQ_TO_CAEP [L] - CAHITS CAEQ input switch
C-   Outputs : DOCAEQ_TO_CAEP 
C-   Controls: CAD1,CAD2,CAEP,CAEQ banks
C-
C-   Created  18-JUN-1995   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZCAD1,GZCAD2,GZCAEP,GZCAEQ
      LOGICAL DOCAEQ_TO_CAEP
C----------------------------------------------------------------------
      IF ((GZCAD1().LE.0).and.(GZCAD2().LE.0).and.(GZCAEP().LE.0)) THEN
        IF(GZCAEQ().GT.0) THEN
          CALL ERRMSG('USE CAEQ FOR CAHITS','CAEQ_CHECK',
     &     'NO CAD or CAEP banks found','S')
          CALL ERRMSG('USE CAEQ FOR CAHITS','CAEQ_CHECK',
     &     'NO CAD or CAEP banks found','I')
           DOCAEQ_TO_CAEP = .TRUE.
        ELSE
          CALL ERRMSG('NO INPUT FOR CAHITS','CAEQ_CHECK',
     &     'NO CAD,CAEP, or CAEQ banks, consider CATD','W')
        END IF
      END IF
  999 RETURN
      END
