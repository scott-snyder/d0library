       SUBROUTINE CLGI
C--------------------------------------------------------------------------
C
C      THIS SUBROUTINE CALLS SUBROUTINES TO CREATE CENTRAL CALORIMETER AND
C      END CALORIMETER inactive BANKS.
C
C      AUTHOR:     S. KAHN             27 JAN 1987
C
C-------------------------------------------------------------------------
C
       CALL SLSRCP('SRCP_UCAL')
       CALL CLGICC             ! creates CEN CAL inactive banks
C
       CALL SLSRCP('SRCP_ECAL')
       CALL CLGIEE             ! creates END CAL EM inactive banks
C
       CALL CLGIEH             ! creates END CAL  HADRON inactive banks
C
       RETURN
       END

