       SUBROUTINE CLAY
C--------------------------------------------------------------------------
C
C      THIS SUBROUTINE CALLS SUBROUTINES TO CREATE CENTRAL CALORIMETER AND
C      END CALORIMETER LAYER BANKS.
C
C      AUTHOR:     S. KAHN             28 APRIL 1987
C      REVISED:    S. KAHN             20 OCT 1989 -- added massless gap calls
C
C-------------------------------------------------------------------------
C
       CALL SLSRCP('SRCP_UCAL')
       CALL CLAYCC             ! creates CEN CAL layer banks
C
       CALL CLAYMG_CC          ! creates CEN CAL MG layer banks
C
       CALL SLSRCP('SRCP_ECAL')
       CALL CLAYEE             ! creates END CAL EM layer banks
C
       CALL CLAYIH             ! creates END CAL INNER HADRONIC layer banks
C
       CALL CLAYMH             ! creates END CAL MIDDLE HADRONIC layer
C                              ! banks
       CALL CLAYOH             ! creates END CAL OUTER HADRONIC layer banks
C 
       CALL CLAYMG_EC          ! creates END CAL MG layer banks
C
       RETURN
       END
