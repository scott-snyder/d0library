       SUBROUTINE CLGA
C--------------------------------------------------------------------------
C
C      THIS SUBROUTINE CALLS SUBROUTINES TO CREATE CENTRAL CALORIMETER AND
C      END CALORIMETER module BANKS.
C
C      AUTHOR:     S. KAHN             28 APRIL 1987
C      REVISED:    S. KAHN             19-OCT-1989  -- Add Massless Gap
C
C-------------------------------------------------------------------------
C
       CALL SLSRCP('SRCP_UCAL')
       CALL CLGACC             ! creates CEN CAL module banks
C
       CALL CMSGCC             ! creates CC massless gap module 
C
       CALL SLSRCP('SRCP_ECAL')
       CALL CLGAEE             ! creates END CAL EM module banks
C
       CALL CLGAEH             ! creates END CAL HADRONIC module banks
C
       CALL CMSGEC             ! creates EC massless gap module
C
       RETURN
       END
