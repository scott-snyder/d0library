      FUNCTION ESUM_CONESIZE_TO_JET( STYP, CONESIZE, IER )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the ESUM object ID for the jet with the
C-                         closest cone size to that which is requested.
C-
C-   Returned value  : ESUM Jet object ID (See D0$PARAMS:ESUM.PARAMS)
C-   Inputs  : STYP      [C*4]         'FILT', 'RECO'...
C-             CONESIZE  [R]           Requested conesize in radians
C-   Outputs : IER       [I]           Error code
C-                                     IER = -1 (No object found )    
C-   Controls: 
C-
C-   Created  19-JUL-1992   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      REAL DELTA_R_MIN
      PARAMETER( DELTA_R_MIN = .15 ) ! Accept conesize within this range
      REAL CONESIZE, DRMIN, R
      INTEGER IER, ESUM_CONESIZE_TO_JET, IOB
      REAL ESUM_JET_TO_CONESIZE
      CHARACTER*4 STYP
C----------------------------------------------------------------------
C
C---Loop over all possible jet id's and see which one brings back an 
C---acceptable cone size
C
      DRMIN = 1000.                        ! Set minium difference high
      ESUM_CONESIZE_TO_JET = 0             ! Nothing found
C
      DO IOB = ID_JET_1, ID_JET_5
        R = ESUM_JET_TO_CONESIZE( STYP, IOB, IER )
        IF ( IER .EQ. 0 .AND. ABS(CONESIZE-R) .LT. DRMIN .AND.
     &    ABS(CONESIZE-R) .LT. DELTA_R_MIN ) THEN
          DRMIN = ABS( CONESIZE - R )
          ESUM_CONESIZE_TO_JET = IOB
        ENDIF
      ENDDO
C
C---Did we find one?
      IF (  ESUM_CONESIZE_TO_JET .GE. ID_JET_1 .AND.
     &  ESUM_CONESIZE_TO_JET .LE. ID_JET_5) THEN
        IER = 0
      ELSE
        IER = -1
        ESUM_CONESIZE_TO_JET = -999
      ENDIF

  999 RETURN
      END
