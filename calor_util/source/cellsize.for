      SUBROUTINE CELLSIZE(FIRST_LAYER,LAST_LAYER,DR,DP,DZ,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the cell sizes in cylindrical
C-   coordinates (dR,dPHI,dZ) for ALL etas and for specified layers.
C-
C-   Inputs  : FIRST_LAYER  [I]   First layer to return
C-             LAST_LAYER   [I]   Last layer to return
C-             
C-   Outputs : DR(NETAL,*)  [R]   Delta-R
C-             DP(NETAL,*)  [R]   Delta-PHI
C-             DZ(NETAL,*)  [R]   Delta-Z
C-             STATUS       [I]   0 - OK
C-   Controls:
C-
C-   Created  27-APR-1992   Harrison B. Prosper
C-   Updated  15-JUN-1992   Harrison B. Prosper  
C-      Change arguments 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C----------------------------------------------------------------------
      INTEGER FIRST_LAYER, LAST_LAYER
      REAL    DR(NETAL,*)
      REAL    DP(NETAL,*)
      REAL    DZ(NETAL,*)
C----------------------------------------------------------------------
      INTEGER STATUS,LAYER,IPHI,N,IETA,NL
      REAL    R,Z,PHI
      REAL    DR1,DR2,DZ1,DZ2,DP1,DP2
      LOGICAL CEXIST
C----------------------------------------------------------------------
      STATUS = 0
C
C ****  Check inputs
C
      IF ( (FIRST_LAYER .LT. 1) .OR.
     &     (FIRST_LAYER .GT. NLYRL) ) THEN
        STATUS =-1
        GOTO 999
      ENDIF
      IF ( (LAST_LAYER .LT. 1) .OR.
     &     (LAST_LAYER .GT. NLYRL) ) THEN
        STATUS =-1
        GOTO 999
      ENDIF
      IF ( FIRST_LAYER .GT. LAST_LAYER ) THEN
        STATUS =-1
        GOTO 999
      ENDIF
C
      DO LAYER  = FIRST_LAYER, LAST_LAYER
        DO IETA = 1,  NETAL
          IF ( CEXIST(IETA,1,LAYER) ) THEN
            N = 0
C
            DR1 = 0.0
            DZ1 = 0.0
            DP1 = 0.0
            CALL CELL_SUB_WIDTH
     &          (IETA,1,LAYER,1,R,DR1,Z,DZ1,PHI,DP1,STATUS)
            IF ( STATUS .EQ. 0 ) THEN
              N = N + 1
            ENDIF
C
            DR2 = 0.0
            DZ2 = 0.0
            DP2 = 0.0
            CALL CELL_SUB_WIDTH
     &          (IETA,1,LAYER,2,R,DR2,Z,DZ2,PHI,DP2,STATUS)
            IF ( STATUS .EQ. 0 ) THEN
              N = N + 1
            ENDIF
C
            DR(IETA,LAYER) = DR1+DR2
            DP(IETA,LAYER) = DP1+DP2
            DZ(IETA,LAYER) = DZ1+DZ2
          ENDIF
        ENDDO
      ENDDO
  999 RETURN
      END
