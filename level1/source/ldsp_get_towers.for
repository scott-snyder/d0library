      SUBROUTINE LDSP_GET_TOWERS(EMET,TOTET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill arrays of level 1 trigger tower Et.
C-                         Try first from TRGR bank, but if not there
C-                         use CATD back.
C-
C-   Inputs  : 
C-
C-   Outputs :    EMET   - Real 3DIM array of trigger tower of EM Et
C-                TOTET  -                 "                   TOT Et
C- 
C-   Controls: 
C-
C-   Created  23-NOV-1993   sFahey
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
C
      REAL EMET(-ETA_MAX:ETA_MAX,PHI_MIN:PHI_MAX)
      REAL TOTET(-ETA_MAX:ETA_MAX,PHI_MIN:PHI_MAX)
      INTEGER L1ETAC,L1PHIC
      INTEGER LTRGR,GZFIND_CRATE,GZTRGR
C
C----------------------------------------------------------------------
C
      CALL VZERO(EMET,(2*ETA_MAX+1)*(PHI_MAX-PHI_MIN+1))
      CALL VZERO(TOTET,(2*ETA_MAX+1)*(PHI_MAX-PHI_MIN+1))
C
      LTRGR = GZFIND_CRATE('TRGR', GZTRGR(), 11 )
      IF (LTRGR.GT.0) THEN
C
        DO L1ETAC = -20,20
          DO L1PHIC = 1,32
            IF (L1ETAC.NE.0) THEN
              CALL CL1PHET(L1PHIC, L1ETAC, 
     &                     EMET(L1ETAC,L1PHIC), 
     &                     TOTET(L1ETAC,L1PHIC))
            ENDIF
          ENDDO
        ENDDO
      ENDIF
C
  999 RETURN
      END
