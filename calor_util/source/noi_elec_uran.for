      SUBROUTINE NOI_ELEC_URAN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Generate electronics and uranium noise
C-
C-   Inputs  :
C-   Outputs : Adds energy to CELL_EN arrray
C-   Controls:
C-
C-   Created   2-OCT-1990   Rajendran Raja
C-   Updated   8-AUG-1991   Rajendran Raja
C-   Updated  27-APR-1993   Ian Adam
C-            Use empirical noise as default if both
C-            gaussian and empirical noise are set in RCP
C-            and give an error message
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:NOISY.INC'
      REAL NOI_GAUSS,NOISE_EMP
      INTEGER ILYR,IPHI,IETA
      LOGICAL FIRST/.TRUE./
C----------------------------------------------------------------------
C
C ****  ADD ELECTRONIC NOISE
C

      IF(USE_EMP_NOISE.AND.USE_GAU_NOISE.AND.FIRST) THEN
        FIRST=.FALSE.
        CALL ERRMSG(' ','NOISY',
     &'Both empirical and gaussian noise set in RCP- use empirical'
     &,'W')
      ELSE
        FIRST=.FALSE.
      ENDIF
      IF(.NOT.USE_EMP_NOISE.AND.USE_GAU_NOISE)THEN
        DO ILYR=NLAYMIN,NLAYMAX
          DO IPHI=NPHIMIN,NPHIMAX
            DO IETA=NETAMIN,NETAMAX
              CELL_EN(IETA,IPHI,ILYR) = CELL_EN(IETA,IPHI,ILYR) +
     &                                  NOI_GAUSS(IETA,IPHI,ILYR)
            ENDDO
          ENDDO
        ENDDO
      ELSEIF(USE_EMP_NOISE) THEN
        DO ILYR=NLAYMIN,NLAYMAX
          DO IPHI=NPHIMIN,NPHIMAX
            DO IETA=NETAMIN,NETAMAX
              CELL_EN(IETA,IPHI,ILYR) = CELL_EN(IETA,IPHI,ILYR) +
     &                                  NOISE_EMP(IETA,IPHI,ILYR)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
  999 RETURN
      END
