      SUBROUTINE NOI_ZERO_SUPPRESS(SIGCUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ZERO SUPPRESS THE CELL_EN ARRAY ACCORDING TO
C-                         NOISY DISTRIBUTIONS
C-
C-   Inputs  : SIGCUT: ZERO SUPPRESSION CUT IN UNITS OF PED SIGMA
C-
C-   Outputs : THE ARRAY CELL_EN (IN THE NOISY COMMON IN NOISY.INC)
C-             IS ZERO SUPPRESSED.
C-
C-   Controls:
C-
C-   Created   8-MAR-1993   Ian Adam
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:NOISY.INC'
      INTEGER IETA,IPHI,ILYR
      INTEGER STATUS
      REAL SIGCUT
      REAL SIGMA(-37:37,1:17)
      SAVE SIGMA
      LOGICAL FIRST/.TRUE./
      REAL HSTATI,CAD_GAIN
      LOGICAL CEXIST,HEXIST,LIST_NOI_HIST
      INTEGER NOI_HIST_ID,HISID
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST=.FALSE.

        CALL EZPICK('NOISY_RCP')
        CALL EZGET('LIST_NOI_HIST',LIST_NOI_HIST,STATUS)
        IF ((STATUS.EQ.0).AND.LIST_NOI_HIST) CALL HLDIR (' ','T')
        CALL EZRSET

        CALL HCDIR ('//PAWC/PEDS',' ')

        IPHI=1
        DO IETA=-37,37
          DO ILYR=1,17
            IF (CEXIST(IETA,IPHI,ILYR)) THEN
              HISID=0
              HISID = NOI_HIST_ID(IETA,IPHI,ILYR)
              IF (HISID.NE.0.AND.HEXIST(HISID)) THEN
                SIGMA(IETA,ILYR)=HSTATI(HISID,2,' ',0)
              ELSE
                SIGMA(IETA,ILYR)=0.0
              ENDIF
            ELSE
              SIGMA(IETA,ILYR)=0.0
            ENDIF
            SIGMA(IETA,ILYR)=SIGMA(IETA,ILYR)*CAD_GAIN(IETA,
     &          ILYR)
          ENDDO
        ENDDO

      ENDIF

      DO IETA=-37,37
        DO IPHI=1,64
          DO ILYR=1,17
            IF (ABS(CELL_EN(IETA,IPHI,ILYR)).LT.(SIGCUT*SIGMA(IETA,
     &        ILYR))) CELL_EN(IETA,IPHI,ILYR)=0.0
            IF (SIGMA(IETA,ILYR).EQ.0.0) CELL_EN(IETA,IPHI,ILYR)=0.0
          ENDDO
        ENDDO
      ENDDO

  999 RETURN
      END
