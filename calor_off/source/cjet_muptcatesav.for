      SUBROUTINE CJET_MUPTCATESAV(PTCATE_SAV,SAVE_CODE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  : PTCATE_SAV : local array in which PTCATE is stored
C-             SAVE_CODE  : 1 -> SAVE PTCATE in PTCATE_SAV
C-                          2 -> RESTORE PTCATE from PTCATE_SAV
C-   Outputs :
C-   Controls:
C-
C-   Created  14-JAN-1993   Alex Smith
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZLINKC.INC/LIST'       ! Protected Link area
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCATE.INC'
      INCLUDE 'D0$PARAMS:CATENM.PARAMS'
      INCLUDE 'D0$LINKS:IZCATE.LINK'
C
      INTEGER K,IPHI,IETA,SAVE_CODE
      INTEGER PTCATE_SAV(-NETAL:NETAL,NPHIL,2)
C----------------------------------------------------------------------
      DO 11 IETA = -NETAL ,  NETAL
        DO 12 IPHI = 1 ,  NPHIL
          DO 13 K = 1 ,  2
            IF (SAVE_CODE .EQ. 1) THEN
              PTCATE_SAV(IETA,IPHI,K) = PTCATE(IETA,IPHI,K)
            ENDIF
            IF (SAVE_CODE .EQ. 2) THEN
              PTCATE(IETA,IPHI,K) = PTCATE_SAV(IETA,IPHI,K)
            ENDIF
   13     CONTINUE
   12   CONTINUE
   11 CONTINUE

  999 RETURN
      END
