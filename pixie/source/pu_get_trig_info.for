      SUBROUTINE PU_GET_TRIG_INFO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get Trig/Filt info for EXAMINE-PIXIE and
C-                         display them.
C-    *** This routines will keep in Beta area until the trigger ***
C-    *** information bank is available !!!                      ***
C-
C-   Updated  16-OCT-1992   Nobuaki Oshima - Use TSAM Bank.
C-   Created  10-APR-1992   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER I,J,ID1,ID2,NLINE
      INTEGER NTRIGON,NFILTON
      INTEGER TRIGBON(32),FILTBON(128)
      CHARACTER*32 TRIGNON(32),FILTNON(128)
      CHARACTER*64 L1_NAME,L2_NAME
      CHARACTER*20 L1_NAME_LIST(128),L2_NAME_LIST(128),TRGMES
      LOGICAL FIRST
      DATA    FIRST / .TRUE. /
C----------------------------------------------------------------------
C-
C--- Get list of Trigger and Filter Name
C-
      CALL GTTSUM(NTRIGON,TRIGBON,TRIGNON,NFILTON,FILTBON,FILTNON)
C-
C--- Start scanning the trigger bits of level 1...
C-
      L1_NAME_LIST(1) = '*L1 Trig Name*'
      L1_NAME_LIST(2) = '--------------'
      J = 2
      IF ( NTRIGON .GT. 0 ) THEN
        DO I = 1,NTRIGON
          J = J + 1
          IF ( J .LE. 128 ) THEN
            L1_NAME_LIST(J) = TRIGNON(I)
            IF (L1_NAME_LIST(J) .EQ. ' ') THEN
              J = J - 1
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C-
C--- Write L1 trigger name list...
C-
      DO NLINE=1,J
        TRGMES = L1_NAME_LIST(NLINE)
        CALL PCTEXT_PST(NLINE,TRGMES)
      ENDDO
C---
C-
C--- Start scanning the trigger bits of level 2...
C-
      L2_NAME_LIST(1) = '*L2 Filt Name*'
      L2_NAME_LIST(2) = '--------------'
      J = 2
      IF ( NFILTON .GT. 0 ) THEN
        DO I = 1,NFILTON
          J = J + 1
          IF ( J .LE. 128 ) THEN
            L2_NAME_LIST(J) = FILTNON(I)
            IF (L2_NAME_LIST(J) .EQ. ' ') THEN
              J = J - 1
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C-
C--- Write L2 trigger name list...
C-
      DO NLINE=1,J
        TRGMES = L2_NAME_LIST(NLINE)
        CALL PCTEXT_PST(NLINE+17,TRGMES)
      ENDDO
C---
C-
  999 RETURN
      END
