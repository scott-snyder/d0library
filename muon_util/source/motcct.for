      SUBROUTINE MOTCCT(ILWC,ILWN,ILWS,ILON,ILOS,ILSN,ILSS)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Return all CCT latch words from MTRG bank
C-
C-    Input  :
C-
C-    Output :  ILWC   - CCT latch for WAMUS central
C-              ILWN   - CCT latch for WAMUS north
C-              ILWS   - CCT latch for WAMUS south
C-              ILON   - CCT latch for overlap north
C-              ILOS   - CCT latch for overlap south
C-              ILSN   - CCT latch for SAMUS north
C-              ILSS   - CCT latch for SAMUS south
C-
C-    Created :  29-JAN-94  M. Fortner
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER ILWC,ILWN,ILWS,ILON,ILOS,ILSN,ILSS
      INTEGER IVER,ISPARE,ITRIG,IDUM
C
      CALL GTMTRG(1,-2,IVER,ISPARE,ITRIG,ILWC,IDUM)
      CALL GTMTRG(2,-2,IVER,ISPARE,ITRIG,ILWN,ILON)
      CALL GTMTRG(3,-2,IVER,ISPARE,ITRIG,ILWS,ILOS)
      CALL GTMTRG(4,-2,IVER,ISPARE,ITRIG,ILSN,IDUM)
      CALL GTMTRG(5,-2,IVER,ISPARE,ITRIG,ILSS,IDUM)
C
      RETURN
      END
