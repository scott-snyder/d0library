C VAX/DEC CMS REPLACEMENT HISTORY, Element MUDHIT.FOR
C *1    15-SEP-1993 17:48:41 DARIEN "New MF code for 1B MUD1"
C VAX/DEC CMS REPLACEMENT HISTORY, Element MUDHIT.FOR
      SUBROUTINE MUDHIT(IHIT,JHIT,NCEL,NLAT,IADC)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Extract one WAMUS hit from MUD1
C-
C-    Input  :  IHIT   - Hit location in MUHP
C-
C-    Output :  JHIT   - Next hit location in MUHP
C-              NCEL   - Cell address
C-              NLAT   - Latch bits
C-              IADC(8)- Raw ADC counts
C-
C-    Created :  31-AUG-93  M. Fortner
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IHIT,JHIT,NCEL,NLAT,IADC(8)
      INTEGER LEVE,LODD,LDUM,IDUM
      INTEGER IW1,IW2,IW3,IW4,IW5,IW6,IW7,IW8
C
C                Initialize
C
      IF (IHIT.EQ.0) THEN
          CALL GTMUHP(0,NCEL,NLAT,LEVE,LODD,JHIT)
          CALL GTMUD1(0,LEVE,LDUM,IDUM,IW1,IW2,
     &                IW3,IW4,IW5,IW6,IW7,IW8)
C
C                Get hit information
C
      ELSE
          CALL GTMUHP(IHIT,NCEL,NLAT,LEVE,LODD,JHIT)
          CALL GTMUD1(13,LEVE,LDUM,IDUM,IADC(1),IADC(2),
     &                IADC(3),IADC(4),IW5,IW6,IW7,IW8)
          CALL GTMUD1(13,LODD,LDUM,IDUM,IADC(5),IADC(6),
     &                IADC(7),IADC(8),IW5,IW6,IW7,IW8)
C
      ENDIF
C
      RETURN
      END
