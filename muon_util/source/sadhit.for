C VAX/DEC CMS REPLACEMENT HISTORY, Element SADHIT.FOR
C *1    30-OCT-1993 23:55:08 FORTNER "add samus hit finding"
C VAX/DEC CMS REPLACEMENT HISTORY, Element SADHIT.FOR
      SUBROUTINE SADHIT(IHIT,JHIT,NCEL,NLAT,IADC)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Extract one SAMUS hit from MUD1
C-
C-    Input  :  IHIT   - Hit location in MUHP
C-
C-    Output :  JHIT   - Next hit location in MUHP
C-              NCEL   - Cell address
C-              NLAT   - Latch bits
C-              IADC(2)- Raw ADC counts
C-
C-    Created :  27-SEP-93  M. Fortner
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IHIT,JHIT,NCEL,NLAT,IADC(2)
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
          CALL GTMUD1(13,LEVE,LDUM,IDUM,IADC(1),IW2,
     &                IW3,IW4,IW5,IW6,IW7,IW8)
          CALL GTMUD1(13,LODD,LDUM,IDUM,IADC(2),IW2,
     &                IW3,IW4,IW5,IW6,IW7,IW8)
      ENDIF
C
      RETURN
      END
