C VAX/DEC CMS REPLACEMENT HISTORY, Element GZFCHT.FOR
C *1     9-NOV-1993 18:02:50 AVERY "fdc changes for v12 reco"
C VAX/DEC CMS REPLACEMENT HISTORY, Element GZFCHT.FOR
      FUNCTION GZFCHT()
C----------------------------------------------------------------------
C-
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FCHT,
C-     If not found in HITS bank, return pointer of L2 compressed hits.
C-
C-   Returned value  : pointer to Zebra bank FCHT
C-
C-   Created   2-NOV-1993   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFCHT.LINK'
      INTEGER GZFCHT, GZFDCH, GZCDH3, LFDCH,LFCHT
C----------------------------------------------------------------------
C
      LFCHT = 0
      LFDCH = GZFDCH()
      IF ( (LFDCH.GT.0).AND.IQ(LFDCH-2).EQ.4) THEN 
        LFCHT = LQ(LFDCH - IZFCHT)
      ENDIF
C
      IF ( LFCHT.LE.0  ) LFCHT = GZCDH3()
C
      GZFCHT = LFCHT
  999 RETURN
      END
