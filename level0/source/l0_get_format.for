      SUBROUTINE L0_GET_FORMAT(FORMAT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Recover from version number / data format
C-                         inconsistencies.
C-
C-   Inputs  : none
C-   Outputs : FORMAT = 1.0 for Run 1a data, 1.5 for Run 1b data with
C-                      version=2 but 1a data format, 2.0 for Run 1b 
C-                      data with 1b format.
C-   Controls: none
C-
C-   Created  13-SEP-1993   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LCRATE0,IFORMAT,NDATA,DETAILS(5,7)
      INTEGER LKPLV0
      INTEGER JSTAT,NVERS,NBLOCKS
      INTEGER ISETVN,GZL0_CRATE,GZPLV0,VERSION
      EXTERNAL ISETVN,GZL0_CRATE,GZPLV0
C
      REAL FORMAT
C
C----------------------------------------------------------------------
C
      LCRATE0=GZL0_CRATE()
C
      IF ( LCRATE0.GT.0 ) THEN
        IFORMAT = 0
        CALL L0_DECODE_HEADER(LCRATE0,IFORMAT,NDATA,DETAILS)
        VERSION = DETAILS(4,3)
        NBLOCKS = DETAILS(3,2)
      ELSE
        LKPLV0 = GZPLV0()
        IF ( LKPLV0.GT.0 ) THEN
          NVERS=0
          JSTAT = ISETVN(IQ(LKPLV0),NVERS)
          VERSION = IBITS(JSTAT,13,5)
          NBLOCKS = 0
        ENDIF
      ENDIF
C
      FORMAT = FLOAT(VERSION)
      IF ( VERSION.LT.1 .OR. VERSION.GT.2 ) FORMAT=1.0
      IF ( FORMAT.EQ.2.0 .AND. NBLOCKS.EQ.25 ) FORMAT = 1.5
C----------------------------------------------------------------------
  999 RETURN
      END
