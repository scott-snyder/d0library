      SUBROUTINE L0_GET_VERSION(VERSION)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  13-SEP-1993   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER VERSION
      INTEGER LCRATE0,IVERSION,NDATA,DETAILS(5,7)
      INTEGER LKPLV0
      INTEGER JSTAT,NVERS
      INTEGER ISETVN,GZL0_CRATE,GZPLV0
      EXTERNAL ISETVN,GZL0_CRATE,GZPLV0
C
C----------------------------------------------------------------------
C
      IVERSION = 0
      LCRATE0=GZL0_CRATE()
C
      IF ( LCRATE0.GT.0 ) THEN
        IVERSION = 0
        CALL L0_DECODE_HEADER(LCRATE0,IVERSION,NDATA,DETAILS)
        IVERSION = DETAILS(4,3)
      ELSE
        LKPLV0 = GZPLV0()
        IF ( LKPLV0.GT.0 ) THEN
          NVERS=0
          JSTAT = ISETVN(IQ(LKPLV0),NVERS)
          IVERSION = IBITS(JSTAT,13,5)
        ENDIF
      ENDIF
C
      IF ( IVERSION.LT.1 .OR. IVERSION.GT.2 ) IVERSION=1
      VERSION = IVERSION
C----------------------------------------------------------------------
  999 RETURN
      END
