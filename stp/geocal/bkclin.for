C DEC/CMS REPLACEMENT HISTORY, Element BKCLIN.FOR
C *5    19-AUG-1992 21:19:34 STEWART "add NFIT and CHISQ to CLIN"
C *4    14-AUG-1992 10:46:59 KAHN "UPDATE"
C *3    12-AUG-1992 22:04:34 KAHN "BETA-1"
C *2    17-AUG-1991 17:13:05 KAHN "corrected zebra problem"
C *1    12-JUN-1991 15:29:36 KAHN "survey related routines"
C DEC/CMS REPLACEMENT HISTORY, Element BKCLIN.FOR
      SUBROUTINE BKCLIN(JCLIN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book 'CLIN' bank.
C-
C-   Inputs  : 
C-   Outputs :       JCLIN
C-   Controls: 
C-
C-   Created  20-MAR-1991   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:CLIN.PARAMS'
      INCLUDE 'D0$LINKS:IZCLIN.LINK'
      INTEGER JCLIN, MCLIN(5), IOCLIN
C
      DATA MCLIN /'CLIN', 1, 0, 30, 9 /
C
      CALL MZFORM('CLIN','2I1F8I-F',IOCLIN)
      MCLIN(5) = IOCLIN
      CALL MZLIFT(IDVSTP, JCLIN, LCGEH, -IZCLIN, MCLIN, 0)
C----------------------------------------------------------------------
  999 RETURN
      END
