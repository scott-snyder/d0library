      SUBROUTINE EZ_REPLACE_ARRAY(RCP_NAME,NEW_ARRAY,LINES,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : REPLACE AN EXISTING RCP ARRAY
C-   BY AN ARRAY .
C-
C-   Inputs  : RCP_NAME = NAME OF RCP ARRAY
C-             NEW_ARRAY = NEW ARRAY VALUES AS THOUGH THEY ARE IN A
C-             ASCII RCP FILE
C-             LINES = NUMBER OF LINES TAKEN BY THE NEW_ARRAY
C-             IN THE RCP FILE
C-   Outputs : IER = NON ZERO ON ERROR
C-   Controls:
C-
C-   Created  25-FEB-1991   Rajendran Raja
C-   Updated  12-Feb-1992   Herbert Greenlee
C-      UNIX version.  Got rid of machine blocks.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      CHARACTER*(*) RCP_NAME,NEW_ARRAY(*)
      CHARACTER*132 CTEMP
      INTEGER LINES,IER,TRULEN,I
      CHARACTER*1 QU
      DATA QU/''''/
      INTEGER IERR
C----------------------------------------------------------------------
      IER = 0
      CALL EZDELETE(RCP_NAME,IERR)
      IF ( IERR.EQ.0 ) THEN
        CTEMP = '\ARRAY '//RCP_NAME
        CALL EZADD(CTEMP,1,IERR)
        IER =IER + IERR
        DO I = 1 , LINES
          CTEMP = QU//NEW_ARRAY(I)(1:TRULEN(NEW_ARRAY(I)))//QU
          CALL EZADD(CTEMP(1:TRULEN(CTEMP)),1,IERR)
          IER =IER + IERR
        ENDDO
        CALL EZADD('\END',1,IERR)
        IER =IER + IERR
        CALL EZEND
        IF ( IER.EQ.EZS_BANK_EXTENDED ) THEN
          IER = 0
        ENDIF
        IF(IER.NE.0)THEN
          CALL ERRMSG(' EZ_PACKAGE','EZ_REPLACE_ARRAY',
     &    ' Error adding to bank','W')
        ENDIF
      ELSE
        IF(IERR.NE.EZS_PARAM_NOTFOUND)THEN
          CALL ERRMSG(' EZ_PACKAGE','EZ_REPLACE_ARRAY',
     &    ' Error deleting array','W')
        ENDIF
      ENDIF
  999 RETURN
      END
