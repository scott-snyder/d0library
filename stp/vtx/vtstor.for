      SUBROUTINE VTSTOR(LUN,FILENAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Store VTX STP structure to an external file.
C-
C-   Inputs  : LUN, FILENAME
C-   Outputs : file is written
C-   Controls: in VTWSTP_RCP
C-
C-   Created   2-AUG-1992   Peter M. Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER LUN
      CHARACTER*(*) FILENAME
C
      INTEGER IER, ILEN
      CHARACTER*10 CHOPT
      LOGICAL OK, XCHANGE
C----------------------------------------------------------------------
C
      CALL EZPICK('VTWSTP_RCP')
      CALL EZGET('XCHANGE',XCHANGE,IER)
      CALL EZRSET
C
      IF ( XCHANGE ) THEN
        CALL D0OPEN(LUN,FILENAME,'OG',OK)
      ELSE
        CALL D0OPEN(LUN,FILENAME,'OU',OK)
      ENDIF
      IF ( .NOT. OK ) THEN
        CALL ERRMSG('VTWSTP','VTSTOR',
     &    'Unable to open output STP file','F')
        GO TO 999
      ENDIF
      CALL XZRECL(ILEN,CHOPT)
      CALL FZFILE(LUN,ILEN,CHOPT)
C
      CALL FZOUT(LUN, IDVSTP, LSVTX, 1, ' ', 0, 0, 0)
C
      CALL FZENDO(LUN,'QT')
C
  999 RETURN
      END
