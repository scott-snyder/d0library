      SUBROUTINE VPDH_READ(OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Replace VTX pedestal banks (VPDH on down) with
C-                         structure read from external ZEBRA file.
C-
C-   Inputs  : filename from RCP
C-   Outputs : OK: .TRUE. if all goes well
C-
C-   Created  19-OCT-1992   Peter M. Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVPDH.LINK'
C
      INTEGER IER, LUN, LEN, GZVPDH, IUSER
      CHARACTER*60 FILENAME
      CHARACTER*10 CHOPT
      LOGICAL OK, FIRST, OPENED
      DATA IUSER / 666 /
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      OK = .TRUE.
      IF ( .NOT. FIRST ) GO TO 999
      FIRST = .FALSE.
C
      CALL EZPICK('VTRAKS_RCP')
      CALL EZGETS('VPDHFILE',1,FILENAME,LEN,IER)
      CALL EZRSET
C
      CALL GTUNIT(IUSER,LUN,IER)
      CALL D0OPEN(LUN,FILENAME,'IX',OPENED)
      IF ( .NOT. OPENED ) THEN
        CALL ERRMSG('File open error','VPDH_READ',
     &    'Unable to open VPDH file','W')
        OK = .FALSE.
      ENDIF
      CALL XZRECL(LEN,CHOPT)
      CALL FZFILE(LUN,LEN,CHOPT)
C
C ****  Drop old VPDH structure and read in new one
C
      LVPDH = GZVPDH()
      IF ( LVPDH .GT. 0 ) CALL MZDROP(IXSTP,LVPDH,' ')
      CALL FZIN(LUN,IDVSTP,LSVTX,-IZVPDH,' ',0,0)
      LVPDH = GZVPDH()  ! Make sure LVPDH is defined
C
      CALL FZENDI(LUN,'QT')
      CLOSE(LUN)
      CALL RLUNIT(IUSER,LUN,IER)
C
  999 RETURN
      END
