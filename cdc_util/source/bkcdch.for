      SUBROUTINE BKCDCH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the banks up to CDCH, hanging from the
C-              bank 'LNKNAM', either 'GEAN' or 'RECO'. Initialize the
C-              CDC link area.
C-
C-   Inputs  : 
C-   Outputs : none, bank CDCH and link area initialised.
C-
C-   Created   4-FEB-1988   Olivier Callot
C-   Updated  12-JUL-1989   Qizhong Li-Demarteau    fix bug in MZLINT,
C-                                      use PATHGT and put in version #
C-   Updated   3-OCT-1989   Qizhong Li-Demarteau  add reference link to
C-                                               history and use ERRMSG
C-   Updated  27-NOV-1989   Qizhong Li-Demarteau  check on MZLINT added
C-   Updated  08-NOV-1990   Qizhong Li-Demarteau  added new words in CDCH
C-   Updated  11-JUL-1991   Qizhong Li-Demarteau  added down link to DHIT 
C-   Updated  12-AUG-1992   Qizhong Li-Demarteau  to handle more paths
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
      INCLUDE 'D0$LINKS:IZCDCH.LINK'
      CHARACTER*60 ERMSG
      CHARACTER*4 LNKNAM
      INTEGER LTHITS, NWDSEC, NWDCDA, ISETVN
      INTEGER MPCDCH(5)
      INTEGER GZHITS, GZCDCH, GZHSTR
      LOGICAL START
      SAVE    START
      DATA    START / .TRUE. /
      DATA MPCDCH / 0, 6, 5, 10, 2 /
C----------------------------------------------------------------------
C
C ****  Initialise the LINK area
C
      IF ( START ) THEN
        START = .FALSE.
        CALL UCTOH( 'CDCH', MPCDCH(1), 4, 4 )
      ENDIF
      IF (CDCLNK(1) .EQ. 0) THEN
        CALL MZLINT(IXCOM,  '/CDCLNK/', CDCLNK, LCDD2, CDCLNK)
      ENDIF
C
C ****  Test for HEAD bank, abort if doesn't exist
C
      IF ( LHEAD .EQ. 0 ) THEN
        WRITE(ERMSG, 1100)
 1100   FORMAT(1X,' Can not book CDCH because HEAD bank doesn''t exist')
        CALL ERRMSG('CDC','BKCDCH',ERMSG,'F')
      ENDIF
C
C ****  Book HITS bank, if needed
C
      LTHITS = GZHITS()
      IF (LTHITS.LE.0) CALL BKHITS(LTHITS)
C
C ****  Book CDCH bank
C
      LCDCH = GZCDCH()
      IF ( LCDCH .EQ. 0 ) THEN
        CALL MZLIFT ( IXMAIN, LCDCH, LTHITS, -IZCDCH, MPCDCH, 0 )
      ENDIF
      LQ(LCDCH - 6) = GZHSTR()        ! Reference Link to latest History
      IQ(LCDCH) = ISETVN(IQ(LCDCH),0)
C
C ****  and store DSEC,DCDA length inside
C
C
      CALL PATHGT(LNKNAM)
      IF ( LNKNAM .EQ. 'GEAN' ) THEN
        NWDSEC = 10
        NWDCDA = 9
      ELSE
        NWDSEC = 12
        NWDCDA = 8
      ENDIF
      IQ(LCDCH + 2) = NWDSEC
      IQ(LCDCH + 3) = 7               ! Number of wires / sector
      IQ(LCDCH + 4) = NWDCDA
      IQ(LCDCH + 5) = 11              ! Number of FADC / sector
C
  999 CONTINUE
      RETURN
      END
