      PROGRAM VDBPUT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read in results of VTXCOFF offline calibration and
C-                         put it into the DBL3 database.
C-
C-   Inputs  : Calibration constants in files created by VTXCOFF
C-   Outputs : Same constants to VTX calibration DBL3 database
C-   Controls: in VTXCOFF_RCP
C-
C-   Created   9-OCT-1992   Peter M. Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVTMH.LINK'
      INCLUDE 'D0$LINKS:IZVGNH.LINK'
      INCLUDE 'D0$LINKS:IZVPDH.LINK'
C
      INTEGER IER, LEN, ILEN, IUSER, LUN
      INTEGER GZVTMH, GZVGNH, GZVPDH, GZSVTX
      CHARACTER*50 VTMH_FILE, VGNH_FILE, VPDH_FILE
      CHARACTER*10 CHOPT
      LOGICAL WVTMH_DBL3, WVGNH_DBL3, WVPDH_DBL3, OPENED
C
      DATA IUSER / 666 /
C----------------------------------------------------------------------
C
C ****  Initialize ZEBRA stuff and read in VTX STP file
C
      CALL MZEBRA(0)
      CALL INZSTP
      CALL VTISTP('D0$STP:VTX_D0STPFILE.DAT',IER)
C
C ****  Read in RCP file and get parameters
C
      CALL INRCP('VTXCOFF_RCP',IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('INRCP error','VDBPUT',
     &    'VTXCOFF_RCP not found','F')
      ENDIF
      CALL EZPICK('VTXCOFF_RCP')
      CALL EZGET('WVTMH_DBL3',WVTMH_DBL3,IER)
      CALL EZGETS('VTMH_FILE',1,VTMH_FILE,LEN,IER)
      CALL EZGET('WVGNH_DBL3',WVGNH_DBL3,IER)
      CALL EZGETS('VGNH_FILE',1,VGNH_FILE,LEN,IER)
      CALL EZGET('WVPDH_DBL3',WVPDH_DBL3,IER)
      CALL EZGETS('VPDH_FILE',1,VPDH_FILE,LEN,IER)
      CALL EZRSET
C
      CALL GTUNIT(IUSER,LUN,IER)
C
C ****  Tzeros:
C
      IF ( WVTMH_DBL3 ) THEN
C
C ****  Drop old VTMH bank and read in new VTMH structure
C
        LVTMH = GZVTMH()
        IF ( LVTMH .GT. 0 ) CALL MZDROP(IXSTP,LVTMH,' ')
        CALL D0OPEN(LUN,VTMH_FILE,'IX',OPENED)
        IF ( .NOT. OPENED ) THEN
          CALL ERRMSG('File open error','VDBPUT',
     &      'Cannot open VTMH file','W')
          GO TO 100
        ENDIF
        CALL XZRECL(ILEN,CHOPT)
        CALL FZFILE(LUN,ILEN,CHOPT)
        LSVTX = GZSVTX()
        CALL FZIN(LUN,IDVSTP,LSVTX,-IZVTMH,' ',0,0)
        LVTMH = GZVTMH()
        CALL FZENDI(LUN,'QT')
        CLOSE(LUN)
C
C ****  Now put the time banks into the database
C
        CALL VSTP_INSERT('TIMES',IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG('DBL3 Error','VDBPUT',
     &      'Error writing TIMES into DBL3 database','W')
        ENDIF
      ENDIF
C
C ****  Gains:
C
  100 IF ( WVGNH_DBL3 ) THEN
C
C ****  Drop old VGNH bank and read in new VGNH structure
C
        LVGNH = GZVGNH()
        IF ( LVGNH .GT. 0 ) CALL MZDROP(IXSTP,LVGNH,' ')
        CALL D0OPEN(LUN,VGNH_FILE,'IX',OPENED)
        IF ( .NOT. OPENED ) THEN
          CALL ERRMSG('File open error','VDBPUT',
     &      'Cannot open VGNH file','W')
          GO TO 200
        ENDIF
        CALL XZRECL(ILEN,CHOPT)
        CALL FZFILE(LUN,ILEN,CHOPT)
        LSVTX = GZSVTX()
        CALL FZIN(LUN,IDVSTP,LSVTX,-IZVGNH,' ',0,0)
        LVGNH = GZVGNH()
        CALL FZENDI(LUN,'QT')
        CLOSE(LUN)
C
C ****  Now put the time banks into the database
C
        CALL VSTP_INSERT('GAINS',IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG('DBL3 Error','VDBPUT',
     &      'Error writing GAINS into DBL3 database','W')
        ENDIF
      ENDIF
C
C ****  Pedestals:
C
  200 IF ( WVPDH_DBL3 ) THEN
C
C ****  Drop old VPDH bank and read in new VPDH structure
C
        LVPDH = GZVPDH()
        IF ( LVPDH .GT. 0 ) CALL MZDROP(IXSTP,LVPDH,' ')
        CALL D0OPEN(LUN,VPDH_FILE,'IX',OPENED)
        IF ( .NOT. OPENED ) THEN
          CALL ERRMSG('File open error','VDBPUT',
     &      'Cannot open VPDH file','W')
          GO TO 300
        ENDIF
        CALL XZRECL(ILEN,CHOPT)
        CALL FZFILE(LUN,ILEN,CHOPT)
        LSVTX = GZSVTX()
        CALL FZIN(LUN,IDVSTP,LSVTX,-IZVPDH,' ',0,0)
        LVPDH = GZVPDH()
        CALL FZENDI(LUN,'QT')
        CLOSE(LUN)
C
C ****  Now put the time banks into the database
C
        CALL VSTP_INSERT('PEDESTALS',IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG('DBL3 Error','VDBPUT',
     &      'Error writing PEDESTALS into DBL3 database','W')
        ENDIF
      ENDIF
C
C ****  Done!
C
  300 CALL RLUNIT(IUSER,LUN,IER)
C
      END
