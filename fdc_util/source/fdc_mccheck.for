C VAX/DEC CMS REPLACEMENT HISTORY, Element FDC_MCCHECK.FOR
C *1     4-NOV-1993 10:52:47 AVERY "FDC changes for v12 RECO"
C VAX/DEC CMS REPLACEMENT HISTORY, Element FDC_MCCHECK.FOR
      SUBROUTINE FDC_MCCHECK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check if MC data is old or new version.
C-      If old version, then replace new version of STP banks
C-      with old version (if they exist).
C-   RCP parameter STP_VERSION optionally allows the user to explicitly
C-      choose which STP bank version to use.
C-       STP_VERSION = 0  -> Choose default based on word in EVENT_HEADER.
C-       STP_VERSION > 0  -> Use STP_VERSION 
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  13-AUG-1992   Robert E. Avery
C-   Updated   9-SEP-1992   Robert E. Avery  Do not use time to determine
C-                      version, since time is too machine dependent,
C-   Updated  15-OCT-1992   Robert E. Avery   Add RCP parameter to 
C-       allow the user to choose which STP bank version to use.
C-       STP_VERSION = 0  -> Choose default as before
C-       STP_VERSION > 0  -> Use STP_VERSION 
C-   Updated   4-DEC-1992   Robert E. Avery   Also look for vers. in EVENT_HEAD 
C-   Updated   1-FEB-1993   Robert E. Avery  Only look in EVENT_HEAD,
C-                              since data generated with vers. in HSTR 
C-                              was corrupt anyway.
C-   Updated  30-SEP-1993   Robert E. Avery  Check that LHEAD is pointing
C-                              to EVENT_HEAD, not RUN_HEAD!
C-                              (This could happen in d0reco and caloroff).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZFGEH.LINK'
      INCLUDE 'D0$LINKS:IZFALH.LINK'
      INCLUDE 'D0$LINKS:IZFTMH.LINK'
      INCLUDE 'D0$LINKS:IZGEAN.LINK'
      INCLUDE 'D0$LINKS:IZHSTR.LINK'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
C
      INTEGER LKSFDC,GZSFDC
      INTEGER LGEAN,LHSTR
      INTEGER IER
      INTEGER VERSION,VERSION_CURRENT
      INTEGER ND
      INTEGER LZFIND
      INTEGER LKFGEH,LKFALH,LKFTMH
      INTEGER LRCP
C
      INTEGER IVERSION
      BYTE BVERSION(4)
      EQUIVALENCE(BVERSION,IVERSION)
C
      LOGICAL SHIFT_FDC,FALH_MOVED,MCDATA,MC_EVENT
      LOGICAL FIRST
      SAVE    FIRST
C
      DATA    FIRST,SHIFT_FDC /.TRUE.,.FALSE./
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
C
        IF ( LHEAD.LE.0 ) GOTO 999
        LKSFDC=GZSFDC()
        IF ( LKSFDC .LE.0 ) GOTO 999
C
        MCDATA = IQ(LHEAD+1) .GE. 1001
        IF (.NOT.MCDATA) THEN 
          FIRST = .FALSE.
          GOTO 999
        ENDIF
C
        MC_EVENT = IQ(LHEAD+1) .GE. 1005
        IF (.NOT.MC_EVENT ) GOTO 999
C
        FIRST = .FALSE.
C
C Get Version number of MC data.
C
C First check for RCP parameter:
C
        CALL EZLOC('FTRAKS_RCP',LRCP)
        IF (LRCP.GT.0) THEN
          CALL EZPICK('FTRAKS_RCP')
          CALL EZGET('STP_VERSION',VERSION,IER)
          CALL EZGET('SHIFT_FDC',SHIFT_FDC,IER)
          CALL EZRSET
        ENDIF
C
        IF ( VERSION.LE.0 ) THEN
C
C  Look at word in EVENT_HEAD bank. (Version in HSTR word is not reliable)
C
          IVERSION = IQ(LHEAD+13)         ! READ VERSION NUMBER
          VERSION = BVERSION(BYTE2)       ! FDC is in second byte
          IF ( VERSION.NE.2 ) VERSION=1
C
        ELSEIF ( VERSION.EQ.1 ) THEN
          IF (IQ(LHEAD+13).NE.0) THEN 
            CALL ERRMSG(
     &        'FDC-MC-Wrong-STP-Banks','FDC_MCCHECK',
     &        'Inconsistant STP banks for V2 FDC MC Data,'//
     &        ' check STP_VERSION in FTRAKS_RCP ','W')
          ENDIF
        ELSEIF ( VERSION.GE.2 ) THEN
          VERSION = 2
          IF (IQ(LHEAD+13).EQ.0) THEN 
            CALL ERRMSG(
     &        'FDC-MC-Wrong-STP-Banks','FDC_MCCHECK',
     &        'Inconsistant STP banks for V1 FDC MC Data,'//
     &        ' check STP_VERSION in FTRAKS_RCP ','W')
          ENDIF
        ENDIF
C
C  Select correct version of FDC STP banks.
C
        LKFGEH = LC(LSFDC-IZFGEH)   ! Don't use GZXXXX (could be recursive)
        IF ( LKFGEH.GT.0 ) THEN
          VERSION_CURRENT = IC(LKFGEH-5)
          IF ( VERSION.NE.VERSION_CURRENT ) THEN
            LKFGEH = LZFIND(IXSTP,LKFGEH,VERSION,-5)
            IF ( LKFGEH.GT.0 ) THEN
              CALL ZSHUNT(IXSTP,LKFGEH ,LSFDC,-IZFGEH,0)
            ENDIF
          ENDIF
        ENDIF
C
        LKFALH = LC(LSFDC-IZFALH)
        FALH_MOVED = .FALSE.
        IF ( LKFALH.GT.0 ) THEN
          VERSION_CURRENT = IC(LKFALH-5)
          IF ( VERSION.NE.VERSION_CURRENT ) THEN
            LKFALH = LZFIND(IXSTP,LKFALH,VERSION,-5)
            IF ( LKFALH.GT.0 ) THEN
              CALL ZSHUNT(IXSTP,LKFALH ,LSFDC,-IZFALH,0)
              FALH_MOVED = .TRUE.
            ENDIF
          ENDIF
        ENDIF
C
        LKFTMH = LC(LSFDC-IZFTMH)
        IF ( LKFTMH.GT.0 ) THEN
          VERSION_CURRENT = IC(LKFTMH-5)
          IF ( VERSION.NE.VERSION_CURRENT ) THEN
            LKFTMH = LZFIND(IXSTP,LKFTMH,VERSION,-5)
            IF ( LKFTMH.GT.0 ) THEN
              CALL ZSHUNT(IXSTP,LKFTMH ,LSFDC,-IZFTMH,0)
            ENDIF
          ENDIF
        ENDIF
C
C Check consistancy of STP banks
C
        CALL FZERO_FDCPRM
        LKFGEH = LC(LSFDC-IZFGEH)
C
        IF ( VERSION.EQ.1 ) THEN
          IF ( LKFGEH.GT.0 ) THEN
            IF ( IC(LKFGEH+10) .EQ. 0 ) THEN
              IC ( LKFGEH + 9 )  = 9001
              IC ( LKFGEH + 10 ) = 9002
            ELSEIF ( IC(LKFGEH+10) .NE. 9002 ) THEN
              CALL ERRMSG('FDC-wrong-STP-file','FDC_MCCHECK',
     &            'Inconsistant STP banks for V1 FDC MC Data,'//
     &            ' check FDC_MCSTPFILE.DAT','W')
            ENDIF
          ENDIF
C
        ELSE
          IF ( LKFGEH.GT.0 ) THEN
            IF ( IC(LKFGEH+10) .NE. 9009 ) THEN
              CALL ERRMSG('FDC-wrong-STP-file','FDC_MCCHECK',
     &            'Inconsistant STP banks for V2 FDC MC Data,'//
     &            ' check FDC_MCSTPFILE.DAT','W')
            ENDIF
          ENDIF
        ENDIF
C
C  Re-perform z-shift (if necessary).
C
        IF ( FALH_MOVED.AND.SHIFT_FDC  ) THEN
          CALL FDC_SHIFT
        ENDIF
C
      ENDIF
C
  999 RETURN
      END
