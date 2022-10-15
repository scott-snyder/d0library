      SUBROUTINE DO_HBOOK_OPEN(PARAM,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Open one of more HBOOK RZ files. The
C-   files are specified in the parameter PARAM of the current RCP
C-   bank.
C-
C-   Inputs  : PARAM  [C*]  Name of open-file parameter
C-
C-   Outputs : STATUS [I]   0 --- OK
C-
C-   Controls: None
C-
C-   Entry points:
C-
C-   DO_HBOOK_CLOSE
C-
C-   This routine MUST be called to close the RZ file.
C-
C-   Format of parameter:
C-
C-   \ARRAY HBOOK_OPEN
C-      'file-name' 'top-directory' [newfile] [record-length]
C-        :   :
C-   \END
C-
C-   If the NEWFILE switch is omitted then the default is NEWFILE=TRUE
C-   If the record-length is omitted then the default is 8191.
C-
C-   Created   3-DEC-1991   Harrison B. Prosper
C-   Updated  19-DEC-1991   Harrison B. Prosper  
C-      Call DHDIR_DECLARE_FILE to define default file to which
C-      histograms etc. should go. The default is the first file
C-      opened by DO_HBOOK_OPEN 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PARAM
      INTEGER STATUS
C----------------------------------------------------------------------
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
      INTEGER II,JJ,LAST_II,IER
      INTEGER LPARAM,LENGTH,NWORDS,LREC,ITYPE,CTRL,IVAL
C
      INTEGER DO_HBOOK_ID
      PARAMETER( DO_HBOOK_ID = 70707 )
C
      CHARACTER*32 TOPDIR,CVAL
      CHARACTER*80 FILENAME,REMARK
      LOGICAL FILE_OPENED,FIRST,NEWFILE
C----------------------------------------------------------------------
      SAVE FILE_OPENED,FIRST
      DATA FILE_OPENED/.FALSE./
      DATA FIRST      /.TRUE./
C----------------------------------------------------------------------
C
      LPARAM = LEN(PARAM)
C
      STATUS = 0
      II   = 1
      CTRL = 0
      DO WHILE ( CTRL .EQ. 0 )
C
C ****  Get FILENAME
C
        CALL EZGET_NEXT_VALUE_TYPE
     &    (PARAM(1:LPARAM),IVAL,FILENAME,ITYPE,LENGTH,CTRL,II)
        FILENAME = FILENAME(1:LENGTH)
C
        IF ( ITYPE .LT. VTCHR ) THEN
          REMARK = ' Bad syntax in parameter '//PARAM(1:LPARAM)
          CALL ERRMSG('BAD_SYNTAX','DO_HBOOK_OPEN',REMARK,'W')
          STATUS =-1
          GOTO 999
        ENDIF
C
C ****  Get TOP DIRECTORY
C
        IF ( CTRL .EQ. 0 ) THEN
          CALL EZGET_NEXT_VALUE_TYPE
     &      (PARAM(1:LPARAM),IVAL,TOPDIR,ITYPE,LENGTH,CTRL,II)
          IF ( ITYPE .LT. VTCHR ) THEN
            REMARK = ' Bad syntax in parameter '//PARAM(1:LPARAM)
            CALL ERRMSG('BAD_SYNTAX','DO_HBOOK_OPEN',REMARK,'W')
            STATUS =-1
            GOTO 999
          ENDIF
          TOPDIR = TOPDIR(1:LENGTH)
        ELSE
          TOPDIR = FILENAME   !DEFAULT
        ENDIF
C
C ****  Get NEWFILE switch for file
C
        LAST_II = II
        IF ( CTRL .EQ. 0 ) THEN
          CALL EZGET_NEXT_VALUE_TYPE_l
     &      (PARAM(1:LPARAM),NEWFILE,CVAL,ITYPE,LENGTH,CTRL,II)
          IF ( ITYPE .NE. VTLOG ) THEN
            NEWFILE = .TRUE.
            II = LAST_II
          ENDIF
        ELSE
          NEWFILE = .TRUE.
        ENDIF
C
C ****  Get RECORD LENGTH
C
        LAST_II = II
        IF ( CTRL .EQ. 0 ) THEN
          CALL EZGET_NEXT_VALUE_TYPE
     &      (PARAM(1:LPARAM),LREC,CVAL,ITYPE,LENGTH,CTRL,II)
          IF ( ITYPE .NE. VTINT ) THEN
            LREC = 8191
            II = LAST_II
          ENDIF
          IF ( LREC .LE. 0 ) THEN
            LREC = 8191
          ENDIF
        ELSE
          LREC = 8191
        ENDIF
C
        CALL NTUPLE_FILE_OPEN(DO_HBOOK_ID,
     &                        NEWFILE,
     &                        FILENAME,
     &                        LREC,
     &                        TOPDIR,
     &                        IER)
        IF ( IER .EQ. 0 ) THEN
          FILE_OPENED = .TRUE.
C
C ****  Declare first file to DHDIR
C
          IF ( FIRST ) THEN
            FIRST = .FALSE.
            CALL DHDIR_DECLARE_FILE(TOPDIR,IER)
          ENDIF
        ELSE
          REMARK = ' Unable to open RZ file '//FILENAME
          CALL ERRMSG('BAD_OPEN','DO_HBOOK_OPEN',REMARK,'W')
          STATUS = IER
        ENDIF
      ENDDO
C
  999 RETURN
C
C ****  Entry point to close RZ files
C
      ENTRY DO_HBOOK_CLOSE
      IF ( FILE_OPENED ) THEN
        CALL NTUPLE_END
      ENDIF
      END
