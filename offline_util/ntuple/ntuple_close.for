      SUBROUTINE NTUPLE_CLOSE(TOP_DIRECTORY,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write out the last buffer of
C-                          all remaining Ntuples, histograms, and close
C-                          file.
C-                          Use entry point
C-
C-                          NTUPLE_CLOSE_AND_DELETE(TOPDIR,STATUS)
C-
C-                          to close file and delete histograms from
C-                          sub-directories in memory, excluding histograms
C-                          in //PAWC.
C-   Inputs  :None
C-   Outputs :None
C-   Controls: none
C-
C-   Created  18-DEC-1991   Srini Rajagopalan, Harrison B. Prosper
C-   Updated   3-APR-1992   Harrison B. Prosper
C-      Remove check on counter
C-   Updated  17-JUN-1992   Harrison B. Prosper
C-      Remove closed files from list
C-   Updated   9-MAR-1993   Harrison B. Prosper  
C-      Add entry point NTUPLE_CLOSE_ID 
C-   Updated  29-NOV-1993   Rajendran Raja  Changed HREND to RZCLOS
C-                          This gives better interaction with H matrix RZ 
C-   Updated  27-DEC-1993   Rajendran Raja  revoking RZCLOS  for new HBOOK
C-                          CERNLIB
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TOP_DIRECTORY
      INTEGER STATUS
      INTEGER NTUPLE_ID
C
      INTEGER II,IDX
      INTEGER IUNIT
      INTEGER ICYCLE
      INTEGER IERR
      INTEGER NEXT,I,J,N,LTOP,IDD
      INTEGER NID, ID(*)
      LOGICAL FOUND, DELETE, LIST
C
      CHARACTER*32 TOPDIR
      CHARACTER*80 REMARK
      CHARACTER*132 RZPATH,PAWCPATH
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:NT_HOUSEKEEP.INC'
C----------------------------------------------------------------------
      LIST   = .FALSE.
      DELETE = .FALSE.
      GOTO 1
C
      ENTRY NTUPLE_CLOSE_ID(TOP_DIRECTORY,NID,ID,STATUS)
      LIST   = .TRUE.
      DELETE = .FALSE.
      GOTO 1
C
C ****  Alternate entry point
C
      ENTRY NTUPLE_CLOSE_AND_DELETE(TOP_DIRECTORY,STATUS)
      LIST   = .FALSE.
      DELETE = .TRUE.
C
    1 CONTINUE
C
C ****  Check if this top directory is known
C
      STATUS = 0
      LTOP = LEN(TOP_DIRECTORY)
      CALL LOCSTR(TOP_DIRECTORY(1:LTOP),TOPDIR_LIST,NFILES,FOUND,II)
      IF ( .NOT. FOUND ) THEN
        REMARK = ' Could not find top directory '//TOPDIR
        CALL ERRMSG('NOT_FOUND','NTUPLE_CLOSE',REMARK,'W')
        GOTO 999
      ENDIF
C
C ****  Get pointer into UNIT buffer
C
      IDX    = IDLIST(II)
      IUNIT  = UNIT_LIST(IDX)
      TOPDIR = '//'//TOP_DIRECTORY(1:LTOP)
      CALL WORD(TOPDIR,I,J,N)
C
      NEXT = 1
      DO WHILE ( NEXT .GT. 0 )
        CALL DHDIR_GET_NEXT_PATH(TOPDIR,RZPATH,NEXT)
        IF ( RZPATH(1:1) .NE. ' ' ) THEN
          IF ( NEW_FILE(IDX) ) THEN
            PAWCPATH = '//PAWC'//RZPATH(J+1:)
            CALL HCDIR(PAWCPATH,' ')        ! Set directory in memory
            CALL HCDIR(RZPATH,' ')          ! Set directory in RZ file
            IF ( LIST ) THEN
              DO I =  1, NID
                CALL HROUT(ID(I),ICYCLE,' ') ! Write out the last buffer
              ENDDO
            ELSE
              CALL HROUT(0,ICYCLE,' ')        ! Write out the last buffer
            ENDIF
            IF ( PAWCPATH .NE. '//PAWC' ) THEN
              IF ( DELETE ) THEN
                CALL HDELET(0)                ! Delete histograms etc.
              ENDIF
            ENDIF
          ENDIF
          CALL DHDIR_DELETE_RZPATH(RZPATH)
        ENDIF
      ENDDO
C
      CALL HREND(TOP_DIRECTORY(1:LTOP))   ! Close the direct access file
C
C ****  Close file
C
      CALL RLUNIT(IUSER,IUNIT,IERR)   ! Return the logical unit
      CLOSE(UNIT=IUNIT)
C
C ****  Remove this file from list of known files
C ****  Re-sort list of TOP directories
C
      TOPDIR_LIST(II) = '_'//TOPDIR_LIST(II)
      CALL SRTCHR(TOPDIR_LIST,NFILES,IDLIST)
C
      NFILES = NFILES - 1
      DO I =  1, NFILES
        IF ( I .GE. IDX ) THEN
          UNIT_LIST(I) = UNIT_LIST(I+1)
          NEW_FILE(I)  = NEW_FILE(I+1)
          FILE_LIST(I) = FILE_LIST(I+1)
          COUNTER(I)   = COUNTER(I+1)
        ENDIF
C
C ****  Reduce ID by one for those files with ID >= IDX
C
        IF ( IDLIST(I) .GE. IDX ) THEN
          IDLIST(I) = IDLIST(I) - 1
        ENDIF
      ENDDO
C
C ****  Un-declare file
C
      CALL DHDIR_DECLARE_FILE(' ')
C
  999 RETURN
      END
