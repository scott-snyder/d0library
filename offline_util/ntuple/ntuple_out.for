      SUBROUTINE NTUPLE_OUT(TOP_DIRECTORY,NID,NTUPLE_ID,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write out the last buffer of specified
C-                          Ntuples, histograms, DELETE them and then
C-                          close the file. See also NTUPLE_CLOSE.
C-
C-   Inputs  : TOP_DIRECTORY  [C*]  Top-directory of file to be closed
C-             NID            [I]   Number of Hist./Ntuples
C-             NTUPLE_ID(*)   [I]   Hist./Ntuple IDs
C-   Outputs : STATUS         [I]   0 - OK
C-   Controls: none
C-
C-   Created   7-JUL-1992   Harrison B. Prosper, Jaehoon Yu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TOP_DIRECTORY
      INTEGER NID, NTUPLE_ID(*)
      INTEGER STATUS
C----------------------------------------------------------------------
      INTEGER II,IDX
      INTEGER IUNIT
      INTEGER ICYCLE
      INTEGER IERR
      INTEGER NEXT,I,J,N,LTOP,IDD
      LOGICAL FOUND, EXIST, HEXIST
C
      CHARACTER*32 TOPDIR
      CHARACTER*80 REMARK
      CHARACTER*132 RZPATH,PAWCPATH
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:NT_HOUSEKEEP.INC'
C----------------------------------------------------------------------
C
C ****  Check if this top directory is known
C
      STATUS = 0
      LTOP = LEN(TOP_DIRECTORY)
      CALL LOCSTR(TOP_DIRECTORY(1:LTOP),TOPDIR_LIST,NFILES,FOUND,II)
      IF ( .NOT. FOUND ) THEN
        REMARK = ' Could not find top directory '//TOPDIR
        CALL ERRMSG('NOT_FOUND','NTUPLE_OUT',REMARK,'W')
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
      IF ( NID .LE. 0 ) THEN
        REMARK = ' Go boil your head...silly NID'
        CALL ERRMSG('ZERO_NID','NTUPLE_OUT',REMARK,'W')
        GOTO 999
      ENDIF
C
      NEXT = 1
      DO WHILE ( NEXT .GT. 0 )
        CALL DHDIR_GET_NEXT_PATH(TOPDIR,RZPATH,NEXT)
        IF ( RZPATH(1:1) .NE. ' ' ) THEN
          IF ( NEW_FILE(IDX) ) THEN
            PAWCPATH = '//PAWC'//RZPATH(J+1:)
            CALL HCDIR(PAWCPATH,' ')        ! Set directory in memory
            CALL HCDIR(RZPATH,' ')          ! Set directory in RZ file
            DO I = 1, NID
              EXIST = HEXIST(NTUPLE_ID(I)) .OR. (NTUPLE_ID(I) .EQ. 0)
              IF ( EXIST ) THEN
                CALL HROUT(NTUPLE_ID(I),ICYCLE,' ')! Write out the last buffer
                CALL HDELET(NTUPLE_ID(I))      ! Delete histograms etc.
              ENDIF
            ENDDO
          ENDIF
          CALL DHDIR_DELETE_RZPATH(RZPATH)
        ENDIF
      ENDDO
C
      CALL HREND(TOP_DIRECTORY(1:LTOP))     ! Close the direct access file
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
