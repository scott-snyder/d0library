      SUBROUTINE NTUPLE_FLUSH (TOP_DIRECTORY,NTUPLE_ID,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write out the last buffer of an Ntuple,
C-                         identified by its ID to disk and close
C-                         the file identified by the specified top
C-                         directory if no more Ntuples to be written.
C-
C-   Inputs  : TOP_DIRECTORY
C-             NTUPLE_ID                 ! Integer ID of the Ntuple
C-   Outputs : STATUS                    ! error Status,if NE. 0 Then error
C-   Controls: none
C-
C-   Created  25-JUN-1991   B.S.Acharya
C-   Updated   5-NOV-1991   Harrison B. Prosper
C-    use NTUPLE_FILE_SELECT1
C-   Updated   7-NOV-1991   Harrison B. Prosper
C-    fix counter bug
C-   Updated   3-JAN-1992   Harrison B. Prosper
C-    fix RZ/PAWC order bug
C-   Updated   3-APR-1992   Harrison B. Prosper  
C-      Set file_list = ' ' if file is closed 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER ICYCLE,LTOP
      INTEGER NTUPLE_ID
      INTEGER IUNIT
      INTEGER IDX
      INTEGER STATUS
C
      CHARACTER*(*) TOP_DIRECTORY
C----------------------------------------------------------------------
      INTEGER I,J,K,N,II
      LOGICAL FOUND
      CHARACTER*80 REMARK,CURDIR,NEWDIR,PAWCPATH,RZPATH
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:NT_HOUSEKEEP.INC'
C----------------------------------------------------------------------
      STATUS = 0
      IF ( NTUPLE_ID .LT. 0 ) GOTO 999
C
C ****  Check if this top directory is known
C
      LTOP = LEN(TOP_DIRECTORY)
      CALL LOCSTR(TOP_DIRECTORY(1:LTOP),TOPDIR_LIST,NFILES,FOUND,II)
      IF ( .NOT. FOUND ) THEN
        REMARK = ' Could not find top directory '//TOP_DIRECTORY(1:LTOP)
        CALL ERRMSG('NOT_FOUND','NTUPLE_CLOSE',REMARK,'W')
        GOTO 999
      ENDIF
C
C ****  Get pointer into UNIT buffer
C
      IDX = IDLIST(II)
C
C ****  Decrement ntuple count
C
      IF ( COUNTER(IDX) .GT. 0 ) THEN
        COUNTER(IDX) = COUNTER(IDX)-1
      ENDIF
C
C ****  Get pathname in PAWC.
C
      CALL HCDIR(CURDIR,'R')          ! Get current directory
      CALL WORD(CURDIR,I,J,N)
      K = INDEX(CURDIR(3:J),'/')
      IF ( K .GT. 0 ) THEN
        K = K + 2
        NEWDIR = CURDIR(K:J)          ! Get path beneath current top directory
      ELSE
        NEWDIR = ' '
      ENDIF
      CALL WORD(TOP_DIRECTORY(1:LTOP),I,J,N)
      PAWCPATH = '//PAWC'//NEWDIR       ! Get pathname in PAWC
      RZPATH   = '//'//TOP_DIRECTORY(I:J)//NEWDIR       ! Get pathname in PAWC
C
C ****  Set the directory in PAWC then in RZ

      CALL HCDIR(PAWCPATH,' ')  ! Set directory in memory
      CALL HCDIR(RZPATH,' ')    ! Set directory in RZ file
      CALL HROUT(NTUPLE_ID,ICYCLE,' ')     ! write out the last buffer
      CALL HDELET(NTUPLE_ID)               ! delete the Ntuple in memory
C
C ****  Close file if all ntuples written
C
      IF ( (COUNTER(IDX) .LE. 0) .OR. (NTUPLE_ID .EQ. 0) ) THEN
        COUNTER(IDX) = 0
        FILE_LIST(IDX) = ' '
        IUNIT  = UNIT_LIST(IDX)
        CALL HREND(TOP_DIRECTORY(1:LTOP))     ! Close the Direct Access file
        CALL RLUNIT(IUSER,IUNIT,STATUS)   ! return the logical unit
        IF(STATUS.EQ.0)THEN
          CLOSE(UNIT=IUNIT)
        ENDIF
      ENDIF
C
  999 RETURN
      END
