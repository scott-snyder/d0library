      SUBROUTINE PXMODIFY_MERGE(CURRENT_RCPFILE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Merges the current RCP file with one selected from
C-   the user.
C-
C-   Inputs  : CURRENT_RCPFILE [C*]: Name of the current RCP file
C-   Outputs : None
C-
C-   Created  30-OCT-1991   Lupe Howell
C-   Updated  22-NOV-1991   Lupe Howell  New SRCP merge routine
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) CURRENT_RCPFILE
C----------------------------------------------------------------------
      CHARACTER*80 FILNAM2,MERGED_FILE,FILREAD,FIL2BANK
      CHARACTER*1 CVAL(40)
      CHARACTER*40 REMARK
      INTEGER FILSIZ,IER,I,J,BLEN,NSCRE1,NSCRE2,ITYPE(40),TOTSCRE
      INTEGER IVAL

C      
      CHARACTER*80 MERGE_ARRAY(3),TEMP
      DATA MERGE_ARRAY/'PXSCREEN',
     &                  'PXPARAMS',
     &                  'COMPACK_MENUS'/
C     
      CHARACTER*(*) TEMP_RCP
      PARAMETER( TEMP_RCP = 'TEMP_RCP' )
C----------------------------------------------------------------------
C
C ****  Get the total number of screens in file1
C
      CALL EZTELL(TEMP,I)
      CALL EZPICK(CURRENT_RCPFILE)
      IVAL = 1
      CALL EZ_GET_ARRAY('PXSCREEN','NSCREEN',1,NSCRE1,CVAL,
     &     ITYPE,REMARK,IER)     !
      CALL EZRSET
C
C ****  Gettin name of the file to be merge
C
      CALL PUZSEL(FILNAM2,FILSIZ)
      IF( FILSIZ .EQ. 0 ) GOTO 999
C
C ****  Read the file requested
C
      I = INDEX(FILNAM2,'.RCP')
      J = INDEX(FILNAM2,':')
      IF ( I .GT. 0 ) THEN
        FILSIZ = I - 1
      ENDIF
      IF ( J .EQ. 0 ) THEN
        J = INDEX(FILNAM2,']')
      ENDIF
      FILREAD = FILNAM2(1:FILSIZ)//'.RCP'
      FIL2BANK = FILNAM2(J+1:FILSIZ)//'_RCP'
      FILSIZ = FILSIZ + 4
      BLEN = FILSIZ - J
      CALL INTMSG(' Reading file: '//FILREAD)
      CALL INRCP(FILREAD,IER)
      IF ( IER .NE. 0 )THEN
        CALL INTMSG(' Problems acessing file '//FILREAD(1:FILSIZ))
        GOTO 999
      ENDIF
C
C ****  Get the total number of screens in file2
C
      CALL EZ_GET_ARRAY('PXSCREEN','NSCREEN',1,NSCRE2,CVAL,
     &     ITYPE,REMARK,IER)     
      CALL EZRSET
C
C ****  Merge Current file with selected file
C
      MERGED_FILE = 'MERGED_RCP'
      CALL EZMERGE_BANKS(CURRENT_RCPFILE,FIL2BANK,MERGED_FILE,IER)
      IF ( IER .NE. 0 ) THEN
        CALL INTMSG(' Problems Merging Files')
        GOTO 999
      ENDIF
C
C ****  Updating the total sccreens in new merged file
C ****  and delete the NSCREEN element from the second file merged
C
      CALL EZPICK(MERGED_FILE)
      TOTSCRE = NSCRE1 + NSCRE2
      CALL EZ_SET_ARRAY('PXSCREEN','NSCREEN',TOTSCRE,IER)     
      CALL EZ_REMOVE_ELEMENT('PXSCREEN','NSCREEN',2,1,IER)
      CALL EZRSET
C
C ****  Rename new RCP file to curent rcp file
C
      CALL EZRNAM(CURRENT_RCPFILE,TEMP_RCP)
      CALL EZDROP(TEMP_RCP) ! Droping old file name
      CALL EZRNAM(MERGED_FILE,CURRENT_RCPFILE)
C
C ****  Update the common blocks 
C
      CALL PXBUILD_READ(CURRENT_RCPFILE)
  999 RETURN
      END
