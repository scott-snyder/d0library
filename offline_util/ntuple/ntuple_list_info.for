      SUBROUTINE NTUPLE_LIST_INFO(LUN,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For the current RZ-directory list, to
C-   unit LUN information about all ntuples in that directory.
C-
C-   Inputs  : LUN        [I]   Unit number for output
C-   Outputs : STATUS     [I]   0 - OK
C-   Controls:
C-
C-   Created  17-JUN-1992   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LUN
      INTEGER STATUS
C----------------------------------------------------------------------
      INTEGER I,J,II,JJ,KK,NENTRY,NDIM
C
      INTEGER MAXDIM, MAXID, USERID
      PARAMETER( MAXDIM = 511 )
      PARAMETER( MAXID  = 511 )
      PARAMETER( USERID = 511 )
C
      INTEGER IDD(MAXID), NIDD
      REAL    XMIN(MAXDIM),XMAX(MAXDIM)
      CHARACTER*8 TAG(MAXDIM)
      CHARACTER*80 TITLE,RZPATH,REMARK
C----------------------------------------------------------------------
      STATUS = 0
C
C ****  Get all ntuple ids in current RZ directory
C
      CALL NTUPLE_GET_IDS(MAXID,NIDD,IDD)
C
      IF ( NIDD .GT. 0 ) THEN
        WRITE(LUN,'('' '')')
C
C ****  Display info for each ntuple
C
        DO I =  1, NIDD
          CALL NTUPLE_GET_INFO
     &      (IDD(I),MAXDIM,TITLE,RZPATH,NENTRY,NDIM,TAG,XMIN,XMAX,
     &      STATUS)
C
          IF ( STATUS .EQ. 0 ) THEN
C
            WRITE(LUN,'('' ID        : '',I10)')IDD(I)
            WRITE(LUN,'(''    Entries: '',I10)')NENTRY
            WRITE(LUN,'(''    Size   : '',I10)')NDIM
C
            CALL SWORDS(TITLE,II,JJ,KK)
            WRITE(LUN,'(''    Title  : '',A)' ) TITLE(II:JJ)
C
            CALL SWORDS(RZPATH,II,JJ,KK)
            WRITE(LUN,'(''    Path   : '',A)' ) RZPATH(II:JJ)
C
            WRITE(LUN,'(''    Tags   : '')')
            WRITE(LUN,'(10X,5A10)') (TAG(J),J=1,NDIM)
            WRITE(LUN,'(''    Xmin   '')')
            WRITE(LUN,'(10X,5(1PE10.3))') (XMIN(J),J=1,NDIM)
            WRITE(LUN,'(''    Xmax   '')')
            WRITE(LUN,'(10X,5(1PE10.3))') (XMAX(J),J=1,NDIM)
          ENDIF
        ENDDO
      ELSE
      ENDIF
  999 RETURN
      END
