      SUBROUTINE D0HSTR(IDNUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Stores histogram IDNUM or if IDNUM=0 stores
C-                         all hists.
C-
C-   Inputs  : IDNUM - ID of HISTOGRAMS to SAVE
C-   Outputs :
C-   Controls:
C-
C-   Created  27-AUG-1988   Tami Kramer
C-   Updated  19-APR-1991   Chip Stewart
C-   Updated  25-Feb1992    Herbert Greenlee
C-      Replace OPEN/HRFILE with HROPEN
C-   Updated  22-APR-1992   SoftExp  (Harrison B. Prosper)
C-      Clean up
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDNUM
C----------------------------------------------------------------------
      INTEGER I,J,K
      LOGICAL HEXIST
      CHARACTER*80 FILNAM,CURDIR,REMARK
      CHARACTER*10 CNUM
C----------------------------------------------------------------------
C
C ****  Note directory
C
      CALL HCDIR(CURDIR,'R')
C
C ****  Create file-name
C
      CALL D0H_FILE(IDNUM,FILNAM)
C
      WRITE(CNUM,FMT='(I10)') IDNUM
      CALL WORD(CNUM,I,J,K)
C
      IF ( IDNUM .LE. 0 ) THEN
        CALL HCDIR('//PAWC',' ')
        CALL HRPUT(0,FILNAM,'NT')
        REMARK = ' ALL Histograms STORED in file '//FILNAM
        CALL INTMSG(REMARK)
      ELSE
        IF ( HEXIST(IDNUM) ) THEN
          CALL HRPUT(IDNUM,FILNAM,'N')
          REMARK = ' Histogram '//CNUM(I:J)//' STORED in file '//FILNAM
          CALL INTMSG(REMARK)
        ELSE
          REMARK = ' Histogram '//CNUM(I:J)//' NOT FOUND'
          CALL INTMSG(REMARK)
        ENDIF
      ENDIF
C
C ****  Restore directory
C
      CALL HCDIR(CURDIR,' ')
  999 RETURN
      END
