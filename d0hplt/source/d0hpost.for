      SUBROUTINE D0HPOST(FILNAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find DI3000 postscript file name from
C-                         the configuration file DRV_DATA:DRVPST.CFG
C-
C-   Inputs  : None
C-
C-   Outputs : FILNAME - postscript file name for DI3000
C-
C-   Created  16-OCT-1991   Sharon Hagopian
C-   Updated  18-JUN-1992   Lupe Howell Open statement cahnged to PXOPEN
C-   Updated 20-AUG-1992    Sharon Hagopian Change PXOPEN to D0HOPEN
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CHARACTER*(*) FILNAME
      INTEGER IUNIT,IERR
      INTEGER I,J,N,KK,IER
      CHARACTER*24 CTEST
      CHARACTER*80 CHARRAY
      CHARACTER*24 PSTCFG
      CHARACTER*4 CDAT
      CHARACTER*1 CPERIOD
C----------------------------------------------------------------------
      DATA PSTCFG/'DRV_DATA:DRVPST.CFG'/
      DATA CDAT/'.DAT'/
      DATA CPERIOD/'.'/
C----------------------------------------------------------------------
      CALL GTUNIT(78,IUNIT,IERR)
      CALL D0HOPEN(IUNIT,PSTCFG,'IF',IER)
      IF ( IER .NE. 0 ) THEN
        WRITE(6,*)' D0HPOST - PST configuartion file Not avaliable'
        WRITE(6,*)' FILE WAS NOT SEND TO THE PRINTER'
        GOTO 999
      ENDIF
   10 READ(IUNIT,701,ERR=800,END=900)CHARRAY
  701 FORMAT(A80)
      IF(CHARRAY(1:11).NE.'Output_file') GO TO 10
      CTEST=CHARRAY(12:36)
      CALL SWORDS(CTEST,I,J,N)
C CHECK IF FILE NAME ALREADY HAS EXTENSION
      DO 20 KK=1,24
      IF(CTEST(KK:KK).EQ.CPERIOD)GO TO 30
   20 CONTINUE 
      FILNAME=CTEST(I:J)//CDAT
      GO TO 999
   30 FILNAME=CTEST(I:J)      
      GO TO 999
  800 WRITE(6,801)
  801 FORMAT(' ERROR IN POSTSCRIPT CFG')
  900 FILNAME='POSTS.DAT'   
  999 RETURN
      END
