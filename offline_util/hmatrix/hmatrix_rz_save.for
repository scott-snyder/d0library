      SUBROUTINE HMATRIX_RZ_SAVE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Save HMATRIX TO RZ
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   3-DEC-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      LOGICAL HDE_EXIST_RZ
      INTEGER KEY,ICYCLE,TRULEN,IER
      CHARACTER*80 DIRECTORY
C----------------------------------------------------------------------
      IER = 0
      DIRECTORY = '//'//TOP_DIRECTORY(1:TRULEN(TOP_DIRECTORY))//'/'
      DIRECTORY = DIRECTORY(1:TRULEN(DIRECTORY))//SUB_DIRECTORY
      CALL RZCDIR(DIRECTORY,' ')
      IF(IQUEST(1).EQ.1)THEN
        CALL INTMSG(
     &    ' Current sub_system directory not valid in RZ file ')
      ELSE
C
C ****  READY TO WRITE OUT RZ FILE HERE
C
        CALL MZDROP(IDVSTP,LWORK,' ')
        CALL MZDROP(IDVSTP,LPROD,' ')
C
        IF ( LHMTR.GT.0 ) THEN
          KEY = 1
          CALL RZOUT(IDVSTP,LHMTR,KEY,ICYCLE,' ')
          WRITE(MSG,1)ICYCLE,SUB_DIRECTORY(1:TRULEN(SUB_DIRECTORY))
    1     FORMAT(' Wrote Cycle ',I7,' of HMATRIX  to RZ  sub-dir. ',A)
C
          CALL INTMSG(MSG)
        ELSE
          CALL INTMSG(' HMATRIX header bank '//
     &      ' does not exist. Nothing saved ')
        ENDIF
      ENDIF
C
  999 RETURN
      END
