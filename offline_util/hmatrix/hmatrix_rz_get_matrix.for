      SUBROUTINE HMATRIX_RZ_GET_MATRIX(IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FETCH HMATRIX (LATEST CYCLE FOR time being)
C-                         from RZ
C-
C-   Inputs  :
C-   Outputs : IER = 0 if AOK
C-   Controls:
C-
C-   Created   8-DEC-1990   Rajendran Raja
C-   Updated  24-JAN-1991   Harrison B. Prosper
C-      Add call to HMATRIX_NAME
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
      INCLUDE 'D0$LINKS:IZHRCP.LINK'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      CHARACTER*80 DIRECTORY
      INTEGER TRULEN,ICYCLE,KEY
      LOGICAL HMATRIX_EXIST_RCP,YES,EZERR
      INTEGER CYC_READ
      LOGICAL LIST_DIR
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /
C
C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('HMATRIX_RCP')
        CALL EZGET('LIST_RZ_DIRECTORY',LIST_DIR,IER)
        CALL EZRSET
      ENDIF
      IER = 0
      DIRECTORY = '//'//TOP_DIRECTORY(1:TRULEN(TOP_DIRECTORY))//'/'
      DIRECTORY = DIRECTORY(1:TRULEN(DIRECTORY))//SUB_DIRECTORY
C
      CALL RZCDIR(DIRECTORY,' ')
C
      IF(IQUEST(1).EQ.1)THEN
        CALL INTMSG(' Directory'//DIRECTORY(1:TRULEN(DIRECTORY))//
     &    ' does not exist in RZ file ')
        IER = 555
        GO TO 999
      ELSE
        ICYCLE = 99999                  ! READ HIGHEST CYCLE
        KEY = 1
        CALL RZIN(IDVSTP,LHMTR,2,KEY,ICYCLE,' ')
        IF(IQUEST(1).NE.0)THEN
          IER = IQUEST(1)
          CALL INTMSG(' Error reading from RZ directory '//DIRECTORY)
          GO TO 999
        ENDIF
        IF(IER.EQ.0)THEN
          CYC_READ = IQUEST(6)
          IF ( LIST_DIR ) THEN
            WRITE(MSG,1)CYC_READ,DIRECTORY(1:TRULEN(DIRECTORY))
    1       FORMAT(' Read Cycle ',I8,'  from RZ directory ',A)
            CALL INTMSG(MSG)
          ENDIF
        ENDIF
      ENDIF
C
C ****  convert CRCP bank to srcp bank in situ. Call Chip's routine here
C
      CALL EZUNNAME('HMATRIX_RZ_RCP')     !Undeclare HMATRIX_RZ_RCP to
      CALL EZCRCP_TO_SRCP('HMATRIX_RZ_RCP',LHMTR,IZHRCP)
C
      CALL HMATRIX_NAME(SUB_DIRECTORY,LHMTR)            ! Name Hmatrix banks
C
  999 RETURN
      END
