      SUBROUTINE HMATRIX_RZ_LIST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : LIST DIRECTORIES IN THE RZ FILE
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  28-NOV-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INTEGER NDIR_MAX,NDIR,I
      PARAMETER( NDIR_MAX = 10000 )
      CHARACTER*16 DEVICE_NAME(NDIR_MAX)
      LOGICAL YES
      CHARACTER*80 MSG1
      INTEGER LEN3
C----------------------------------------------------------------------
      CALL RZCDIR('//'//TOP_DIRECTORY,' ')
C
      IF(IQUEST(1).EQ.1)THEN
        CALL INTMSG
     &    (' Current sub_directory not valid in RZ file ')
C
      ELSE
        CALL RZRDIR(NDIR_MAX,DEVICE_NAME,NDIR)
        WRITE(MSG,1)NDIR
    1   FORMAT(' Number of subdirectories in HMatrix rz file ',I12)
        CALL INTMSG(MSG)
C
        IF(NDIR.GT.0)THEN
            CALL ADDSTR(TOP_DIRECTORY,'/',MSG1,LEN3)
            WRITE(MSG,3)MSG1(1:LEN3)
    3       FORMAT(' Sub-directories in top directory ',A)
            CALL INTMSG(MSG)
            DO I = 1 ,NDIR
              WRITE(MSG,2)DEVICE_NAME(I)
    2         FORMAT(1X,A)
              CALL INTMSG(MSG)
            ENDDO
        ENDIF
      ENDIF
  999 RETURN
      END
