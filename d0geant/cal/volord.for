      SUBROUTINE VOLORD(NMSRCP,AXIS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GETS VOLUME NAME FROM SRCP AND ORDERS
C-                         IT ALONG AXIS
C-
C-   Inputs  : NMSRCP = SRCP MOTHER VOLUME NAME, AXIS = AXIS NUMBER
C-   Outputs :
C-   Controls:
C-
C-   Created  14-JUN-1989   Norman Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:SRCPR.INC'
C
      INTEGER AXIS
      CHARACTER*(*) NMSRCP
      CHARACTER*32 NMSRC1
      INTEGER IVOLU,LEN3
      CHARACTER*4 VOLNAM
C----------------------------------------------------------------------
      CALL ADDSTR(NMSRCP,'(1)',NMSRC1,LEN3)   !Makes it into array format
      CALL GTSRCP(NMSRC1,SRCPAR(1),1)
C !
C ! VOLUME DESCRIPTOR SRCPARS are formatted as follows
C ! SRCPAR(1) = volume name
C
C ****  NOW DO GSORD
C
      CALL UHTOC(SRCPAR(1),4,VOLNAM,4)
      CALL GSORD(VOLNAM,AXIS)
C
  999 RETURN
      END
