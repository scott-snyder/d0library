      SUBROUTINE SETROT(NMSRCP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sets up rotation matrices
C-
C-   Inputs  : NMSRCP = Character string SRCP ident
C-   Outputs : None
C-             SRCPAR array containing the parameters of the
C-             Rotation matrices in question. In SRCPR.INC
C-   Controls: None
C-
C-   Created   3-OCT-1988   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NMSRCP
      CHARACTER*32 NMSRC1
      INTEGER NRTMX
      PARAMETER( NRTMX  = 500 )   !maximum number of rotation matrices
C                                 ! per array
      INTEGER LSET(7*NRTMX+1),IS,LEN3,NROT,IT
C----------------------------------------------------------------------
      CALL ADDSTR(NMSRCP,'(1)',NMSRC1,LEN3)   !Makes it into array format
      CALL GTSRCP(NMSRC1,LSET(1),1)
C
      NROT = LSET(1)
      IT = 1
      DO 40 IS = 1 , NROT
        CALL GSROTM(LSET(1+IT),LSET(2+IT),LSET(3+IT),LSET(4+IT),
     &    LSET(5+IT),LSET(6+IT),LSET(7+IT))
        IT = IT + 7
   40 CONTINUE
C
  999 RETURN
      END
