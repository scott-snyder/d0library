      SUBROUTINE STZDIB(NMZDIV,NMTAG,NMBMP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sets up Z divisions FOR OH module
C-                         containing Main ring beam pipe
C-                         and hangs the pipe segments
C-
C-   Inputs  : NMZDIV. Character string referring to SRCP array
C-             containing Z division info.
C-             NMTAG. Name to be added to get the full Z division name
C-             NMBMP. Name of Beam pipe arrays
C-   Outputs : None
C-   Controls: None
C-
C-   Created   16-NOV-1988   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LEN3,LSR(500),NZ,I,IPT
      REAL RSR(500)
      EQUIVALENCE(LSR,RSR)
      CHARACTER*(*) NMZDIV,NMTAG,NMBMP
      CHARACTER*32 NMSRC1
      CHARACTER*4 NAMEC
      CHARACTER*32 NAME
C----------------------------------------------------------------------
      CALL ADDSTR(NMZDIV,'(1)',NMSRC1,LEN3)   !Makes it into array format
      CALL GTSRCP(NMSRC1,LSR(1),1)
      NZ = LSR(1)
      IPT = 3
      LEN3 = LEN(NMTAG)
      DO 100 I = 1,NZ
        CALL UHTOC(LSR(IPT+1),4,NAMEC,4)
        NAME = NMTAG//NAMEC
        CALL VOLPOS(NAME(1:LEN3+4))
        IPT = IPT + 3 + LSR(IPT+4) + 1
        IPT = IPT + LSR(IPT+1) + 1   ! Eta indices 
        IPT = IPT + LSR(IPT+1) + 1   !PHI DIVISIONS
        CALL STRINT(NMBMP,I,NMSRC1,LEN3)
        CALL VOLPOS(NMSRC1)         !Position Beam pipe segments
  100 CONTINUE
  999 RETURN
      END
