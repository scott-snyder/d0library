      SUBROUTINE STZDV1(NMZDIV,NMTAG,IAB)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sets up Z divisions FOR MH MODULES.
C-
C-   Inputs  : NMZDIV. Character string referring to SRCP array
C-             containing Z division info.
C-             NMTAG. Name to be added to get the full Z division name
C-             IAB = 1 MEANS USE MFA NAME. =2 MFB NAME
C-   Outputs : None
C-   Controls: None
C-
C-   Created   5-NOV-1988   Rajendran Raja
C-   Updated  20-JUN-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LEN3,LSR(500),NZ,I,IPT,IAB
      REAL RSR(500)
      EQUIVALENCE(LSR,RSR)
      CHARACTER*(*) NMZDIV,NMTAG
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
        IF(IAB.EQ.1)THEN
          CALL UHTOC(LSR(IPT+1),4,NAMEC,4)
        ELSEIF(IAB.EQ.2)THEN
          CALL UHTOC(LSR(IPT+2),4,NAMEC,4)
        ENDIF
        NAME = NMTAG//NAMEC
        CALL VOLPOS(NAME(1:LEN3+4))
        IPT = IPT + 4 + LSR(IPT+5) + 1
        IPT = IPT + LSR(IPT+1) + 1   !Eta indices
        IPT = IPT + LSR(IPT+1) + 1   !PHI DIVISIONS
  100 CONTINUE
  999 RETURN
      END
