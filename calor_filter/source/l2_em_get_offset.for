      SUBROUTINE L2_EM_GET_OFFSET(IETAC,IPHIC,ILYR,OFFSET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get ET(nominal) offset for this eta cell
C-
C-   Inputs  : IETAC  offline tower eta index (plus CAGS bank)
C-             IPHIC  offline tower phi index
C-             ILYR   offline tower layer index
C-   Outputs : OFFSET  offset in energy scale
C-   Controls:  ET_IN_CAEP = .TRUE. means EFLOOR was in Et
C-
C-   Created  7-NOV-1993   James T. Linnemann   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CL2_STP_LINK.INC'
      INCLUDE 'D0$INC:L2_EM_STP.INC'
      INTEGER IETAC,IPHIC,ILYR
      INTEGER IVERS
      REAL    OFFSET, CL2_SNTH
C----------------------------------------------------------------------
C
      IF (L2CAGS.LE.0) THEN
        CALL ERRMSG('NO_CAGS','L2_EM_GET_OFFSET',
     &    'No CAGS bank found','F')
      ENDIF
      OFFSET = 0
      IVERS = IC(L2CAGS+10)
      IF (IVERS.GE.2) THEN
C
C...for now, no eta or even detector dependence
        IF (IETAC.LE.14) OFFSET = C(L2CAGS+271)   ! in Energy, for EC North
        IF (IABS(IETAC).LT.14) OFFSET = C(L2CAGS+272)   ! in Energy, for CC
        IF (IETAC.GE.14) OFFSET = C(L2CAGS+273)   ! in Energy, for EC South
        IF (ET_IN_CAEP) OFFSET = OFFSET * CL2_SNTH(IETAC,IPHIC,ILYR,0.0)
      ENDIF
  999 RETURN
      END
