      SUBROUTINE DEHIT(ITRA,X,Y,Z,LAYER,SUBLAY,DESTEP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Save Energy for EC track in working array
C-
C-   Inputs  : ITRA   = Current track number
C-             X,Y,Z  = Current step position
C-             LAYER  = Layer number (from IDTYPE)
C-             SUBLAY = Sublayer number (from IDTYPE)
C-             DESTEP = Energy deposited at step
C-   Outputs :
C-   Controls:
C-
C-   Created  23-JAN-1989   Alan M. Jonckheere
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C  INPUT VARIABLES
      INTEGER ITRA
      REAL    X,Y,Z
      INTEGER LAYER,SUBLAY
      REAL    DESTEP
C
      INTEGER IETA,IPHI,ILYR
      REAL    RXYZ,SMALL,COSTH,ETA
      REAL    PHI
      DATA    SMALL/1.E-6/
C
      INCLUDE 'D0$INC:GCONST.INC/LIST'
C
C----------------------------------------------------------------------
C
      IF ( LAYER.EQ.10 ) THEN            ! Massless Gap
        RXYZ = SQRT(X*X + Y*Y + Z*Z)
        IF ( RXYZ.LT.SMALL ) RXYZ = SMALL
        COSTH = Z/RXYZ
        ETA = -LOG((1.-COSTH)/(1.+COSTH))/2.
        IETA = INT(ETA/.1) + INT(SIGN(1.,Z))
        IF ( IABS(IETA).GT.13 ) THEN
          IETA = IETA - SIGN(1.,Z)
        ELSE IF (IABS(IETA).LT.8) THEN
          IETA = IETA + SIGN(1.,Z)
        ENDIF
C
        PHI = ATAN2(-Y,-X) + PI
        IPHI = INT(PHI*32./PI) + 1
C
        ILYR = LAYER
      ELSE
        CALL ECINDX(LAYER,SUBLAY,X,Y,Z,IETA,IPHI,ILYR)
        IF ( ILYR.LT.0 ) THEN
          CALL DCHIT(ITRA,X,Y,Z,-ILYR,DESTEP)
          GOTO 999
        ENDIF
      ENDIF
C
C ****  STORE THE DATA
C
      CALL DHSTOR(ITRA,IETA,IPHI,ILYR,DESTEP)
C
  999 RETURN
      END
