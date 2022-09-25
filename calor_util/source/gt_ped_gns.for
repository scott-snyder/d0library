      SUBROUTINE GT_PED_GNS(TASK,IGN,CRATE,CARD,HEAD,VAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns Values and Sigmas for PED/?GAINS
C-
C-   Inputs  : TASK =1,2 PEDS, =3 Gains
C-             IGN = 0 X8 Gain. =1 X1 Gain
C
C-   Outputs : HEAD(30) Contents of header bank
C-             VAL(768) Value,Sigma of channels
C-
C-   Created  25-DEC-1987   Rajendran Raja
C-   Modified  2-MAR-1989   Jan Guida,   PRTPDG now in D0$PARAMS
C-   Updated  13-NOV-1990   Jan Guida  Added CRATE argument, and ability 
C-                                      to do multiple crates 
C-   Updated   7-JUN-1991   Jan Guida  Pedestals:  TASK = 1 or 2 (add 2)
C-   Updated   8-APR-1992   Jan Guida   Change LEN to LENDAT (UNIX)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER TASK,IGN,CARD,HEAD(*),LINKH,LINK,LZFIND
      INTEGER CRATE
C
      INCLUDE 'D0$PARAMS:PRTPDG.DEF'
C
      REAL VAL(*)
      CHARACTER*80 STRING
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCPD1.LINK'
      INCLUDE 'D0$LINKS:IZCPD8.LINK'
      INCLUDE 'D0$LINKS:IZCGN1.LINK'
      INCLUDE 'D0$LINKS:IZCGN8.LINK'
C----------------------------------------------------------------------
      IF ( TASK.LT.3 ) THEN      !Pedestals
        LCPDH = LZFIND(IDVSTP,LCPDH,CRATE,9)   !Finds Bank with Crate
        IF (LCPDH.EQ.0) THEN
          WRITE(STRING,10)CRATE
   10     FORMAT(' ERROR in GT_PED_GNS: no pedestal bank for crate ',I2)
          CALL INTMSG(STRING)
          HEAD(1)=-1
          GO TO 999
        ENDIF
        IF ( IGN.EQ.0 ) THEN     !X8 gains
          LINKH = LC(LCPDH-IZCPD8)
        ELSEIF ( IGN.EQ.1 ) THEN !X1 gains
          LINKH = LC(LCPDH-IZCPD1)
        ENDIF
      ELSE                       !Gains
        LCGNH = LZFIND(IDVSTP,LCGNH,CRATE,9)   !Finds Bank with Crate
        IF (LCGNH.EQ.0) THEN
          WRITE(STRING,11)CRATE
   11     FORMAT(' ERROR in GT_PED_GNS: no gain bank for crate ',I2)
          CALL INTMSG(STRING)
          HEAD(1)=-1
          GO TO 999
        ENDIF
        IF ( IGN.EQ.0 ) THEN     !X8 gains
          LINKH = LC(LCGNH-IZCGN8)
        ELSEIF ( IGN.EQ.1 ) THEN !X1 gains
          LINKH = LC(LCGNH-IZCGN1)
        ENDIF
      ENDIF
C
      LINK  = LZFIND(IDVSTP,LINKH,CARD,11)   !Finds Bank with Card
      IF ( LINK.GT.0 ) THEN
        CALL UCOPY_i(IC(LINK),HEAD,NHEAD)
        CALL UCOPY(C(LINK+NHEAD+1),VAL,LENDAT)
      ELSE
        HEAD(1)=-1
      ENDIF
  999 RETURN
      END
