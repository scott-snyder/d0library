      FUNCTION ISGINI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Initialization hook for ISAJET
C-
C-   Created   8-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ISALNK.INC'
      LOGICAL OK,ISGINI
      CHARACTER*72 V,VISAZEB
      CHARACTER*40 VS,VISAJE
C----------------------------------------------------------------------
      ISGINI=.TRUE.
      VS=VISAJE()
      CALL INTMSG(VS)
      V=VISAZEB()
      CALL INTMSG(V)
      CALL INTMSG(' For writing events pick option "Output Data File",'
     &  //' select stream STA.')
C
C        ISAJET dialog flags
      CALL FLGBK('ISA_DIAL',1)
      CALL FLGSET('ISA_DIAL',.FALSE.)
      CALL FLGSET('NO_INPUT_FILE',.TRUE.)
      CALL FLGBK('ONE_TRACK',1)
      CALL FLGBK('PARTONS',1)
C
C        setup the link area for ISAZEB
      CALL MZLINK(IXCOM,'/ISALNK/',LVD,PQREF(MPQREF),LVD)
  999 RETURN
      END
