      SUBROUTINE ISAGEA(IDENT,PARTID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Maps from ISAJET (IDENT) to Geant
C-
C-   Inputs  : IDENT = isajet code
C-             PARTID = Geant code
C-   Outputs :
C-   Controls:
C-
C-   Created   SLL 30-DEC-1985
C-   Updated   8-JAN-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC'
      INTEGER NTABLE
      PARAMETER (NTABLE = 3331)
      INTEGER IDENT,PARTID,TABLE(-NTABLE:NTABLE)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA TABLE/6663*4/
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        TABLE(10) = 1
        TABLE(-12) = 2
        TABLE(12) = 3
        TABLE(-11) = 4
        TABLE(11) = 4
        TABLE(-13) = 4
        TABLE(13) = 4
        TABLE(-15) = 4
        TABLE(15) = 4
        TABLE(-14) = 5
        TABLE(14) = 6
        TABLE(110) = 7
        TABLE(120) = 8
        TABLE(-120) = 9
        TABLE(-20) = 10
        TABLE(130) = 11
        TABLE(-130) = 12
        TABLE(1220) = 13
        TABLE(1120) = 14
        TABLE(-1120) = 15
        TABLE(20) = 16
        TABLE(220) = 17
        TABLE(2130) = 18
        TABLE(1130) = 19
        TABLE(1230) = 20
        TABLE(2230) = 21
        TABLE(1330) = 22
        TABLE(2330) = 23
        TABLE(3331) = 24
        TABLE(-1220) = 25
        TABLE(-2130) = 26
        TABLE(-2230) = 29
        TABLE(-1230) = 28
        TABLE(-1130) = 27
        TABLE(-1330) = 30
        TABLE(-2330) = 31
        TABLE(-3331) = 32
        TABLE(16) = 34
        TABLE(-16) = 33
        TABLE(-240) = 35
        TABLE(240) = 36
        TABLE(-140) = 37
        TABLE(140) = 38
        TABLE(-340) = 39
        TABLE(340) = 40
        TABLE(2140) = 41
      ENDIF
      IF ( ABS(IDENT).LE.NTABLE ) THEN
        PARTID = TABLE(IDENT)
      ELSE
        CALL ERRMSG('ISAGEA','ISAGEA',
     &    'NO CORRESPONDING GEANT PARTICLE ','W')
        PARTID = TABLE(120)             ! PION AS DEFAULT
        IF ( IDENT.LT.0 ) PARTID = TABLE(-120)      ! KEEP CHARGE
        RETURN
      ENDIF
  999 RETURN
      END

