      INTEGER FUNCTION MUORIENT( NMOD )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return postion of muon chamber
C-
C-   Inputs  : NMOD  : muon chmaber ID
C-   Return  :         1 - TOP of bottom
C-                     2 - SIDE
C-                     3 - Forward Vertical
C-                     4 - Forward horizontal
C-                     0 - Error
C-   Controls: None
C-
C-   Created  10-JUN-1992   Atsushi Taketani
C-   Modified 17-AUG-1992   Atsushi Taketani  Use orientation
C-                                            index from MGEO
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER  NMOD
C
      INTEGER  MOD_100, MOD_10
      INTEGER  HORIZ(4), VERT(4),K1
      DATA     VERT/0,3,4,7/
      DATA     HORIZ /1,2,5,6/
C
C----------------------------------------------------------------------
      MUORIENT = 0
C
      MOD_100 = MOD(NMOD,100)
      MOD_10  = MOD(NMOD,10)
C
      IF ( NMOD.LT.300 .AND. MOD_100.LE.49 ) THEN   ! Central
        DO K1=1,4
          IF ( HORIZ(K1).EQ.MOD_10 ) THEN
            MUORIENT = 1
            GOTO 999
          END IF
          IF ( VERT(K1).EQ.MOD_10 ) THEN
            MUORIENT = 2
            GOTO 999
          END IF
        END DO
      ELSE                                          ! Forward
        DO K1=1,4
          IF ( HORIZ(K1).EQ.MOD_10 ) THEN
            MUORIENT = 4
            GOTO 999
          END IF
          IF ( VERT(K1).EQ.MOD_10 ) THEN
            MUORIENT = 3
            GOTO 999
          END IF
        END DO
      END IF
C
  999 RETURN
      END
