C----------------------------------------------------------------------
      SUBROUTINE GTZ
     & (SLOWZ,SLOWF,MINTF,FASTZ,FASTF,
     &  TKVZ,TKVDZ,TOOL,L0Q,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To get words that were in JUTL in 1A for WDF case
C-
C-   Inputs  : none
C-   Outputs for GTZ :
C-             SLOWZ              (R)    L0 slow vertex z
C-             SLOWF              (I)    L0 slow good z flag (1/0 = T/F)
C-             MINTF              (I)    Multiple interaction flag (L0) (1-4)
C-             FASTZ              (R)    L0 fast vertex z
C-             FASTF              (I)    L0 fast good z flag (1/0 = T/F)
C-             TKVZ(3)            (R)    Tracking vertex z
C-             TKVDZ(3)           (R)    Tracking vertex delta z
C-             TOOL               (I)    Multiple interaction tool (1-4)
C-             L0Q                (R)    L0 quality
C-             IER                (I)    0 is good
C-   Created  10-MAR-1994   Andrew G. Brandt 
C-   Updated  07-FEB-1995   Andrew G. Brandt remove GTL0VT stuff for bad fastz
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER  I
C: VERT
      INTEGER NVER
      REAL ZVER(3,3)
C: L0
      INTEGER MI_FLAG             
      INTEGER LPLV0, GZPLV0
      INTEGER MULTIPLE_INTERACTION_TOOL
      EXTERNAL MULTIPLE_INTERACTION_TOOL
      LOGICAL OK, FAST_FLAG, SLOW_FLAG
C
      INTEGER  SLOWF,MINTF,FASTF,IER,TOOL
      REAL     SLOWZ, FASTZ, TKVZ(3), TKVDZ(3), L0Q
C----------------------------------------------------------------------
C- Reset stuff
C
      SLOWZ = -999.
      SLOWF = -1
      MINTF = -1
      FASTZ = -999.
      FASTF = -1
      DO I = 1,3
         TKVZ(I)  = -999.
         TKVDZ(I) = -999.
      END DO
      L0Q  = -999.
      TOOL = -1
C
C: Ideally, we get this info from PLV0 bank
C
      CALL GTPLV0_ZONLY(FASTZ, FAST_FLAG, SLOWZ, SLOW_FLAG, MI_FLAG)
      MINTF=MI_FLAG
      IF ( SLOW_FLAG ) THEN
          SLOWF = 1   ! 1=good 0=bad
      ELSE
          SLOWF = 0
      ENDIF
      IF ( FAST_FLAG ) THEN
          FASTF = 1
      ELSE
          FASTF = 0
      ENDIF
C
C: Take the first 3 vertices
C
      CALL VERTEX_INFO(3, NVER, ZVER, OK )
      IF (.NOT. OK) NVER = 0
      DO I = 1, MIN( NVER, 3 )
        TKVZ(I)  = ZVER( 1,I)
        TKVDZ(I) = ZVER( 2,I)
      ENDDO
C
C: Get PLV0 time sigma word
C
      LPLV0 = GZPLV0()
      IF ( LPLV0 .GT. 0 ) L0Q = Q( LPLV0 + 4)
C
C: Get Multiple interaction tool word
C
      TOOL = MULTIPLE_INTERACTION_TOOL()
      IER = 0
  999 RETURN
      END
