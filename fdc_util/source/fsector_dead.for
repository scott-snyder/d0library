      FUNCTION FSECTOR_DEAD(HALF,UNIT,QUAD, SECTOR) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return true if sector is known to be dead,
C-      or not read out.
C-
C-   Inputs  : HALF,UNIT,QUAD, SECTOR
C-
C-   Created   1-JUL-1991   Robert E. Avery
C-   Updated  22-NOV-1991   Robert E. Avery  Always false for MC. 
C-   Updated   1-FEB-1993   Robert E. Avery  MC defined as RUNTYPE <= 0,
C-                              not necessarily =0 (for special cases). 
C-   Updated  15-DEC-1993   Robert E. Avery  Always false for collider data.  
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL FSECTOR_DEAD
C
C  Input:
      INTEGER HALF,UNIT,QUAD, SECTOR
C  Local
      INTEGER RUNTYPE,IER
      INTEGER PHI_QDRT(0:35,0:1)        ! TRANSLATION FROM SECT TO HV OCTANT
C
      LOGICAL FDEXST
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
      DATA PHI_QDRT /2,2,2,2,1,1,1,1,8,8,8,8,8,7,7,7,7,7,
     &               6,6,6,6,5,5,5,5,4,4,4,4,4,3,3,3,3,3,
     &               7,7,7,7,7,8,8,8,8,8,1,1,1,1,2,2,2,2,
     &               3,3,3,3,3,4,4,4,4,4,5,5,5,5,6,6,6,6/
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('RUNTYPE',RUNTYPE,IER)
        CALL EZRSET
        FIRST = .FALSE.
      ENDIF
C
      FSECTOR_DEAD = .FALSE.
      IF (   (RUNTYPE .LE. 0)
     &  .OR. (RUNTYPE .GE. 6) ) THEN
        GOTO 999
      ENDIF
C
      IF ( .NOT. FDEXST(HALF,UNIT,QUAD,SECTOR ) ) THEN
        FSECTOR_DEAD = .TRUE.
        GOTO 999
      ENDIF
C
      IF ( (RUNTYPE .EQ.3) .OR. (RUNTYPE.EQ.4) ) THEN
        IF ( HALF .EQ. 0 ) THEN
          IF ( UNIT .EQ. 1 ) THEN
            IF ( PHI_QDRT(SECTOR,HALF).EQ.1 ) THEN
              FSECTOR_DEAD = .TRUE.
              GOTO 999
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
  999 RETURN
      END
