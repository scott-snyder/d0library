      SUBROUTINE FOVRLP(HALF1,UNIT1,QUAD1,SECT1,HALF2,UNIT2,QUAD2,
     &                    SECT2,TASK,NSECT,SOVRLP,CHKOVL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Checks overlap of Theta and Phi chamber
C-                         for given sectors and returns array of
C-                         possibilites.
C-
C-   Inputs  : HALF1,UNIT1,QUAD1,SECT1 = Location of starting sector 
C-             HALF2,UNIT2,QUAD2,SECT2 = Location of possible overlapping
C-                                 sector
C-             TASK = 1=check only given HALF2,UNIT2,QUAD2,SECT2
C-                    2=check for all possible sectors given HALF2,UNIT2,QUAD2
C-                    3=check all sectors given HALF1
C-   Outputs : NSECT  = Number of sectors that overlap (task 2 & 3 only)
C-             SOVRLP = Array filled with possible sectors of overlap
C-                      for a given starting sector. (tasks 2 & 3 only)
C-             CHKOVL = Set TRUE if ANY starting and possible overlapping
C-                      sector actually do overlap. (all tasks)
C-
C-   Created   5-JUN-1990   Jeffrey Bantly
C-   Updated   8-NOV-1990   Jeffrey Bantly  increase SECTT range for D0 
C-   Updated  12-DEC-1990   Susan K. Blessing  Fix typo for phi section 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER HALF1,UNIT1,QUAD1,SECT1,HALF2,UNIT2,QUAD2,SECT2
      INTEGER TASK,IQUAD,ISECT
      INTEGER SOVRLP(50,4)       ! contents SOVRLP=>(HALF,UNIT,QUAD,SECT)
      INTEGER QUADTB(0:7),QUADTE(0:7),QUADPB(0:7),QUADPE(0:7)
      INTEGER SECTT(2,0:5,2),SECTM1,SECTM2,QUADT1,QUADT2,NSECT,SECTEN
      LOGICAL CHKOVL
      DATA QUADTB / 4,5,6,7,3,0,1,2/
      DATA QUADTE / 5,6,7,4,0,1,2,3/
      DATA QUADPB / 0,8,18,26,31,3,13,21/
      DATA QUADPE / 8,18,26,0,4,14,22,32/
      DATA SECTT  / 0,2,0,4,1,4,1,5,3,5,3,5,
     &              0,2,0,3,1,4,2,4,3,5,3,5/
C----------------------------------------------------------------------
      CALL VFILL(SOVRLP,75,-1)
      CHKOVL = .FALSE.
      NSECT=0
      IF(TASK.LT.1 .OR. TASK.GT.3) GOTO 999
C
C   Check for bad starting sector location
C
      IF(HALF1.GT.1) GOTO 999
      IF(QUAD1.GT.7) GOTO 999
      IF(UNIT1.EQ.0 .AND. SECT1.GT.5) GOTO 999
      IF(UNIT1.EQ.1 .AND. SECT1.GT.35) GOTO 999
C
C   Initialize some values based on starting sector.
C
      SECTM1=0
      IF(QUAD1.EQ.0.OR.QUAD1.EQ.2.OR.QUAD1.EQ.4.OR.QUAD1.EQ.6) THEN
        QUADT1=2
        IF(SECT1.EQ.5) SECTM1=2
        IF(SECT1.EQ.5 .AND. QUAD1.LE.3) SECTM1=1
      ELSE
        QUADT1=1
        IF(SECT1.EQ.5) SECTM1=2
        IF(QUAD1.GE.4 .AND. SECT1.EQ.5) SECTM1=3
      ENDIF
      SECTM2=0
      IF(QUAD2.EQ.0.OR.QUAD2.EQ.2.OR.QUAD2.EQ.4.OR.QUAD2.EQ.6) THEN
        QUADT2=2
        IF(SECT2.EQ.5) SECTM2=2
        IF(QUAD2.LE.3 .AND. SECT2.EQ.5) SECTM2=1
      ELSE
        QUADT2=1
        IF(SECT2.EQ.5) SECTM2=2
        IF(QUAD2.GE.4 .AND. SECT2.EQ.5) SECTM2=3
      ENDIF
C
C   Task 1 - Just check if the input possible sector overlaps the
C            starting sector.
C
      IF(TASK.EQ.1) THEN
        IF(UNIT1.EQ.0) THEN
          IF(UNIT2.EQ.0) THEN
            IF(QUAD2.EQ.QUADTB(QUAD1) .OR. QUAD2.EQ.QUADTE(QUAD1)) THEN
              IF(SECT2.GE.SECTT(1,SECT1,QUADT1) .AND. 
     &                       SECT2.LE.SECTT(2,SECT1,QUADT1)) THEN
                CHKOVL=.TRUE.
              ENDIF
            ENDIF
          ELSEIF(UNIT2.EQ.1) THEN
            IF(QUADPB(QUAD1).LT.QUADPE(QUAD1)) THEN
              IF(SECT1.LE.4) THEN
                IF(SECT2.GE.QUADPB(QUAD1) .AND.
     &                          SECT2.LE.QUADPE(QUAD1)) THEN
                  CHKOVL=.TRUE.
                ENDIF
              ELSE
                IF(SECT2.GE.QUADPB(QUAD1)+SECTM1 .AND. 
     &                          SECT2.LE.QUADPE(QUAD1)-SECTM1) THEN
                  CHKOVL=.TRUE.
                ENDIF
              ENDIF
            ELSE
              IF(SECT1.LE.4) THEN
                IF(SECT2.GE.QUADPB(QUAD1) .OR.
     &                          SECT2.LE.QUADPE(QUAD1)) THEN
                  CHKOVL=.TRUE.
                ENDIF
              ELSE
                IF(SECT2.GE.QUADPB(QUAD1)+SECTM1 .OR. 
     &                          SECT2.LE.QUADPE(QUAD1)-SECTM1) THEN
                  CHKOVL=.TRUE.
                ENDIF
              ENDIF
            ENDIF
          ELSE                          ! Illegal UNIT2 value, quit
            GOTO 999
          ENDIF
        ELSEIF(UNIT1.EQ.1) THEN
          IF(UNIT2.EQ.0) THEN
            IF(QUADPB(QUAD2).LT.QUADPE(QUAD2)) THEN
              IF(SECT2.LE.4) THEN
                IF(SECT1.GE.QUADPB(QUAD2) .AND.
     &                          SECT1.LE.QUADPE(QUAD2)) THEN
                  CHKOVL=.TRUE.
                ENDIF
              ELSE
                IF(SECT1.GE.QUADPB(QUAD2)+SECTM2 .AND. 
     &                          SECT1.LE.QUADPE(QUAD2)-SECTM2) THEN
                  CHKOVL=.TRUE.
                ENDIF
              ENDIF
            ELSE
              IF(SECT2.LE.4) THEN
                IF(SECT1.GE.QUADPB(QUAD2) .OR.
     &                          SECT1.LE.QUADPE(QUAD2)) THEN
                  CHKOVL=.TRUE.
                ENDIF
              ELSE
                IF(SECT1.GE.QUADPB(QUAD2)+SECTM2 .OR. 
     &                          SECT1.LE.QUADPE(QUAD2)-SECTM2) THEN
                  CHKOVL=.TRUE.
                ENDIF
              ENDIF
            ENDIF
          ELSEIF(UNIT2.EQ.1) THEN
            GOTO 999                    ! No two phi sectors overlap
          ELSE                          
            GOTO 999                    ! Illegal UNIT2 value, quit
          ENDIF
        ELSE
          GOTO 999                      ! Illegal UNIT1 value, quit
        ENDIF
        GOTO 999
      ENDIF
C
      IF(HALF1 .NE. HALF2) GOTO 999     ! Halves input must be same
      IF(TASK.EQ.3) GOTO 500
C
C   Task 2 - List all possible sectors for a given possible HALF2,QUAD2
C            that overlap the starting sector.
C
      IF(UNIT1.EQ.0) THEN
        IF(UNIT2.EQ.0) THEN
          IF(QUAD2.EQ.QUADTB(QUAD1) .OR. QUAD2.EQ.QUADTE(QUAD1)) THEN
            DO 10 ISECT=SECTT(1,SECT1,QUADT1),SECTT(2,SECT1,QUADT1)
              NSECT=NSECT+1
              SOVRLP(NSECT,1)=HALF2
              SOVRLP(NSECT,2)=0
              SOVRLP(NSECT,3)=QUAD2
              SOVRLP(NSECT,4)=ISECT
   10       CONTINUE
            CHKOVL=.TRUE.
          ENDIF
        ELSEIF(UNIT2.EQ.1) THEN
          IF(QUADPB(QUAD1).LT.QUADPE(QUAD1)) THEN
            IF(SECT1.LE.4) THEN
              DO 20 ISECT=QUADPB(QUAD1),QUADPE(QUAD1)
                NSECT=NSECT+1
                SOVRLP(NSECT,1)=HALF2
                SOVRLP(NSECT,2)=1
                SOVRLP(NSECT,3)=0
                SOVRLP(NSECT,4)=ISECT
   20         CONTINUE
              CHKOVL=.TRUE.
            ELSE
              DO 22 ISECT=QUADPB(QUAD1)+SECTM1,QUADPE(QUAD1)-SECTM1
                NSECT=NSECT+1
                SOVRLP(NSECT,1)=HALF2
                SOVRLP(NSECT,2)=1
                SOVRLP(NSECT,3)=0
                SOVRLP(NSECT,4)=ISECT
   22         CONTINUE
              CHKOVL=.TRUE.
            ENDIF
          ELSE
            IF(SECT1.LE.4) THEN
              DO 25 ISECT=QUADPB(QUAD1),35
                NSECT=NSECT+1
                SOVRLP(NSECT,1)=HALF2
                SOVRLP(NSECT,2)=1
                SOVRLP(NSECT,3)=0
                SOVRLP(NSECT,4)=ISECT
   25         CONTINUE
              DO 26 ISECT=0,QUADPE(QUAD1)
                NSECT=NSECT+1
                SOVRLP(NSECT,1)=HALF2
                SOVRLP(NSECT,2)=1
                SOVRLP(NSECT,3)=0
                SOVRLP(NSECT,4)=ISECT
   26         CONTINUE
              CHKOVL=.TRUE.
            ELSE
              DO 27 ISECT=QUADPB(QUAD1)+SECTM1,35
                NSECT=NSECT+1
                SOVRLP(NSECT,1)=HALF2
                SOVRLP(NSECT,2)=1
                SOVRLP(NSECT,3)=0
                SOVRLP(NSECT,4)=ISECT
   27         CONTINUE
              DO 28 ISECT=0,QUADPE(QUAD1)-SECTM1
                NSECT=NSECT+1
                SOVRLP(NSECT,1)=HALF2
                SOVRLP(NSECT,2)=1
                SOVRLP(NSECT,3)=0
                SOVRLP(NSECT,4)=ISECT
   28         CONTINUE
              CHKOVL=.TRUE.
            ENDIF
          ENDIF
        ELSE
          GOTO 999                      ! Illegal UNIT2 value, quit
        ENDIF
      ELSEIF(UNIT1.EQ.1) THEN
        IF(UNIT2.EQ.0) THEN
          SECTM2=0
          IF(QUAD2.EQ.0.OR.QUAD2.EQ.2.OR.QUAD2.EQ.4.OR.QUAD2.EQ.6) THEN
            QUADT2=2
            SECTM2=2
            IF(QUAD2.LE.3) SECTM2=1
          ELSE
            QUADT2=1
            SECTM2=2
            IF(QUAD2.GE.4) SECTM2=3
          ENDIF
          IF(QUADPB(QUAD2).LT.QUADPE(QUAD2)) THEN
            IF(SECT1.GE.QUADPB(QUAD2).AND.SECT1.LE.QUADPE(QUAD2)) THEN
              DO 30 ISECT=0,4
                NSECT=NSECT+1
                SOVRLP(NSECT,1)=HALF2
                SOVRLP(NSECT,2)=0
                SOVRLP(NSECT,3)=QUAD2
                SOVRLP(NSECT,4)=ISECT
   30         CONTINUE
              CHKOVL=.TRUE.
            ENDIF
            IF(SECT1.GE.QUADPB(QUAD2)+SECTM2 .AND. 
     &                   SECT1.LE.QUADPE(QUAD2)-SECTM2) THEN
              NSECT=NSECT+1
              SOVRLP(NSECT,1)=HALF2
              SOVRLP(NSECT,2)=0
              SOVRLP(NSECT,3)=QUAD2
              SOVRLP(NSECT,4)=5
              CHKOVL=.TRUE.
            ENDIF
          ELSE
            IF(SECT1.GE.QUADPB(QUAD2).OR.SECT1.LE.QUADPE(QUAD2)) THEN
              DO 35 ISECT=0,4
                NSECT=NSECT+1
                SOVRLP(NSECT,1)=HALF2
                SOVRLP(NSECT,2)=0
                SOVRLP(NSECT,3)=QUAD2
                SOVRLP(NSECT,4)=ISECT
   35         CONTINUE
              CHKOVL=.TRUE.
            ENDIF
            IF(SECT1.GE.QUADPB(QUAD2)+SECTM2 .OR. 
     &                   SECT1.LE.QUADPE(QUAD2)-SECTM2) THEN
              NSECT=NSECT+1
              SOVRLP(NSECT,1)=HALF2
              SOVRLP(NSECT,2)=0
              SOVRLP(NSECT,3)=QUAD2
              SOVRLP(NSECT,4)=5
              CHKOVL=.TRUE.
            ENDIF
          ENDIF
        ELSEIF(UNIT2.EQ.1) THEN
          GOTO 999                      ! No two phi sectors overlap
        ELSE
          GOTO 999                      ! Illegal UNIT2 value, quit
        ENDIF
      ELSE
        GOTO 999                        ! Illegal UNIT1 value, quit
      ENDIF
      GOTO 999
C
C   Task 3 - List all possible sectors that overlap the starting sector.
C
  500 CONTINUE
      IF(UNIT1.EQ.0) THEN
        DO 50 ISECT=SECTT(1,SECT1,QUADT1),SECTT(2,SECT1,QUADT1)
          NSECT=NSECT+1
          SOVRLP(NSECT,1)=HALF1
          SOVRLP(NSECT,2)=0
          SOVRLP(NSECT,3)=QUADTB(QUAD1)
          SOVRLP(NSECT,4)=ISECT
   50   CONTINUE
        DO 55 ISECT=SECTT(1,SECT1,QUADT1),SECTT(2,SECT1,QUADT1)
          NSECT=NSECT+1
          SOVRLP(NSECT,1)=HALF1
          SOVRLP(NSECT,2)=0
          SOVRLP(NSECT,3)=QUADTE(QUAD1)
          SOVRLP(NSECT,4)=ISECT
   55   CONTINUE
        DO 60 ISECT=QUADPB(QUAD1),QUADPE(QUAD1)
          NSECT=NSECT+1
          SOVRLP(NSECT,1)=HALF1
          SOVRLP(NSECT,2)=1
          SOVRLP(NSECT,3)=0
          SOVRLP(NSECT,4)=ISECT
   60   CONTINUE
        CHKOVL=.TRUE.
      ELSEIF(UNIT1.EQ.1) THEN
        DO 70 IQUAD=0,7
          SECTM2=0
          IF(IQUAD.EQ.0.OR.IQUAD.EQ.2.OR.IQUAD.EQ.4.OR.IQUAD.EQ.6) THEN
            QUADT2=2
            SECTM2=2
            IF(IQUAD.LE.3) SECTM2=1
          ELSE
            QUADT2=1
            SECTM2=2
            IF(IQUAD.GE.4) SECTM2=3
          ENDIF
          IF(QUADPB(IQUAD).LT.QUADPE(IQUAD)) THEN
            IF(SECT1.GE.QUADPB(IQUAD) .AND. SECT1.LE.QUADPE(IQUAD)) THEN
              DO 80 ISECT=0,4
                NSECT=NSECT+1
                SOVRLP(NSECT,1)=HALF1
                SOVRLP(NSECT,2)=0
                SOVRLP(NSECT,3)=IQUAD
                SOVRLP(NSECT,4)=ISECT
   80         CONTINUE
              CHKOVL=.TRUE.
            ENDIF
            IF(SECT1.GE.QUADPB(IQUAD)+SECTM2 .AND. 
     &                           SECT1.LE.QUADPE(IQUAD)-SECTM2) THEN
              NSECT=NSECT+1
              SOVRLP(NSECT,1)=HALF1
              SOVRLP(NSECT,2)=0
              SOVRLP(NSECT,3)=IQUAD
              SOVRLP(NSECT,4)=5
              CHKOVL=.TRUE.
            ENDIF
          ELSE
            IF(SECT1.GE.QUADPB(IQUAD) .OR. SECT1.LE.QUADPE(IQUAD)) THEN
              DO 85 ISECT=0,4
                NSECT=NSECT+1
                SOVRLP(NSECT,1)=HALF1
                SOVRLP(NSECT,2)=0
                SOVRLP(NSECT,3)=IQUAD
                SOVRLP(NSECT,4)=ISECT
   85         CONTINUE
              CHKOVL=.TRUE.
            ENDIF
            IF(SECT1.GE.QUADPB(IQUAD)+SECTM2 .OR. 
     &                           SECT1.LE.QUADPE(IQUAD)-SECTM2) THEN
              NSECT=NSECT+1
              SOVRLP(NSECT,1)=HALF1
              SOVRLP(NSECT,2)=0
              SOVRLP(NSECT,3)=IQUAD
              SOVRLP(NSECT,4)=5
              CHKOVL=.TRUE.
            ENDIF
          ENDIF
   70   CONTINUE
      ELSE
C                                       ! Illegal UNIT1 value, quit
      ENDIF
      GOTO 999
C
C----------------------------------------------------------------------
  999 RETURN
      END
