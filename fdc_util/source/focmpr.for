      SUBROUTINE FOCMPR(NFSECT,FOVRLP,NSSECT,SOVRLP,
     &                                        NMATCH,LOCMATCH,MATCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Match the overlapping FDC sectors between a 
C-                         first set and a second set of overlap sectors.
C-
C-   Inputs  : NFSECT,FOVRLP = Number and locations of first set of sectors
C-             NSSECT,SOVRLP = Number and locations of second set of sectors
C-   Outputs : NMATCH,LOCMATCH = Number and location of successful matchups
C-             MATCH = set TRUE if any overlap is found between two sets
C-
C-   Created   6-JUN-1990   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NFSECT,NSSECT,IFSECT,ISSECT,NMATCH
      INTEGER FOVRLP(50,4)     ! contents FOVRLP=>(HALF,UNIT,QUAD,SECTOR)
      INTEGER SOVRLP(50,4)     ! contents SOVRLP=>(HALF,UNIT,QUAD,SECTOR)
      INTEGER LOCMATCH(50,4)   ! contents =>(HALF,UNIT,QUAD,SECTOR) 
      LOGICAL MATCH
C----------------------------------------------------------------------
      MATCH=.FALSE.
      NMATCH=0
      CALL VZERO(LOCMATCH,200)
C
C  Loop over the two sets of sectors
C
      DO 10 IFSECT=1,NFSECT
        DO 20 ISSECT=1,NSSECT
          IF( FOVRLP(IFSECT,1).EQ.SOVRLP(ISSECT,1)) THEN        ! check HALF
            IF( FOVRLP(IFSECT,2).EQ.SOVRLP(ISSECT,2)) THEN      ! check UNIT
              IF( FOVRLP(IFSECT,3).EQ.SOVRLP(ISSECT,3)) THEN    ! check QUAD
                IF( FOVRLP(IFSECT,4).EQ.SOVRLP(ISSECT,4)) THEN  ! check SECTOR
                  MATCH=.TRUE.                 ! set TRUE if ANY matches 
                  NMATCH=NMATCH+1                       ! count matches
                  IF(NMATCH.GT.50) GOTO 999
                  LOCMATCH(NMATCH,1)=FOVRLP(IFSECT,1)   ! record matches,HALF
                  LOCMATCH(NMATCH,2)=FOVRLP(IFSECT,2)   ! UNIT
                  LOCMATCH(NMATCH,3)=FOVRLP(IFSECT,3)   ! QUAD
                  LOCMATCH(NMATCH,4)=FOVRLP(IFSECT,4)   ! SECTOR
                ENDIF
              ENDIF
            ENDIF
          ENDIF
   20   CONTINUE
   10 CONTINUE
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
