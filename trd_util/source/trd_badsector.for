      SUBROUTINE TRD_BADSECTOR(PHI,BAD_SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Returned value  : TRUE if run bad
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  28-DEC-1994   A. ZYLBERSTEJN
C-   Updated  24-JUN-1995   Lewis Taylor Goss   
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER ILIM,LOC,IER,N,N_ZONES,I,K,LEN
      PARAMETER (ILIM=100)
      INTEGER NRUN,RUNNO,SECTOR,RUN_BEGIN(1:ILIM),RUN_END(1:ILIM)
      INTEGER SECTOR_HEX(1:ILIM),CONHEX,JBYT
      REAL PHI
      CHARACTER*4 SECTOR_CHAR(1:ILIM)
      LOGICAL BAD_SECTOR,FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZLOC ('TRD_ANALYSIS_RCP',LOC)
        IF (LOC.LE.0) CALL INRCP ('TRD_ANALYSIS_RCP',IER)
        CALL EZPICK ('TRD_ANALYSIS_RCP')
        CALL EZGETA ('ANODE_BAD_SECTOR',0,0,0,N,IER)
        N_ZONES = N / 3
        DO I = 1,N_ZONES
          K = (I-1)*3
          CALL EZGETA ('ANODE_BAD_SECTOR',K+1,K+1,1,RUN_BEGIN(I),IER)
          CALL EZGETA ('ANODE_BAD_SECTOR',K+2,K+2,1,RUN_END(I),IER)
          CALL EZGETS ('ANODE_BAD_SECTOR',I,SECTOR_CHAR(I),LEN,IER)
          SECTOR_HEX(I) = CONHEX(SECTOR_CHAR(I))
        ENDDO
        CALL EZRSET
      ENDIF
C
      NRUN=RUNNO()
C      SECTOR=PHI/(16.*0.02454)+1.
      SECTOR=(PHI*256./(16.*TWOPI))+1.
      BAD_SECTOR=.FALSE.
      DO I = 1,N_ZONES
        IF (NRUN.GE.RUN_BEGIN(I).AND.NRUN.LE.RUN_END(I)
     &    .AND.JBYT(SECTOR_HEX(I),SECTOR,1).EQ.0) THEN
          BAD_SECTOR = .TRUE.
          GOTO 999
        ENDIF
      ENDDO
C run 1a
C      IF(NRUN.LT.60376 .OR. NRUN.GT.87065)GO TO 999
C      IF(NRUN.EQ.60376  .OR. NRUN.EQ.60377 .OR.
C     +  (NRUN.GE.60414 .AND. NRUN.LE.60417).OR.
C     +  (NRUN.GE.64277 .AND. NRUN.LE.64278)
C     &  .AND.(SECTOR.LE.4 .OR.SECTOR.GE.13))GO TO 100
C      IF(NRUN.GE.64275 .AND. NRUN.LE.64500 .AND.
C     &                       (SECTOR.EQ.7 .OR.SECTOR.EQ.11))GO TO 100
C run 1b
C      IF(NRUN.LT.86871 .OR. NRUN.GT.87065)GO TO 999
C      IF(NRUN.EQ.87062)GO TO 999
C      IF(NRUN.LT.87061 .AND. SECTOR.EQ.2)GO TO 100 ! bad sector 2
C      IF(NRUN.GE.86901 .AND. SECTOR.EQ.15)GO TO 100 !bad sector 15
C      IF(NRUN.GE.87047 .AND.NRUN.LT.87062 .AND.
C     &  (SECTOR.EQ.1 .OR.SECTOR.EQ.16)) GO TO 100
C  100 BAD_SECTOR=.TRUE.
  999 CONTINUE
      RETURN
      END
