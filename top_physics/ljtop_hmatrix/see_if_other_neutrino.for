      SUBROUTINE SEE_IF_OTHER_NEUTRINO(PT_MAX,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SEE IF EVENT CONTAINS MUON
C-
C-   Inputs  :PT_MAX = maximum pt of missing stuff allowed
C-   Outputs :IER not zero if other neutrino present.
C-   Controls:
C-
C-   Created   5-JAN-1991   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INTEGER LISAL
      INTEGER IPART,IER,GZISAL
      EQUIVALENCE (LISAL,LBANK)
      REAL    PT_MISS,PT_MAX
      CHARACTER*80 MSG
      CHARACTER*20 PARTICLE(10)
C
      INTEGER BAD_PERM(5)
      INTEGER I,NPART
      INTEGER BAD_COUNT(5),AIPART
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('HMATRIX_RCP')
        CALL EZ_GET_CHARS('PROHIBITED_PARTICLE_NAMES',NPART,
     &    PARTICLE,IER)
        CALL EZGET('PROHIBITED_PARTICLE_COUNT',BAD_PERM,IER)
        CALL EZRSET
      ENDIF
      IER = 0
      LISAL = GZISAL()
      DO I = 1 ,NPART
        BAD_COUNT(I) = 0
      ENDDO
      DO WHILE (LISAL.NE.0)
        IPART = IQ(LISAL+1)
        AIPART = IABS(IPART)-10
C
        PT_MISS = SQRT(Q(LISAL+2)**2 + Q(LISAL+3)**2)
        IF(PT_MISS.GT.PT_MAX)THEN
          BAD_COUNT(AIPART) = BAD_COUNT(AIPART) + 1
        ENDIF
C
        LISAL = LQ(LISAL)
      ENDDO
      DO I = 1 , NPART
        IF(BAD_COUNT(I).GT.BAD_PERM(I))THEN
          IER = 1
          WRITE(MSG,1)PARTICLE(I),PT_MAX
    1     FORMAT(A20,' Present with Et greater than 'F6.3)
          CALL ERRMSG('HMATRIX','SEE_IF_OTHER_NEUTRINO',
     &      MSG,'W')
        ENDIF
      ENDDO
  999 RETURN
      END
