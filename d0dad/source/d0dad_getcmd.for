      SUBROUTINE D0DAD_GETCMD(IC,IARGC,ARGV,LENARG,LOPT,CMD)
C-----------------------------------------------------------------------
C  Return the ic-th non-option from the command line.
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IC
      CHARACTER*(132) CMD,ARGV(*)
      INTEGER IARGC,LENARG(*)
      LOGICAL LOPT(*)
      INTEGER I,ITMP
C
      ITMP=0
      DO I=1,IARGC
         IF( .NOT.LOPT(I) ) ITMP=ITMP+1
         IF( ITMP.EQ.IC .AND. LENARG(I).GT.0 ) THEN
            CMD=ARGV(I)(1:LENARG(I))
            GOTO 999
         ENDIF
      ENDDO
C
 999  CONTINUE
      RETURN
      END
