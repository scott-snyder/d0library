      SUBROUTINE Q_GET_UNKNOWNS(LUN,P,B,R_OR_U,PDEAD,SOLVE,PER_SERVER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Solve for unknowns in queue and write to file
C-
C-   Inputs  : LUN  unit to write results (0 means don't write to file)
C-                                        (- means don't write at all)
C-             P    # processors
C-             B    # extra buffers outside processors
C-             R_OR_U  traffic intensity/node in erlangs
C-                    (but see PER_SERVER)
C-             PDEAD probability of signal loss
C-   Outputs : one of P,B,U,PDEAD (according SOLVE)
C-   Controls: PER_SERVER .TRUE. if R is really per server
C-                        .FALSE. if R is really u
C-                  (only relevant if solving for P)
C-             SOLVE
C-                    'R' solve for R(P,B,PDEAD)
C-                    'D' solve for PDEAD(P,B,U)
C-                    'P' solve for P(B,R,PDEAD)
C-                    'B' solve for B(U,P,PDEAD)
C
C-   Created  21-JUN-1993   James T. Linnemann
C-   Updated  16-AUG-1995   James T. Linnemann   separate P and B; R = U/C
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LUN,P,K,B,N0,NMAX,MAX_CALLS,IPASS
      LOGICAL PER_SERVER
      REAL    R_OR_U,R,PDEAD,LEAST
      REAL    DEAD_FRAC,PROB_QUEUE_BUSY,RMIN,RMAX,DELTA_R,ACT_ERR
      EXTERNAL R_ERLANG_SOLVE
      INCLUDE 'D0$INC:QUEUE_SOLVE.INC'
      CHARACTER*1 SOLVE
      LOGICAL EXACT,WRITTEN
      COMMON/IF_EXACT/EXACT
      DATA LEAST/.00005/  !won't print if less than .005 %
C----------------------------------------------------------------------
      R = R_OR_U
      K = P + B
      WRITTEN = .FALSE.
      DO IPASS = 1,2
        EXACT = IPASS.EQ.1
        IF (SOLVE.EQ.'D') THEN
          PDEAD = PROB_QUEUE_BUSY(P,K,R) !overwrite PDEAD when solving for D
          DEAD_FRAC = PDEAD   !for writing out
        ELSEIF (SOLVE.EQ.'R') THEN
          P_DEAD_IN = PDEAD !set up common block for R_ERLANG_SOLVE
          K_IN = K
          P_IN = P
          RMIN = 1.0E-10
          RMAX = 1000.
          DELTA_R = .001    !limit on dR/(1+Abs(R))
          MAX_CALLS = 10000
          CALL RZERO(RMIN,RMAX,R,ACT_ERR,DELTA_R,MAX_CALLS,
     &      R_ERLANG_SOLVE)
          DEAD_FRAC = PROB_QUEUE_BUSY(P,K,R) ! write out actual value
        ELSEIF (SOLVE.EQ.'P') THEN  !Solve for minimum N achieving deadtime goal
          N0 = 1
          NMAX = 1000
          DO P = N0,NMAX      !overwrite N when solving for it
            K = P + B
            IF(.NOT.PER_SERVER) R = R_OR_U/P
            DEAD_FRAC = PROB_QUEUE_BUSY(P,K,R)
            IF (DEAD_FRAC.LT.PDEAD) GO TO 50
          ENDDO
          P = NMAX+1
          K = P + B
   50     CONTINUE
        ELSEIF (SOLVE.EQ.'B') THEN !Find minimum Buffers achieving deadtime goal
          N0 = P
          NMAX = 1000
          DO K = N0,NMAX      !overwrite N when solving for it
            DEAD_FRAC = PROB_QUEUE_BUSY(P,K,R)
            IF (DEAD_FRAC.LT.PDEAD) GO TO 60
          ENDDO
          K = NMAX+1
   60     CONTINUE
        ELSE
          WRITE(6,*)' unknown SOLVE option = ',SOLVE
        ENDIF
        B = K - P   !deduce number of buffers

        IF (EXACT.AND.(DEAD_FRAC.GT.LEAST)) THEN
          CALL Q_WRITE_SOLN(LUN,P,B,R,DEAD_FRAC,EXACT)
          CALL Q_WRITE_SOLN(6,P,B,R,DEAD_FRAC,EXACT)
          WRITTEN = .TRUE.
        ELSEIF ((.NOT.EXACT).AND.WRITTEN) THEN
          CALL Q_WRITE_SOLN(LUN,P,B,R,DEAD_FRAC,EXACT)
          CALL Q_WRITE_SOLN(6,P,B,R,DEAD_FRAC,EXACT)
        ENDIF
      ENDDO
  999 RETURN
      END
