      SUBROUTINE AMOEBA(FUN,FTOL,ITMAX,NDIM,P,Y,RTOL,ITER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find the minimum of a function FUN of NDIM
C-   variables using the Nelder Mead simplex algorithm. This code was
C-   copied from The Art of Numerical Analysis (...I'll get the exact
C-   reference later!). 
C-   
C-   A simplex is a convex object defined by NDIM + 1 points where NDIM
C-   is the dimension of the space. So in 2-D, the simplex is just a
C-   triangle. One begins by giving the starting points of the simplex:
C-   
C-      P(NDIM+1,NDIM)
C-      
C-   where the first argument labels the point and the second gives the
C-   components which define the point. One also supplies the function   
C-   value Y(NDIM+1) at each of these points. The algorithm proceeds by
C-   shifting the points around so that the simplex converges to the
C-   minimum. The simplex behaves like an amoeba which eventually shrinks
C-   to zero area at the minimum. The output points and values are returned in
C-   P and Y.
C-
C-   Inputs  : FUN(X)         [R]   Real*4 function where X is a point defined
C-                                  in an NDIM array.
C-             FTOL           [R]   Tolerance (say 1.E-08). Look at the code 
C-                                  for now.
C-             ITMAX          [I]   Number of iterations (say 500)
C-             NDIM           [I]   Dimension of space
C-             P(NDIM+1,NDIM) [R]   Points
C-             Y(NDIM+1)      [R]   Function values
C-             
C-   Outputs : P(NDIM+1,NDIM) [R]   Final Points
C-             Y(NDIM+1)      [R]   Final Function values
C-             RTOL           [R]   Actual tolerance achieved
C-             ITER           [I]   Actual number of iterations
C-   Controls:
C-
C-   Created  10-FEB-1993   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    FUN
      EXTERNAL FUN
      REAL    FTOL
      INTEGER ITMAX, NDIM
      REAL    P(NDIM+1,*),Y(*),RTOL
      INTEGER ITER
C----------------------------------------------------------------------
      INTEGER NMAX
      PARAMETER( NMAX = 20 )
      REAL    ALPHA, BETA, GAMMA
      PARAMETER( ALPHA = 1.0, BETA = 0.5, GAMMA = 2.0 )
C
      INTEGER MPTS, ILO, IHI, INHI, I, J
      REAL    PR(NMAX),PRR(NMAX),PBAR(NMAX), YPR, YPRR
C----------------------------------------------------------------------
      MPTS = NDIM + 1
      ITER = 0
C
C ****  First determine which point is the highest, next-highest and
C ****  lowest by looping over points of simplex.
C
    1 CONTINUE
      ILO  = 1
      IF ( Y(1) .GT. Y(2) ) THEN
        IHI = 1
        INHI= 2
      ELSE
        IHI = 2
        INHI= 1
      ENDIF
C
      DO 11 I =  1, MPTS
        IF ( Y(I) .LT. Y(ILO) ) THEN
          ILO = I
        ENDIF
        IF     ( Y(I) .GT. Y(IHI) ) THEN
          INHI = IHI
          IHI  = I
        ELSEIF ( Y(I) .GT. Y(INHI) ) THEN
          IF ( I .NE. IHI ) THEN
            INHI = I
          ENDIF
        ENDIF
   11 CONTINUE
C
C ****  Compute fractional change from the highest to lowest and
C ****  return if satisfactory.
C
      RTOL = 2.0*ABS(Y(IHI)-Y(ILO))/(ABS(Y(IHI))+ABS(Y(ILO)))
      IF ( RTOL .LT. FTOL )  GOTO 999
      IF ( ITER .GE. ITMAX ) GOTO 999
C
C ****  Begin a new iteration. Compute the vector average of all points
C ****  except the highest, ie., the center of the "face" of the
C ****  simplex across from the highest point. Explore along ray from the
C ****  highest point through that center.
C
      ITER = ITER + 1
      DO 12 J = 1, NDIM
        PBAR(J) = 0.0
   12 CONTINUE
C
      DO 14 I =  1, MPTS
        IF ( I .NE. IHI ) THEN
          DO 13 J =  1, NDIM
            PBAR(J) = PBAR(J) + P(I,J)
   13     CONTINUE
        ENDIF
   14 CONTINUE
C
C ****  Extrapolate by a factor ALPHA through the face, ie., reflect
C ****  the highest point through the "face".
C
      DO 15 J =  1, NDIM
        PBAR(J) = PBAR(J)/NDIM
        PR(J) = (1.0+ALPHA)*PBAR(J)-ALPHA*P(IHI,J)
   15 CONTINUE
C
C ****  Evaluate the function at the reflected point.
C
      YPR = FUN(PR)
      IF ( YPR .LE. Y(ILO) ) THEN
C
C ****  New point lower the current best point; try additional
C ****  extrapolation by a factor GAMMA and check value of function there
C
        DO 16 J =  1, NDIM
          PRR(J) = GAMMA*PR(J) + (1.0-GAMMA)*PBAR(J)
   16   CONTINUE
        YPRR = FUN(PRR)
C
        IF ( YPRR .LT. Y(ILO) ) THEN
C
C ****  Good extrapolation; replace highest point with new point
C
          DO 17 J =  1, NDIM
            P(IHI,J) = PRR(J)
   17     CONTINUE
          Y(IHI) = YPRR
        ELSE
C
C ****  Bad extrapolation; use reflected point because it's still ok.
C
          DO 18 J =  1, NDIM
            P(IHI,J) = PR(J)
   18     CONTINUE
          Y(IHI) = YPR
        ENDIF
C
      ELSEIF ( YPR .GE. Y(INHI) ) THEN
C
C ****  The reflected point is worse than the second highest point.
C ****  If it is better than the highest point replace the latter
C ****  with it.
C
        IF ( YPR .LT. Y(IHI) ) THEN
          DO 19 J =  1, NDIM
            P(IHI,J) = PR(J)
   19     CONTINUE
          Y(IHI) = YPR
        ENDIF
C
C ****  Look for an intermediate lower point, i.e, perform a
C ****  contraction of simplex along one dimension. Then check function
C ****  value.
C
        DO 21 J =  1, NDIM
          PRR(J) = BETA*P(IHI,J) + (1.0-BETA)*PBAR(J)
   21   CONTINUE
        YPRR = FUN(PRR)
C
        IF ( YPRR .LT. Y(IHI) ) THEN
C
C ****  Contraction improves so accept it
C
          DO 22 J =  1, NDIM
            P(IHI,J) = PRR(J)
   22     CONTINUE
          Y(IHI) = YPRR
        ELSE
C
C ****  Contraction doesn't help so just contract around lowest point
C
          DO 24 I =  1, MPTS
            IF ( I .NE. ILO ) THEN
              DO 23 J =  1, NDIM
                PR(J) = 0.5*(P(I,J)+P(ILO,J))
                P(I,J)= PR(J)
   23         CONTINUE
              Y(I) = FUN(PR)
            ENDIF
   24     CONTINUE
        ENDIF
      ELSE
C
C ****  Here if the original reflection gives a so-so point.
C ****  replace the old highest point and continue.
C
        DO 25 J =  1,  NDIM
          P(IHI,J) = PR(J)
   25   CONTINUE
        Y(IHI) = YPR
      ENDIF
      GOTO 1
C
  999 RETURN
      END
