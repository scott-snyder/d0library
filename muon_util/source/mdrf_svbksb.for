      SUBROUTINE MDRF_SVBKSB(U,W,V,M,N,B,X)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Solves Ax = b for a vector X, where A is specified 
C-   by the arrays U, W, V as returned by SVDCMP. A is an M x N matrix.
C-
C-   This code is directly from Numerical Recipes, by Press et al.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  10-MAY-1994   R. Markeloff
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
	INTEGER M, N, I, J, NMAX, MMAX, JJ
C
	PARAMETER (MMAX=5, NMAX=4)
C
	REAL B(MMAX), X(NMAX), S, TMP(MMAX)
	REAL U(MMAX,NMAX), V(NMAX,NMAX), W(NMAX)
C
	DO 12 J=1,N
	   S = 0.
	   IF (W(J) .NE. 0.) THEN
		DO 11 I=1,M
		   S = S + U(I,J)*B(I)
11		CONTINUE
		S = S/W(J)
	   ENDIF
	   TMP(J) = S
12	CONTINUE
C
	DO 14 J=1,N
	   S = 0.
	   DO 13 JJ=1,N
		S = S + V(J,JJ)*TMP(JJ)
13	   CONTINUE
	   X(J) = S
14	CONTINUE
C
	RETURN 
	END
