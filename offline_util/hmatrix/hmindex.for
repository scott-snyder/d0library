      FUNCTION HMINDEX(I,J,NIDIM,NJDIM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : WORKS OUT A LINEAR INDEX IN A 2D MATRIX
C-                         referred to by index I,J with NIDIM members
C-                         of variable I and NJDIM variables of variable J.
C-                         I runs fastest.
C-
C-   Returned value  : HMINDEX = Index to the linear array
C-   Inputs  : I,J indices to a two Dimensional array of 
C-             dimension NIDIM*NJDIM
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-DEC-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER HMINDEX,I,J,NIDIM,NJDIM
C----------------------------------------------------------------------
      HMINDEX = I + (J-1)*NIDIM
      IF ( HMINDEX.GT.NIDIM*NJDIM ) THEN
        CALL ERRMSG('HMATRIX','HMINDEX',
     &    'INDICES EXCEED ARRAY DIMENSION ','W')
      ENDIF
  999 RETURN
      END
