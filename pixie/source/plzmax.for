      REAL FUNCTION PLZMAX(NXMIN,NX,NYMIN,NY,ARRAY1,ARRAY2,ZMAX,N,
     X                DATNUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find the highest value inside two data arrays that
C-          will be displayed together in a plot.  
C-
C-   Inputs  :  NXMIN  - The minimum value in the x axis in the array 
C-              NX     - Maximum value in the x axis in the array
C-              NYMIN  - The minimum value in the y axis in the array
C-              NY     - The maximum value in the y axis in the array
C-              ARRAY1 - Data array
C-              ARRAY2 - Data array
C-              ZMAX   - Maximum value found in the array(s)
C-              N      - X size of the arrays that contain the data
C-              DATNUM - Control parameter to tell if the ARRAY2 has data 
C-                         0 - no data in array2
C-                         anything else assume both arrays will be consider 
C-   Outputs :  ZMAX   - Maximum value in the array(s)
C-
C-   Created  26-JUL-1988   LUPE ROSAS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C-   Argument Declarations:
C-   ----------------------
      INTEGER N,NXMIN,NX,NYMIN,NY,DATNUM
      REAL ARRAY1(1:N,1:*),ARRAY2(1:N,1:*),ZMAX
C----------------------------------------------------------------------
C-   Local Declarations:
C-   -------------------
      REAL MAX,TEMP
      INTEGER I,J
C----------------------------------------------------------------------
       MAX = 0.

       DO 20 I=NXMIN, NX
         DO 20 J=NYMIN, NY
         IF (DATNUM.NE.0)THEN
           IF ((ARRAY1(I,J).GT.0).AND.
     X          (ARRAY2(I,J).GT.0)) THEN
             TEMP = ARRAY1(I,J) + ARRAY2(I,J)
             IF ((ZMAX.GT.0).AND.(ZMAX.LT.TEMP))
     X         ZMAX = TEMP
           ELSE IF (ARRAY1(I,J).LT.ARRAY2(I,J)) THEN
             TEMP = ARRAY2(I,J)
           ELSE 
             TEMP = ARRAY1(I,J)
           END IF
         ELSE
           TEMP = ARRAY1(I,J)
         ENDIF
         IF (MAX.LT.TEMP)
     X     MAX = TEMP
 20    CONTINUE
       IF(ZMAX.LE.0) THEN
         PLZMAX = MAX
       ELSE
        PLZMAX=ZMAX
      ENDIF
  999 RETURN
      END
