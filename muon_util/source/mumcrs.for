      SUBROUTINE MUMCRS(NMOD,ITRUNC,ICENFL,MCRS)
C-----------------------------------------------------------------
C-
C-    Purpose and Methods : Get coarse centroids for one module
C-
C-    Input  :  NMOD   - Module ID
C-
C-    Output :  ITRUNC - Raw data MAC truncation flag
C-              ICENFL - Centroid status : -1 module not found
C-                                          0 centroids not made
C-                                          1 centroids from raw hits
C-                                          2 centroids from proc hits
C-              MCRS(16) - Array of coarse centroids
C-
C-    Created :  7-JAN-94  M. Fortner
C-
C-----------------------------------------------------------------
C
      IMPLICIT NONE
      INTEGER NMOD,MCRS(16)
      INTEGER I
      INTEGER IMOD,IMUD1,ITRUNC,NMUHP,IMUHP,JPLN,NMUOH,IMUOH
      INTEGER NMSCT,IMSCT,ISP1,ISP2,ITRIG,ICENFL,JCRS,JFINE(4),ILAT(3)
C
      ICENFL = -1
      CALL GTMUHM(NMOD,IMOD,IMUD1,ITRUNC,NMUHP,IMUHP,JPLN,NMUOH,IMUOH,
     &            NMSCT,IMSCT,ISP1,ISP2,ITRIG,ICENFL,JCRS,JFINE,ILAT)
C
      IF (IMOD.NE.0) THEN
          DO I = 1,16
              MCRS(I) = IBITS(JCRS,I-1,1)
          ENDDO
      ENDIF
C
      RETURN
      END
