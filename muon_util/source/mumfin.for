      SUBROUTINE MUMFIN(NMOD,NFINMX,ITRUNC,ICENFL,NFINE,MFINE)
C-----------------------------------------------------------------
C-
C-    Purpose and Methods : Get fine centroids for one module
C-
C-    Input  :  NMOD   - Module ID
C-              NFINMX - Maximum number of fine centroids
C-
C-    Output :  ITRUNC - Raw data MAC truncation flag
C-              ICENFL - Centroid status : -1 module not found
C-                                          0 centroids not made
C-                                          1 centroids from raw hits
C-                                          2 centroids from proc hits
C-              NFINE  - Number of fine centroids in array
C-              MFINE(NFINMX) - Array of fine centroid addresses
C-
C-    Created :  7-JAN-94  M. Fortner
C-    Modified : 5-JUL-94  MF  Pack module ID into bits 11-13
C-
C-----------------------------------------------------------------
C
      IMPLICIT NONE
      INTEGER NMOD,NFINMX,NFINE,MFINE(NFINMX)
      INTEGER I,J,JFINE,ICRS,IFINE(4),MODID(460),IMODID
      INTEGER IMOD,IMUD1,ITRUNC,NMUHP,IMUHP,JPLN,NMUOH,IMUOH
      INTEGER NMSCT,IMSCT,ISP1,ISP2,ITRIG,ICENFL,ILAT(3)
C
C               Data for chamber id
C
      DATA MODID/
     1       9*0,1,1,3,3,0,1,3,3*0,4*2,0,2,2,3*0,3,3,1,1,0,3,1,3*0,
     2       21*0,1,2,0,3,4,0,5,23*0,1,2,0,3,4,0,5,2*0,
     3       5,5,1,1,5,4,1,1,0,0,4,4,2,2,4,3,2,2,0,0,5*3,0,0,3,0,0,
     4       2,2,4,4,2,2,3,4,0,0,1,1,5,5,1,1,4,5,0,0,1,0,0,2,6*0,
     5       3,5,4,3,1,5,4,2,12*0,2,0,0,1,6*0,3,4,5,3,2,4,5,1,0,0,
     6       1,1,5,5,1,1,4,5,0,0,2,2,4,4,2,2,3,4,0,0,5*3,0,0,3,0,0,
     7       4,4,2,2,4,3,2,2,0,0,5,5,1,1,5,4,1,1,0,0,10*0,
     8       1,4,3,2,1,4,3,2,0,0,2,3,4,1,2,3,4,1,0,0,10*0,
     9       2,3,4,1,2,3,4,1,0,0,1,4,3,2,1,4,3,2,0,0,90*0,
     &       3*2,0,3*1,3*0,1,0,3*2,0,1,1,0,0,3*2,0,3*1,3*0,
     1       1,0,3*2,0,1,1,0,0,3*2,0,3*1,3*0,1,0,3*2,0,1,1,3*0/
C
      NFINE = 0
      ICENFL = -1
      IF (NMOD.LE.0.OR.NMOD.GT.460) GO TO 999
      IMODID = ISHFT(MODID(NMOD),11)
      MFINE(1) = IMODID
C
      CALL GTMUHM(NMOD,IMOD,IMUD1,ITRUNC,NMUHP,IMUHP,JPLN,NMUOH,IMUOH,
     &            NMSCT,IMSCT,ISP1,ISP2,ITRIG,ICENFL,ICRS,IFINE,ILAT)
      IF (IMOD.EQ.0) GOTO 999
C
      DO I = 1,4
          DO J = 0,31
              IF (BTEST(IFINE(I),J)) THEN
                  NFINE = NFINE + 1
                  MFINE(NFINE) = (I-1)*32 + J + IMODID
                  IF (NFINE.EQ.NFINMX) GOTO 999
              ENDIF
          ENDDO
      ENDDO
C
 999  RETURN
      END
