      FUNCTION GTFSEG(MODULE,ISEG,CONT)
C------------------------------------------------------------------------
C
C    Purpose and Methods : Fetch contents of segment ISEG in HALF,LAYER of FDC
C
C-   Returned value  : Segment status value.
C    Inputs : HALF
C             LAYER
C             ISEG   = Segment number
C    Output : CONT   = Segment parameters
C
C-   Created  xx-JAN-1989   Daria Zieminska
C-   Updated  19-MAR-1990   Jeffrey Bantly  use logical format
C-   Updated   2-JUN-1991   Robert E. Avery  Make into function,
C-                      return status word. (still back combatable)
C-   Updated  17-JUN-1991   Susan K. Blessing  Change size of arrays
C-    to accomodate larger FSGx banks. 
C-   Updated  13-MAR-1992   Susan K. Blessing   Change VZERO call to 
C-    reflect size of banks.
C-   Updated   2-JUN-1992   Susan K. Blessing  Do VZERO only if segment
C-    not found.
C-   Updated  23-AUG-1994   Susan K. Blessing  Remove UCOPY calls. 
C
C------------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      INTEGER GTFSEG
      INTEGER ISEG,LSEGM,LOC,MODULE,LAYER,HALF,I,TEMP(62)
      INTEGER GZFSEG,LZFIND
C
      REAL CONT(*)
C
C------------------------------------------------------------------------
C
      HALF=MODULE/3
      LAYER = MODULE-HALF*3
      LSEGM = GZFSEG(HALF,LAYER)
      IF (LSEGM .LE. 5) THEN
        CALL VZERO(CONT,62)
        GOTO 999
      END IF
      LOC = LZFIND(IXCOM,LSEGM,ISEG,-5)
      IF (LOC.LE.5) THEN
        CALL VZERO(CONT,62)
        GOTO 999
      END IF
C
      DO I = 1, 3
        CONT(I) = Q(LOC+I)
      END DO
C
      IF (LAYER.NE.2) THEN
        DO I = 4, 19
          CONT(I) = FLOAT(IQ(LOC+I))
        END DO
        DO I = 20, 37
          CONT(I) = Q(LOC+I)
        END DO
      ELSE
        DO I = 4, 35
          CONT(I) = FLOAT(IQ(LOC+I))
        END DO
        DO I = 36, 62
          CONT(I) = Q(LOC+I)
        END DO
      END IF
C
      GTFSEG = IQ(LOC)
C
C------------------------------------------------------------------------
  999 RETURN
      END
