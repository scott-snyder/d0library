      SUBROUTINE P8LEGO(NX,XMIN,XMAX,NY,YMIN,YMAX,ECUT,ZMAX,TITLE,
     X       XLAB,YLAB,ZLAB,COL1,COL2,COL3,COL4,ARRAY1,ARRAY2,
     X       ARRAY3,ARRAY4,NXMIN,NYMIN,IXG,IYG,N,ZSCAL,IMARK,CALPLOT)
C=====================================================================
C
C   Description:
C   ===========
C          Lego plot OF ARRAY1-4
C
C   INPUT:
C   =====
C       N      - Total number of x elements in ARRAY1
C       NX     - Number of x elements of ARRAY1 that will be displayed
C       XMIN   - Minimum value for X
C       XMAX   - Maximum value for X
C       NY     - Number of y elements of ARRAY1 that will be displayed
C       YMIN   - Minimum value for Y
C       YMAX   - Maximum value for Y
C       ZMIN   - Minimum value for Z
C       ZMAX   - Maximum value for Z
C                (if ZMAX is .LT. 0, prog. will calculate ZMAX from data)
C       TITLE  - Plot title
C       XLAB   - X label
C       YLAB   - Y label
C       ZLAB   - Z label
C       COL1   - Color of bins
C       COL2   - Color for second set of bins
C       ARRAY% - Array of data(%=1:4)
C       IMARK - 0 if no marks, 1 if special marks (ex. JET center or missing PT)
C       CALPLOT - Tells what kind of plot is doing .FALSE. Cal Plot ET
C                                                  .TRUE.  cal Plot E
C
C   OUTPUT:
C   =======
C     LEGO PLOT
C
C   Author:
C   =======
C       Lupe Rosas
C
C   Revision History:
C   =================
C      Jun 14, 1988 - Original Creation
C      Aug 16, 1994 - Nobuaki Oshima, Modified to handle 4 Arrays
C
C=====================================================================
      IMPLICIT NONE
C=====================================================================
C    Argument Declarations:
C    ======================
      CHARACTER*(*) XLAB,YLAB,ZLAB
      CHARACTER*(*) TITLE
      CHARACTER*(*) COL1,COL2,COL3,COL4
      INTEGER NX,NY,NXMIN,NYMIN,N,IXG,IYG, IMARK
      REAL ZSCAL,ZDIV
      REAL ARRAY1(1:N,1:*),ARRAY2(1:N,1:*),ARRAY3(1:N,1:*),
     X        ARRAY4(1:N,1:*),XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,ECUT
      REAL RLEVEL
      LOGICAL CALPLOT
C=====================================================================
C    Local Declaration:
C    ==================
      REAL TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,
     X     XDIR,YDIR,ZDIR,TEMP,UMIN,UMAX,VMIN,VMAX,
     X     WX,WY,WZ,XSIZE,YSIZE,PLZMAX,ZMED
      INTEGER I,J,IETA,IPHI,XNUM,YNUM,SEGNUM
      INTEGER ICOLOR(4)
      CHARACTER*38 IMESS1
      CHARACTER*3 COLORS(4)
      CHARACTER*15 LABELS(4)
C    Data Statements:
C    ================
      DATA IMESS1/' P8LEGO - No data inside given bounds '/
C=====================================================================
      CALL JIQDIL(RLEVEL)
      XSIZE=1.
      YSIZE=1.
      ZMIN=0.
C    DETERMINING ZMAX
C--- ADD FOR FIXEMAX IS TRUE IN COMBINED VIEW( Nobu. 14-JUL-92).
      IF(ZMAX.LE.0)THEN   
        ZMAX=PLZMAX(NXMIN,NX,NYMIN,NY,ARRAY1,ARRAY3,ZMAX,N,2)
      ENDIF
C---
      IF(ZMAX.LE.0)THEN
        CALL PUMESS(IMESS1)
        GO TO 999
      ENDIF
C Set viewing parameters
      IF(RLEVEL .NE. -2.) THEN
        CALL PLSETV(NX,NY,UMIN,UMAX,VMIN,VMAX)
      ENDIF
C  MAKING GRID
      CALL PLGRID(NXMIN,NX,NYMIN,NY,IXG,IYG,XMIN,XMAX,
     X        YMIN,YMAX)
C DRAW Z-AXIS
      ZMED=(XMAX+ZMIN)/2.
      CALL PLZAXS(NXMIN,NYMIN,ZMAX,ZMIN,ZMED,ZSCAL)
C  BUILDING BLOCKS
      CALL PUOPEN
      DO 100 IPHI=NXMIN,NX
        J = NX - IPHI + NXMIN
        DO 100 IETA=NYMIN,NY
C  DEFINE BAR
          TXMIN=XSIZE*FLOAT(J-NXMIN)+FLOAT(NXMIN)
          TXMAX=TXMIN+1.0*XSIZE
          TYMIN=YSIZE*FLOAT(IETA-NYMIN)+FLOAT(NYMIN)
          TYMAX=TYMIN+YSIZE
          TZMIN=0.
          TZMAX=0.
          IF ((ARRAY1(J,IETA)+ARRAY2(J,IETA)+ARRAY3(J,IETA)
     &        +ARRAY4(J,IETA)) .LT. ECUT)   GO TO 60 
          CALL JPEDGE(0)
C-1 
          IF (ARRAY1(J,IETA).GT.0.) THEN
            TZMAX=ARRAY1(J,IETA)/(ZMAX*ZSCAL)
            IF(RLEVEL .EQ. -2.) THEN
              CALL PXCOLR(COL1)
              CALL PLDBAR(6,TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,1)
            ELSE
              CALL PXCOLFILL(COL1)
              CALL PLDBAR(3,TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,1)
            ENDIF
          ENDIF
C-2
          IF (ARRAY2(J,IETA).GT.0.) THEN
            TZMIN=TZMAX
            TZMAX=ARRAY2(J,IETA)/(ZMAX*ZSCAL) + TZMAX
            IF(RLEVEL .EQ. -2.) THEN
              CALL PXCOLR(COL2)
              CALL PLDBAR(6,TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,2)
            ELSE
              CALL PXCOLFILL(COL2)
              CALL PLDBAR(3,TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,2)
            ENDIF
          ENDIF
C-3
          IF (ARRAY3(J,IETA).GT.0.) THEN
            TZMIN=TZMAX
            TZMAX=ARRAY3(J,IETA)/(ZMAX*ZSCAL) + TZMAX
            IF(RLEVEL .EQ. -2.) THEN
              CALL PXCOLR(COL3)
              CALL PLDBAR(6,TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,2)
            ELSE
              CALL PXCOLFILL(COL3)
              CALL PLDBAR(3,TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,2)
            ENDIF
          ENDIF
C-4
          IF (ARRAY4(J,IETA).GT.0.) THEN
            TZMIN=TZMAX
            TZMAX=ARRAY4(J,IETA)/(ZMAX*ZSCAL) + TZMAX
            IF(RLEVEL .EQ. -2.) THEN
              CALL PXCOLR(COL4)
              CALL PLDBAR(6,TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,2)
            ELSE
              CALL PXCOLFILL(COL4)
              CALL PLDBAR(3,TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,2)
            ENDIF
          ENDIF
C-END
   60     CONTINUE
  100 CONTINUE
      TXMIN=0.
      TXMAX=XMAX*10.+1.
      TYMIN=0.
      TYMAX=YMAX*20.+1.
C MAKE SPECIAL MARKS IF REQUESTED
      IF(IMARK.NE.0)THEN
        ZDIV=ZMAX*ZSCAL
        CALL PLMARK(IMARK,TXMIN,TXMAX,TYMIN,TYMAX,ZDIV)
      ENDIF
      CALL JRCLOS
C   PRINTING AXIS
      CALL PLABEL(VMIN,XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,XLAB,YLAB,ZLAB,
     X       TYMAX,TXMAX,TZMAX,NX,NXMIN,NY,NYMIN,ZSCAL)
C  PRINTING TITLE
C NOTE: THIS ROUTINE SETS VIEWING PARAMETES TO X-Y VIEW
      CALL PXTITL(TITLE)
CC  PRINTING LEGEND
      IF ( CALPLOT ) THEN               !  Cal Plot E
        LABELS(1) = ' EM E          '
        LABELS(2) = ' ICD/MG E      '
        LABELS(3) = ' FH E          '
        LABELS(4) = ' CH E          '
      ELSE
        LABELS(1) = ' EM ET         '
        LABELS(2) = ' ICD/MG ET     '
        LABELS(3) = ' FH ET         '
        LABELS(4) = ' CH ET         '
      ENDIF
      COLORS(1) = COL1
      COLORS(2) = COL2
      COLORS(3) = COL3
      COLORS(4) = COL4
      IF(RLEVEL .EQ. -2.) THEN
        DO I=1,4
          CALL PXCOLCTOI(COLORS(I),ICOLOR(I))
        ENDDO
        CALL LEGEND3D(ICOLOR,LABELS,4)
      ELSE
        CALL LEGEND(COLORS,LABELS,4)
      ENDIF
  999 RETURN
      END
