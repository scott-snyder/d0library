      SUBROUTINE P2LEGO(NX,XMIN,XMAX,NY,YMIN,YMAX,ECUT,ZMAX,TITLE,
     X       XLAB,YLAB,ZLAB,COL1,COL2,ARRAY1,ARRAY2,
     X       NXMIN,NYMIN,IXG,IYG,N,ZSCAL,IMARK,CALPLOT)
C=====================================================================
C
C   Description:
C   ===========
C          Lego plot OF ARRAY1
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
C       ARRAY1 - Array of data
C       ARRAY2 - Array of data
C       IMARK - 0 if no marks, 1 if special marks (ex. JET center or missing PT)
C       CALPLOT - Tells what kind of plot is doing .FALSE. Cal Plot ET
C                                                  .TRUE.  cal Plot E
C
C   OUTPUT:
C   =======
C     LEGO PLOT
C
C   Preconditions necessary before call:
C     NONE
C
C   Author:
C   =======
C       Lupe Rosas
C
C   Revision History:
C   =================
C      Jun 14, 1988 - Original Creation
C      Dec 19, 1989 - Updated Lupe Rosas Implementing PIXIE color table
C      Aug 18, 1991 - Nobuaki Oshima
C            ( Modify for color and 3D LEGO rotating on E&S. )
C      Jan 20, 1992 - Nobuaki Oshima ( Correct IMESS1 size. )
C      Mar 15, 1993 - Nobuaki Oshima 
C            ( Added 'CALL PLMARK_TRGR' to show Miss Et in TRGR_LEGO. )
C
C=====================================================================
      IMPLICIT NONE
C=====================================================================
C    Argument Declarations:
C    ======================
      CHARACTER*(*) XLAB,YLAB,ZLAB
      CHARACTER*(*) TITLE
      CHARACTER*(*) COL1, COL2
      INTEGER NX,NY,NXMIN,NYMIN,N,IXG,IYG,
     X        IMARK
      REAL ZSCAL,ZDIV
      REAL ARRAY1(1:N,1:*),ARRAY2(1:N,1:*),XMIN,XMAX,YMIN,YMAX,
     X        ZMIN,ZMAX,ECUT
      REAL RLEVEL
      LOGICAL CALPLOT
C=====================================================================
C    Local Declaration:
C    ==================
      REAL TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,
     X     XDIR,YDIR,ZDIR,TEMP,UMIN,UMAX,VMIN,VMAX,
     X     WX,WY,WZ,XSIZE,YSIZE,PLZMAX,ZMED
      INTEGER I,J,IETA,IPHI,XNUM,YNUM,SEGNUM
      INTEGER ICOLOR(2)
      CHARACTER*38 IMESS1
      CHARACTER*3 COLORS(2)
      CHARACTER*15 LABELS(2)
C    Data Statements:
C    ================
      DATA IMESS1/' P2LEGO - No data inside given bounds '/
C=====================================================================
      CALL JIQDIL(RLEVEL)
      XSIZE=1.
      YSIZE=1.
      ZMIN=0.
C    DETERMINING ZMAX
C--- ADD FOR FIXEMAX IS TRUE IN COMBINED VIEW( Nobu. 14-JUL-92).
      IF(ZMAX.LE.0)THEN   
        ZMAX=PLZMAX(NXMIN,NX,NYMIN,NY,ARRAY1,ARRAY2,ZMAX,N,2)
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
          IF ((ARRAY1(J,IETA)+ARRAY2(J,IETA)).LT.ECUT)
     &        GO TO 60                   ! Checking for Ecut
          TZMAX=ARRAY1(J,IETA)/(ZMAX*ZSCAL)
          CALL JPEDGE(0)
          IF (TZMAX.GT.0) THEN               ! Draw the first bin
            IF(RLEVEL .EQ. -2.) THEN
              CALL PXCOLR(COL1)
              CALL PLDBAR(6,TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,1)
            ELSE
              CALL PXCOLFILL(COL1)
              CALL PLDBAR(3,TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,1)
            ENDIF
            IF (ARRAY2(J,IETA).GT.0) THEN    ! Draw second bin on top the first
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
          ELSEIF (ARRAY2(J,IETA).GT.0) THEN  ! Draw the second bin
            TZMIN=0.
            TZMAX=ARRAY2(J,IETA)/(ZMAX*ZSCAL)
            IF(RLEVEL .EQ. -2.) THEN
              CALL PXCOLR(COL2)
              CALL PLDBAR(6,TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,2)
            ELSE
              CALL PXCOLFILL(COL2)
              CALL PLDBAR(3,TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,2)
            ENDIF
          ENDIF
   60     CONTINUE
  100 CONTINUE
      TXMIN=0.
      TXMAX=XMAX*10.+1.
      TYMIN=0.
      TYMAX=YMAX*20.+1.
C MAKE SPECIAL MARKS IF REQUESTED
      IF(IMARK.NE.0)THEN
        ZDIV=ZMAX*ZSCAL
        IF(TITLE .EQ. 'ET of TRGR in ETA-PHI') THEN
          CALL PLMARK_TRGR(IMARK,TXMIN,TXMAX,TYMIN,TYMAX,ZDIV)
        ELSE
          CALL PLMARK(IMARK,TXMIN,TXMAX,TYMIN,TYMAX,ZDIV)
        ENDIF
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
        LABELS(2) = ' HAD E         '
      ELSE
        LABELS(1) = ' EM ET         '
        LABELS(2) = ' HAD ET        '
      ENDIF
      COLORS(1) = COL1
      COLORS(2) = COL2
      IF (COL1.EQ.'MAG' .AND. COL2.EQ.'YEL') THEN
        IF ( CALPLOT ) THEN               !  Cal Plot E
          LABELS(1) = ' MG E          '
          LABELS(2) = ' ICD E         '
        ELSE
          LABELS(1) = ' MG ET         '
          LABELS(2) = ' ICD ET        '
        ENDIF
      ENDIF
      IF(RLEVEL .EQ. -2.) THEN
        DO I=1,2
          CALL PXCOLCTOI(COLORS(I),ICOLOR(I))
        ENDDO
        CALL LEGEND3D(ICOLOR,LABELS,2)
      ELSE
        CALL LEGEND(COLORS,LABELS,2)
      ENDIF
  999 RETURN
      END
