C=====================================================================
       SUBROUTINE P3LEGO(NX,XMIN,XMAX,NY,YMIN,YMAX,ECUT,ZMAX,TITLE,
     X      XLAB,YLAB,ZLAB,ARRAY1,IARRAY,NXMIN,NYMIN,
     X       IXG,IYG,N,ZSCAL,IMARK)
C=====================================================================
C
C   Description:
C   ===========
C          Lego plot OF ARRAY1 (FAST PLOT; front and top of bins only)
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
C       ARRAY1 - Array of data
C       IARRAY - COLOR CODE for data
C       IMARK - 0 if no marks, 1 if special marks (ex. JET center or missing PT)
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
C      Aug 18, 1991 - Nobuaki Oshima
C            ( Modify for color and 3D LEGO rotating on E&S. )
C      Aug 18, 1993 - C. Stewart
C            ( Change the color scheme )
C
C=====================================================================
      IMPLICIT NONE
C=====================================================================
C    Argument Declarations:
C    ======================
      CHARACTER*(*) XLAB,YLAB,ZLAB
      CHARACTER*(*) TITLE
      INTEGER NX,NY,COL1,COL2,NXMIN,NYMIN,N,IXG,IYG,IMARK
      REAL ZSCAL
      REAL ARRAY1(1:N,1:*),XMIN,XMAX,YMIN,YMAX,
     X        ZMIN,ZMAX,ECUT
      INTEGER IARRAY(1:N,1:*)
C=====================================================================
C    Local Declaration:
C    ==================
      REAL TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,ZDEL,
     X     XDIR,YDIR,ZDIR,TEMP,UMIN,UMAX,VMIN,VMAX,
     X     WX,WY,WZ,PLZMAX,ZMED,ZDIV
      REAL RLEVEL
      INTEGER COLRED,I,J,IETA,IPHI,XNUM,YNUM,SEGNUM
      INTEGER    ICOL(14), KCOL, COL, INTE, FIL, STYL
      CHARACTER*24 IMESS1
      CHARACTER*3  CC
C    Data Statements:
C    ================
      DATA COLRED/1/
C      DATA ICOL/9, 7,16,12,14,11,13,15,10, 8, 6,5,4,3/
      DATA ICOL/13,16,15, 4, 6, 9, 7,14,10,12,11,8,5,3/
      DATA IMESS1/' No data inside bounds'/
C=====================================================================
      ZMIN=0.
      CALL JIQDIL(RLEVEL)
C   DETERMINING ZMAX
      ZMAX=PLZMAX(NXMIN,NX,NYMIN,NY,ARRAY1,ARRAY1,ZMAX,N,0)
      IF(ZMAX.LE.0)THEN
        CALL PCTEXT(1,IMESS1)
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
      CALL PUOPEN
C  BUILDING BLOCKS
      DO 100 IPHI=NXMIN,NX
       J = NX - IPHI + NXMIN
       DO 100 IETA=NYMIN,NY
C  DEFINE BAR
      TXMIN=FLOAT(J)
      TXMAX=TXMIN+1.0
      TYMIN=FLOAT(IETA)
      TYMAX=TYMIN+1.
      TZMIN=0.
      IF(ARRAY1(J,IETA).LT.ECUT)GO TO 60
      TZMAX=ARRAY1(J,IETA)/(ZMAX*ZSCAL)
      IF (TZMAX.GT.0) THEN
        IF(IARRAY(J,IETA).GT.0.AND.IARRAY(J,IETA).LT.15)THEN
          KCOL=ICOL(IARRAY(J,IETA))
        ELSE
          KCOL=3  ! Green
        ENDIF
        CALL PXCOLITOC(KCOL,CC)
        IF(RLEVEL .EQ. -2.) THEN
          CALL PXCOLR(CC)
          CALL PLDBAR(6,TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,0)
        ELSE
          CALL PXCOLFILL(CC)
          CALL PLDBAR(3,TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,0)
        ENDIF
      ENDIF
   60 CONTINUE
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
  999 RETURN
      END
