C=====================================================================
       SUBROUTINE PLEGO(NX,XMIN,XMAX,NY,YMIN,YMAX,ECUT,ZMAX,TITLE,
     X       XLAB,YLAB,ZLAB,COL1,COL2,ARRAY1,ARRAY2,NXMIN,NYMIN,
     X       IXG,IYG,N,ZSCAL)
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
C       COL2   - Color for second set of bins (if = 0 no second array)
C       ARRAY1 - Array of data
C       ARRAY2 - Array of data
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
C      Jan  2, 1990 - Updated Lupe Rosas Implementing Color Table
C
C=====================================================================
      IMPLICIT NONE
C=====================================================================
C    Argument Declarations:
C    ======================
      CHARACTER*24 XLAB,YLAB,ZLAB
      CHARACTER*20 TITLE
      CHARACTER*(*) COL1, COL2
      INTEGER NX,NY,NXMIN,NYMIN,N,IXG,IYG
      REAL ZSCAL
      REAL ARRAY1(1:N,1:*),ARRAY2(1:N,1:*),XMIN,XMAX,YMIN,YMAX,
     X        ZMIN,ZMAX,ECUT
C=====================================================================
C    Local Declaration:
C    ==================
      REAL TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,ZDEL,
     X     XDIR,YDIR,ZDIR,TEMP,UMIN,UMAX,VMIN,VMAX,
     X     WX,WY,WZ,XSIZE,YSIZE,PLZMAX,ZMED
      INTEGER COLRED,I,J,IETA,IPHI,XNUM,YNUM,SEGNUM,LIST(1)
      CHARACTER*30 IMESS1
C    Data Statements:
C    ================
      DATA COLRED/1/
      DATA IMESS1/' No data inside given bounds'/
C=====================================================================
      CALL JFRAME
      ZMIN=0.      
      XSIZE=(XMAX-XMIN)/FLOAT(NX-NXMIN+1)
      YSIZE=(YMAX-YMIN)/FLOAT(NY-NYMIN+1)
      XDIR=15.
      YDIR=-15.
      ZDIR=-3.
C   DETERMINING ZMAX
      ZMAX=PLZMAX(NXMIN,NX,NYMIN,NY,ARRAY1,ARRAY2,ZMAX,N,2)
      IF(ZMAX.LE.0)THEN
        CALL PUMESS(IMESS1)
        GO TO 999
      ENDIF
C  DETERMINE THE WINDOW SIZE
      VMIN=ZDEL-YSIZE*NY*.12-XSIZE*NX*.12
      VMAX=NX*XSIZE*.15
      UMIN=NXMIN-YSIZE*NY*.8-XSIZE*NX*.9
      UMAX=NXMIN+1.7
C  CALLING WINDOW
      CALL JWINDO(UMIN,UMAX,VMIN,VMAX)
C  PRINTING TITLE
      CALL PLTITL(TITLE,VMIN,UMIN,UMAX)
C  CALCULATING THE DIRECTION
      CALL JVUPNT(0.,0.,0.)
      CALL JNORML(XDIR,YDIR,ZDIR)
      CALL JUPVEC(0.,0.,1.)
      CALL JHITHR(-200.)
      CALL JYON(200.)
      CALL JRIGHT(.TRUE.)
      CALL JHCLIP(.TRUE.)
      CALL JYCLIP(.TRUE.)
      CALL JWCLIP(.TRUE.)
      CALL JPARAL
C  MAKING GRID
       CALL PLGRID(NXMIN,NX,NYMIN,NY,IXG,IYG,XMIN,XMAX,
     X        YMIN,YMAX)
C DRAW Z-AXIS
      ZMED=(XMAX+ZMIN)/2.
      CALL PLZAXS(NXMIN,NYMIN,ZMAX,ZMIN,ZMED,ZSCAL)
C  CHECKING TYPE OF DEVICE to see if it is black and white or color
      CALL JIQDEV(1,1,LIST)
C  BUILDING BLOCKS
      DO 100 IETA=NXMIN,NX
       J = NX - IETA + NXMIN
       DO 100 IPHI=NYMIN,NY
C  DEFINE BAR
      TXMIN=XSIZE*FLOAT(J-NXMIN)+FLOAT(NXMIN)
      TXMAX=TXMIN+1.0*XSIZE
      TYMIN=YSIZE*FLOAT(IPHI-NYMIN)+FLOAT(NYMIN)
      TYMAX=TYMIN+YSIZE
      TZMIN=0.
      IF (COL2.EQ.'   ')THEN
        IF(ARRAY1(J,IPHI).LT.ECUT)GO TO 60
      ELSE
        IF((ARRAY1(J,IPHI)+ARRAY2(J,IPHI)).LT.ECUT)GO TO 60
      ENDIF
      TZMAX=ARRAY1(J,IPHI)/(ZMAX*ZSCAL)
      IF (TZMAX.GT.0) THEN
        CALL PXCOLFILL(COL1)
        CALL PLDBAR(6,TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,1)
        IF (COL2.NE.'   ') THEN
          IF (ARRAY2(J,IPHI).GT.0) THEN
            TZMIN=TZMAX
            TZMAX=ARRAY2(J,IPHI)/(ZMAX*ZSCAL) + TZMAX
            CALL PXCOLFILL(COL2)
            CALL PLDBAR(6,TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,2)
          ENDIF
        ENDIF
        CALL JRCLOS
      ELSEIF (COL2.NE.'   ') THEN
             IF (ARRAY2(J,IPHI).GT.0) THEN
               TZMIN=0.
               TZMAX=ARRAY2(J,IPHI)/(ZMAX*ZSCAL)
               CALL PXCOLFILL(COL2)
               CALL PLDBAR(6,TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,2)
               CALL JRCLOS
             ENDIF
      ENDIF
   60 CONTINUE
  100 CONTINUE
C   PRINTING AXIS
      CALL PLABEL(VMIN,XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,XLAB,YLAB,ZLAB,
     X       TYMAX,TXMAX,TZMAX,NX,NXMIN,NY,NYMIN,ZSCAL)
  999 RETURN
      END
