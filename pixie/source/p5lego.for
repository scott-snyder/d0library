      SUBROUTINE P5LEGO(NX,XMIN,XMAX,NY,YMIN,YMAX,ECUT,ZMAX,TITLE,
     &                  XLAB,YLAB,ZLAB,ARRAY,IARRAY,NXMIN,NYMIN,
     &                  IXG,IYG,N,ZSCAL,IMARK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Lego plot of ARRAY and IARRAY
C-                         ( Make for ESAM Lego )
C-
C-   Inputs  :
C-   N      - Total number of x elements in ARRAY
C-   NX     - Number of x elements of ARRAY that will be displayed
C-   XMIN   - Minimum value for X
C-   XMAX   - Maximum value for X
C-   NY     - Number of y elements of ARRAY that will be displayed
C-   YMIN   - Minimum value for Y
C-   YMAX   - Maximum value for Y
C-   ZMIN   - Minimum value for Z
C-   ZMAX   - Maximum value for Z
C-           (if ZMAX is .LT. 0, prog. will calculate ZMAX from data)
C-   TITLE  - Plot title
C-   XLAB   - X label
C-   YLAB   - Y label
C-   ZLAB   - Z label
C-   ARRAY  - Array of data
C-   IARRAY - Objects ID
C-   IMARK - 0 if no marks, 1 if special marks (ex. JET center or missing PT)
C-
C-   Created  13-JUL-1992   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:ESUM.PARAMS/LIST'
C--- Argument Declarations:
C-
      CHARACTER*(*) XLAB,YLAB,ZLAB
      CHARACTER*(*) TITLE
      INTEGER NX,NY,NXMIN,NYMIN,N,IXG,IYG,IMARK
      REAL ZSCAL
      REAL ARRAY(1:N,1:*),XMIN,XMAX,YMIN,YMAX,
     &        ZMIN,ZMAX,ECUT
      INTEGER IARRAY(1:N,1:*)
C--- Local Declaration:
C-
      REAL TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,ZDEL,
     &     XDIR,YDIR,ZDIR,TEMP,UMIN,UMAX,VMIN,VMAX,
     &     WX,WY,WZ,PLZMAX,ZMED,ZDIV
      REAL TSTEP
      INTEGER I,J,IETA,IPHI,NLEGO,IDLEGO,NL,IDL2,IDL(3)
      CHARACTER*24 IMESS1
      CHARACTER*3  COL
C---    Data Statements:
      DATA IMESS1/' No data inside bounds'/
C=====================================================================
      ZMIN=0.
      CALL VZERO(IDL,3)
C---   DETERMINING ZMAX
      ZMAX=PLZMAX(NXMIN,NX,NYMIN,NY,ARRAY,ARRAY,ZMAX,N,0)
      IF(ZMAX.LE.0)THEN
        CALL PUMESS(IMESS1)
        GO TO 999
      ENDIF
C--- Set viewing parameters
      CALL PLSETV(NX,NY,UMIN,UMAX,VMIN,VMAX)
C---  MAKING GRID
      CALL PLGRID(NXMIN,NX,NYMIN,NY,IXG,IYG,XMIN,XMAX,
     &        YMIN,YMAX)
C--- DRAW Z-AXIS
      ZMED=(XMAX+ZMIN)/2.
      CALL PLZAXS(NXMIN,NYMIN,ZMAX,ZMIN,ZMED,ZSCAL)
      CALL PUOPEN
C-
C---  BUILDING BLOCKS
C-
      DO 100 IPHI=NXMIN,NX
        J = NX - IPHI + NXMIN
        DO 100 IETA=NYMIN,NY
C---  DEFINE BAR
          IF (IARRAY(J,IETA) .GT. 10000) THEN
            IDL(1) = MOD(IARRAY(J,IETA),100)
            IDL2   = MOD(IARRAY(J,IETA),10000)
            IDL(2) = INT(IDL2/100)
            IDL(3) = INT(IARRAY(J,IETA)/10000)
            TSTEP = .3
            NLEGO = 3
          ELSEIF (IARRAY(J,IETA) .GT. 100) THEN
            IDL(1) = MOD(IARRAY(J,IETA),100)
            IDL(2) = INT(IARRAY(J,IETA)/100)
            TSTEP = .5
            NLEGO = 2
          ELSE
            IDL(1) = IARRAY(J,IETA)
            TSTEP = 1.
            NLEGO = 1
          ENDIF
          DO NL = 1,NLEGO
            IF (NL .EQ. 1) THEN
              TXMIN=FLOAT(J)
              TYMIN=FLOAT(IETA)
            ELSE
              TXMIN=FLOAT(J) + TSTEP*(NL-1)
              TYMIN=FLOAT(IETA) + TSTEP*(NL-1)
            ENDIF
            TXMAX=TXMIN + TSTEP
            TYMAX=TYMIN + TSTEP
            TZMIN=0.
            IF(ARRAY(J,IETA) .LT. ECUT)   GO TO 60
            TZMAX=ARRAY(J,IETA)/(ZMAX*ZSCAL)
            IF (TZMAX.GT.0) THEN
              IDLEGO = IDL(NL)
              IF (IDLEGO .EQ. ID_JET)          THEN
                COL = 'CYA'
              ELSEIF (IDLEGO .EQ. ID_MUON)     THEN
                COL = 'GRE'
              ELSEIF (IDLEGO .EQ. ID_ELECTRON) THEN
                COL = 'RED'
              ELSEIF (IDLEGO .EQ. ID_PHOTON)   THEN
                COL = 'RED'
              ELSEIF (IDLEGO .EQ. ID_TAU)      THEN
                COL = 'CYA'
              ELSEIF (IDLEGO .EQ. ID_ETMISS)   THEN
                COL = 'MAG'
              ELSEIF (IDLEGO .EQ. ID_ETSUM) THEN
                COL = 'MAG'
              ENDIF
C---
              IF (IDLEGO .EQ. ID_PHOTON .OR.
     &          IDLEGO .EQ. ID_TAU    .OR.
     &          IDLEGO .EQ. ID_ETMISS)     THEN
                CALL PXCOLFILL('BLA')
                CALL PXCOLR(COL)
              ELSE
                CALL PXCOLFILL(COL)
                CALL PXCOLR(COL)
              ENDIF
              CALL PLDBAR(3,TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,0)
            ENDIF
          ENDDO
   60     CONTINUE
  100 CONTINUE
C-
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
C---   PRINTING AXIS
      CALL PLABEL(VMIN,XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,XLAB,YLAB,ZLAB,
     &       TYMAX,TXMAX,TZMAX,NX,NXMIN,NY,NYMIN,ZSCAL)
C--- PRINTING TITLE
C--- NOTE: THIS ROUTINE SETS VIEWING PARAMETES TO X-Y VIEW
      CALL PXTITL(TITLE)
C----------------------------------------------------------------------
  999 RETURN
      END
