      SUBROUTINE P4LEGO(NX,XMIN,XMAX,NY,YMIN,YMAX,ETCUT,ZMAX,TITLE,
     X      XLAB,YLAB,ZLAB,ARRAY1,IARRAY,XPMUO,NXMIN,NYMIN,
     X      IXG,IYG,N,ZSCAL,NMARK)
C=====================================================================
C
C   Description:
C   ===========
C          Lego plot OF ARRAY1 (FAST PLOT; front and top of bins only)
C   INPUT:
C   =====
C       N      - Total number of x elements in ARRAY1
C       NX     - Number of x elements of ARRAY1 that will be displayed
C       XMIN   - Minimum value for X
C       XMAX   - Maximum value for X
C       NY     - Number of y elements of ARRAY1 that will be displayed
C       YMIN   - Minimum value for Y
C       YMAX   - Maximum value for Y
C       ETCUT  - Minimum Et for value to be plotted
C       ZMIN   - Minimum value for Z
C       ZMAX   - Maximum value for Z
C                (if ZMAX is .LT. 0, prog. will calculate ZMAX from data)
C       TITLE  - Plot title
C       XLAB   - X label
C       YLAB   - Y label
C       ZLAB   - Z label
C       COL1   - Color of bins
C       ARRAY1 - Array of data
C       IARRAY - Array of code for data
C       XPMUO  - Array of PMUO
C       NMARK (1)= number of Miss Et bins (0 or 1)
C       NMARK (2)= number of Electrons
C       NMARK (3)= number of Photons
C       NMARK (4)= number of TAUS
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
C-     21-nov-1991 jf Det÷uf IARRAY.ge.20 for trak_marks
C-   Updated  14-JAN-1992   J.f. Det÷uf (jet_size coded in array1
C-                        ratio EM/ET coded in iarray/100 )
C-   Updated  24-FEB-1992 S. Hagopian, changed to arrows for CD tracks
C-                       changed legend and colors
C-              changed to marks for Missing Et, electrons and photons
C-   Modified  4-FEB-1994 Nobuaki Oshima
C-              Added new array XPMUO for muon and cleaned up.
C-              Determine ZMAX by all objects but jets only.
C-
C
C=====================================================================
      IMPLICIT NONE
C=====================================================================
C    Argument Declarations:
C    ======================
      CHARACTER*(*) XLAB,YLAB,ZLAB
      CHARACTER*(*) TITLE
      INTEGER NX,NY,COL1,COL2,NXMIN,NYMIN,N,IXG,IYG
      INTEGER NMARK(4)
      REAL ZSCAL,XPMUO(3,50)
      REAL ARRAY1(1:NX,1:NY),XMIN,XMAX,YMIN,YMAX,
     X        ZMIN,ZMAX,ETCUT
      INTEGER IARRAY(1:N,1:*)
C=====================================================================
C    Local Declaration:
C    ==================
      INTEGER IMARK,NMISS,NELEC,NPHOTON,NTAUS
      INTEGER COLRED,I,J,K,IETA,IPHI,XNUM,YNUM,SEGNUM
      INTEGER IRAT ,ICODE,ITRA        !!jfd   em/et,code,track
      INTEGER ICOL(14), KCOL, COL, INTE, FIL, STYL,  IC  !!m
      INTEGER ISIG,INDP,INDE
      REAL RAD
      REAL TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,ZDEL,
     X     XDIR,YDIR,ZDIR,TEMP,UMIN,UMAX,VMIN,VMAX,
     X     WX,WY,WZ,PLZMAX,ZMED,ZDIV
      REAL TXMID,TYMID,TXARROW,TYARROW,TZARROW
      REAL TZMINE,TZMAXE, ZMAXP
      REAL RLEVEL
      REAL XSIZ,YSIZ
      REAL TXDEL,TYDEL,TZDEL
      CHARACTER*24 IMESS1
      CHARACTER*3  CC
C    Data Statements:
C    ================
      DATA TZDEL/.7/
      DATA XSIZ,YSIZ /1.0,.67/
      DATA COLRED/1/
      DATA ICOL/9,7,16,12,7,11,13,15,10,8,6,5,4,3/
C see SUBROUTINE PXCOLR for correspondence between ICOL and color name
      DATA IMESS1/' No objs inside bounds'/
C=====================================================================
      ZMIN=0.
      CALL JIQDIL(RLEVEL)
C--- DETERMINING ZMAX
      ZMAX=PLZMAX(NXMIN,NX,NYMIN,NY,ARRAY1,ARRAY1,ZMAX,N,0)
      CALL PUGET_MAXET(ZMAXP)
      IF (ZMAXP .GT. ZMAX)   ZMAX = ZMAXP
      IF(ZMAX .LE. 0.)THEN
        CALL PUMESS(IMESS1)
        GO TO 999
      ENDIF
C--- Set viewing parameters
      IF(RLEVEL .NE. -2.) THEN
        CALL PLSETV(NX,NY,UMIN,UMAX,VMIN,VMAX)
      ENDIF
C--- MAKING GRID
      CALL PLGRID(NXMIN,NX,NYMIN,NY,IXG,IYG,XMIN,XMAX,
     X        YMIN,YMAX)
C--- DRAW Z-AXIS
      ZMED=(XMAX+ZMIN)/2.
      CALL PLZAXS(NXMIN,NYMIN,ZMAX,ZMIN,ZMED,ZSCAL)
      CALL PUOPEN
C-
C--- BUILDING BLOCKS
C-
      DO 100 IPHI=NXMIN,NX
        J = NX - IPHI + NXMIN
        TXMIN=FLOAT(J)
        TXMAX=TXMIN+1.0
        DO 100 IETA=NYMIN,NY
          CALL JPINTR(1)
          CALL PXCOLR('FOR') ! DEFAULT COLOR
          TYMIN=FLOAT(IETA)
          TYMAX=TYMIN+1.
          TZMIN=0.
          TZMAX=0.
          IF (IARRAY(J,IETA).LE.0) GO TO 100
          ISIG=IARRAY(J,IETA)/10000
          IRAT=(IARRAY(J,IETA) - 10000*ISIG)/100
          ICODE = IARRAY(J,IETA)-100*IRAT -10000*ISIG
          ITRA= ICODE/20
          IC= ICODE-20*ITRA
C--- code=4 FOR JET
          IF (ICODE.EQ.4)THEN
C--- DRAW JET SIZE
            CALL PXCOLR('FOR')
            CALL JPINTR(0)
            RAD=ISIG
            RAD=RAD/10.
            CALL JCIRCL(TXMIN+.5,TYMIN+.5,TZMIN,RAD,0)
          ENDIF
C--- DEFINE BAR
          IF (ARRAY1(J,IETA) .LT. ETCUT) GO TO 60
C-
          TZMAX= ARRAY1(J,IETA)/(ZMAX*ZSCAL)
          CALL PXCOLFILL('CYA')
          IF(IRAT.GT.0)THEN    !     em_part
            TZMAXE= TZMAX*FLOAT(IRAT)/100.
            CALL PXCOLFILL('RED')
            CALL PLDBAR(3,TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAXE,0)
            TZMIN= TZMAXE
          ENDIF
C-
C--- Draw Jets
          IF (IC.EQ.4) THEN  
            CALL PXCOLFILL('CYA')
          ENDIF
C-
          CALL PLDBAR(3,TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,0)
   60     CONTINUE
  100 CONTINUE
C-
C--- Draw Muon with checking overlapped PMUO to JETS in (IPHI,IETA)
C-
      K = 0
  120 K = K + 1
      IF (XPMUO(1,K).EQ.0. .OR. K.GT.50) GO TO 150
      IF (XPMUO(3,K) .LT. ETCUT) GO TO 120
      INDP = XPMUO(1,K)
      INDE = XPMUO(2,K)
      TXMIN = XPMUO(1,K)
      TXMAX = TXMIN + 1.
      TYMIN = XPMUO(2,K)
      TYMAX = TYMIN + 1.
      IF (ARRAY1(INDP,INDE) .GT. 0.) THEN
        TXMAX = TXMAX - .5
        TYMIN = TYMIN + .5
      ENDIF
      TZMIN = 0.
      TZMAX = XPMUO(3,K)/(ZMAX*ZSCAL)
      CALL PXCOLFILL('GRE')
      CALL JSIZE(XSIZ,YSIZ)
      TXDEL=(TXMAX-TXMIN)/2.
      TYDEL=(TYMAX-TYMIN)/2.
      CALL J3MOVE(TXMIN+TXDEL,TYMIN+TYDEL,TZMAX+TZDEL)
      CALL JJUST(2,2)
      CALL J1STRG('MUON')
C--- Draw vertical line topoint to bin
      CALL J3MOVE(TXMIN+TXDEL,TYMIN+TYDEL,TZMAX+(TZDEL/2.))
      CALL J3DRAW(TXMIN+TXDEL,TYMIN+TYDEL,TZMAX)
      CALL PLDBAR(3,TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,0)
      GO TO 120
  150 CONTINUE
C-
C--- SPECIAL MARKS FOR TRAKS   !Tracks overwrite
C-
      DO 200 IPHI=NXMIN,NX
        J = NX - IPHI + NXMIN
        DO 200 IETA=NYMIN,NY
          IRAT=IARRAY(J,IETA)/100
          ICODE = IARRAY(J,IETA)-100*IRAT
          ITRA= ICODE/20
          IF (ITRA.NE.0) THEN   !!jfd  tracks
            TXMIN=FLOAT(J)
            TXMAX=TXMIN+1.
            TYMIN=FLOAT(IETA)
            TYMAX=TYMIN+1.
            TZMIN=0.
            TZMAX=1.1/ZSCAL
            TXMID=TXMIN +(TXMAX-TXMIN)/2.
            TYMID=TYMIN +(TYMAX-TYMIN)/2.
            CALL PXCOLR('FOR')
            IF (ITRA.EQ.1.OR.ITRA.EQ.2) THEN   !!CD TRACKS
              CALL J3MOVE(TXMID,TYMID,TZMIN)
              CALL J3DRAW(TXMID,TYMID,TZMAX)
C--- DRAW Arrow on top of track line
              TXARROW=(TXMAX-TXMIN)/4.
              TYARROW=(TYMAX-TYMIN)/4.
              TZARROW=.9*TZMAX
              CALL J3MOVE(TXMID,TYMID,TZMAX)
              CALL J3DRAW(TXMID+TXARROW,TYMID+TYARROW,TZARROW)
              CALL J3MOVE(TXMID,TYMID,TZMAX)
              CALL J3DRAW(TXMID-TXARROW,TYMID-TYARROW,TZARROW)
            ELSE
              TZMAX=1.2/ZSCAL
              CALL J3MOVE(TXMID,TYMID,TZMIN)
              CALL J3DRAW(TXMID,TYMID,TZMAX)
C--- DRAW "M" on top of track line
              TXARROW=(TXMAX-TXMIN)/2.
              TYARROW=(TYMAX-TYMIN)/2.
              TZARROW=1.05*TZMAX
              CALL J3MOVE(TXMID,TYMID,TZMAX)
              CALL J3DRAW(TXMID-2*TXARROW,TYMID-TYARROW,TZARROW)
              CALL J3DRAW(TXMID-2*TXARROW,TYMID-TYARROW,TZMAX)
              CALL J3MOVE(TXMID,TYMID,TZMAX)
              CALL J3DRAW(TXMID+TXARROW,TYMID+TYARROW,TZARROW)
              CALL J3DRAW(TXMID+TXARROW,TYMID+TYARROW,TZMAX)
            ENDIF
          ENDIF
  200 CONTINUE
      ZDIV=ZMAX*ZSCAL
      TXMIN=0.
      TXMAX=XMAX*10.+1.16815
      TYMIN=0.
      TYMAX=YMAX*20.+1.
C--- MARK ELECTRONS
      IMARK=2
      CALL PLEGO_BIN(IMARK,TXMIN,TXMAX,TYMIN,TYMAX,ZDIV,ETCUT,NELEC)
      NMARK(2)=NELEC
C--- MARK PHOTONS
      IMARK=3
      CALL PLEGO_BIN(IMARK,TXMIN,TXMAX,TYMIN,TYMAX,ZDIV,ETCUT,NPHOTON)
      NMARK(3)=NPHOTON
C--- MARK TAUS
      IMARK=4
      CALL PLEGO_BIN(IMARK,TXMIN,TXMAX,TYMIN,TYMAX,ZDIV,ETCUT,NTAUS)
      NMARK(4)=NTAUS
C--- MARK MISSING ET
      IMARK=1
      CALL PLEGO_BIN(IMARK,TXMIN,TXMAX,TYMIN,TYMAX,ZDIV,ETCUT,NMISS)
      NMARK(1)=NMISS
      CALL JRCLOS
C--- PRINTING AXIS
      CALL PLABEL(VMIN,XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,XLAB,YLAB,ZLAB,
     X       TYMAX,TXMAX,TZMAX,NX,NXMIN,NY,NYMIN,ZSCAL)
C--- PRINTING TITLE(NOTE: PXTITL SETS VIEWING PARAMETES TO X-Y VIEW)
      CALL PXTITL(TITLE)
C-
  999 RETURN
      END
