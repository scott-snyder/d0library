C======================================================================
      SUBROUTINE PMDDETC(IVIEW,IFLAG)
C======================================================================
C
C  Description:
C  ============
C  Draws the Muon Detector in view IVIEW
C
C-   Inputs  : 
C-             IVIEW - 1 for Y-Z
C-                     2 for X-Y CENTRAL
C-                     3 for Z-X
C-             IFLAG - 0 all layers
C-             IFLAG - 1 A layer only
C-             IFLAG - 2 Y-Z cen only  
C-   Outputs : 
C-   Controls: 
C-
C  Author:
C  =======
C  Sharon Hagopian - Based on PMDDET
C
C  Revision History:
C  =================
C  Original Creation - June 14, 1990
C=====================================================================
C
      IMPLICIT NONE
      INTEGER IVIEW                     ! IVIEW - the present view
      INTEGER IFLAG 
C
C  Local Declarations:
C  ===================
C
      INTEGER M,NMOD,IMOD               ! M - LOOP VARIABLE
      INTEGER I,J
      INTEGER NMURAW,NMUPROC,NHMOD,NHPL3
      INTEGER LPMUOF(310),NRAW,IMUD1,NPROC,IMUOH,NHPL0,NHPL1,NHPL2
      REAL SPAR(3),XPAR(3),ROTM(3,3)
      REAL SPARM(3)
      INTEGER NBUF,IBUF,NSPAR,JVIEW,ICHNUM
      REAL MXC,MYC,MZC
      REAL MXD,MYD,MZD
      REAL XWC,YWC,ZWC
      REAL MODXC,MODYC,MODZC
      REAL MODXD,MODYD,MODZD
      REAL MODA(10,3),MODB(20,3),MODC(31,3)
      CHARACTER*4 HSHAPE
C--------------------------------------------------------------------
      DATA MODA/11,21,31,16,26,36,91,97,61,67,
     X         30,31,32,33,35,36,4*0, 
     X         10,20,30,13,23,33,91,92,61,62/
      DATA MODB/101,111,121,131,141,106,116,136,146,191,180,196,
     X          161,150,166,
     x           190,197,160,167,0,  ! END VIEW 1
     X  140,141,142,143,144,135,136,147,145,146,130,137,131,132,
     X  133,134,4*0,         ! END VIEW 2
     X  100,110,120,130,140,103,113,123,133,143,191,192,161,162,
     X  190,193,160,163,2*0/
      DATA MODC/201,211,221,231,241,206,216,236,246,
     X  300,307,301,306,290,297,280,291,296,
     X  281,285,251,255,
     X  270,277,276,271,250,266,261,260,267,   ! END VIEW 1
     X  240,241,242,243,244,245,246,247,23*0, !END VIEW 2
     X  200,210,220,230,240,203,213,223,233,243,301,302,250,253,
     X  300,303,290,291,292,293,270,271,272,273,260,261,262,263,
     X  280,283,0/

C-------------------------------------------------------------------
        CALL PUOPEN
C
C
C  Draw muon modules in  A-LAYER
C  ==========================================================
          DO 10 IMOD=1,10
          IF(IFLAG.EQ.2.AND.IMOD.GT.6)GO TO 10
          NMOD=MODA(IMOD,IVIEW)
      IF(NMOD.EQ.0)GO TO 10
          CALL MUMODU(NMOD,HSHAPE,NSPAR,SPAR,XPAR,ROTM,
     X                        NBUF,IBUF)
          IF ((NSPAR .NE. 0) .AND. (HSHAPE .NE. '    ')) THEN
            DO 25 I = 1,3
              SPARM(I) = 0.
              DO 35 J = 1,3
                SPARM(I) = SPARM(I) + SPAR(J)*ROTM(I,J)
   35         CONTINUE
   25       CONTINUE
            MODXC = (XPAR(1))
            MODYC = (XPAR(2))
            MODZC = (XPAR(3))
            MODXD = ABS(SPARM(1))
            MODYD = ABS(SPARM(2))
            MODZD = ABS(SPARM(3))
              IF (IVIEW.EQ.1) CALL PXBOX(MODZC,MODYC,
     X                     MODXC,MODZD,MODYD,MODXD)
              IF (IVIEW.EQ.2) CALL PXBOX(MODXC,MODYC,
     X                     MODZC,MODXD,MODYD,MODZD)
              IF (IVIEW.EQ.3) CALL PXBOX(MODZC,MODXC,
     X                     MODYC,MODZD,MODXD,MODYD)
          ENDIF
C
   10   CONTINUE
C IF IFLAG=1, A-Layer only
      IF(IFLAG.EQ.1)GO TO 999
C  Draw muon modules in  B-LAYER
C  ==========================================================
          DO 50 IMOD=1,20
          NMOD=MODB(IMOD,IVIEW)
      IF(NMOD.EQ.0)GO TO 50
C FOR IFLAG=2, DO NOT DRAW END CAPS
      IF(IFLAG.EQ.2.AND.IVIEW.EQ.1.AND. IMOD.GT.9)GO TO 50
      IF(IFLAG.EQ.2.AND.IVIEW.EQ.3.AND. IMOD.GT.10)GO TO 50
          CALL MUMODU(NMOD,HSHAPE,NSPAR,SPAR,XPAR,ROTM,
     X                        NBUF,IBUF)
          IF ((NSPAR .NE. 0) .AND. (HSHAPE .NE. '    ')) THEN
            DO 45 I = 1,3
              SPARM(I) = 0.
              DO 40 J = 1,3
                SPARM(I) = SPARM(I) + SPAR(J)*ROTM(I,J)
   40         CONTINUE
   45       CONTINUE
            MODXC = (XPAR(1))
            MODYC = (XPAR(2))
            MODZC = (XPAR(3))
            MODXD = ABS(SPARM(1))
            MODYD = ABS(SPARM(2))
            MODZD = ABS(SPARM(3))
              IF (IVIEW.EQ.1) CALL PXBOX(MODZC,MODYC,
     X                     MODXC,MODZD,MODYD,MODXD)
              IF (IVIEW.EQ.2) CALL PXBOX(MODXC,MODYC,
     X                     MODZC,MODXD,MODYD,MODZD)
              IF (IVIEW.EQ.3) CALL PXBOX(MODZC,MODXC,
     X                     MODYC,MODZD,MODXD,MODYD)
          ENDIF
C
   50   CONTINUE
C  Draw muon modules in  C-LAYER
C  ==========================================================
          DO 80 IMOD=1,31
          NMOD=MODC(IMOD,IVIEW)
      IF(NMOD.EQ.0)GO TO 80
      IF(IFLAG.EQ.2.AND.NMOD.GT.249)GO TO 80
          CALL MUMODU(NMOD,HSHAPE,NSPAR,SPAR,XPAR,ROTM,
     X                        NBUF,IBUF)
          IF ((NSPAR .NE. 0) .AND. (HSHAPE .NE. '    ')) THEN
            DO 75 I = 1,3
              SPARM(I) = 0.
              DO 60 J = 1,3
                SPARM(I) = SPARM(I) + SPAR(J)*ROTM(I,J)
   60         CONTINUE
   75       CONTINUE
            MODXC = (XPAR(1))
            MODYC = (XPAR(2))
            MODZC = (XPAR(3))
            MODXD = ABS(SPARM(1))
            MODYD = ABS(SPARM(2))
            MODZD = ABS(SPARM(3))
              IF (IVIEW.EQ.1) CALL PXBOX(MODZC,MODYC,
     X                     MODXC,MODZD,MODYD,MODXD)
              IF (IVIEW.EQ.2) CALL PXBOX(MODXC,MODYC,
     X                     MODZC,MODXD,MODYD,MODZD)
              IF (IVIEW.EQ.3) CALL PXBOX(MODZC,MODXC,
     X                     MODYC,MODZD,MODXD,MODYD)
          ENDIF
C
   80   CONTINUE
C
  999   CONTINUE
        CALL JRCLOS        
      RETURN
      END
