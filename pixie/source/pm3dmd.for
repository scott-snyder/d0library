      SUBROUTINE PM3DMD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw Muon System Modules in 3D View.
C-
C-   Inputs  : ISELC [I] - 1: CEN,  2: END(S)  and 3: END(N)
C-   Controls:
C-
C-   Updated  13-AUG-1992   Nobuaki Oshima(Check picking is active or not)
C-   Created  30-JUN-1992   Nobuaki Oshima(Ref. Mike Shupe's SGI routine)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER ISELC,IND
      INTEGER JVIEW,NMODS,NMDCEN,NMDENP,NMDENM
      INTEGER NMD,NMOD,NSPAR,NBUF,IBUF,I,J
      INTEGER IMOD,IRAW,IPROC,IHTMOD(307)
      INTEGER LMUOF,GZMUOF,NDATA, DRWHITS,NUMDO
      REAL    SPAR(3),XPAR(3),ROTM(3,3),SPARM(3)
      REAL    MODXC,MODYC,MODZC
      REAL    MODXD,MODYD,MODZD
      LOGICAL PU_PICK_ACTIVE
      CHARACTER*4 HSHAPE
C---     CENTRAL MODULE LIST
      INTEGER MODCEN(98)
      DATA NMDCEN /98/
      DATA MODCEN/
     &  10,11,12,13,15,16, 20,21,22,23,25,26, 30,31,32,33,35,36,
     &  100,101,102,103,104,105,106,107,
     &  110,111,112,113,114,115,116,117,
     &  120,121,122,123,124,125,126,127,
     &  130,131,132,133,134,135,136,137,
     &  140,141,142,143,144,145,146,147,
     &  200,201,202,203,204,205,206,207,
     &  210,211,212,213,214,215,216,217,
     &  220,221,222,223,224,225,226,227,
     &  230,231,232,233,234,235,236,237,
     &  240,241,242,243,244,245,246,247/
C---     END (SOUTH) MODULE LIST
      INTEGER MODENP(35)
      DATA NMDENP/35/
      DATA MODENP/
     &  91,92,94,95,97,180,183,190,191,192,193,194,195,196,197,
     &  280,281,283,285,290,291,292,293,294,295,296,297,
     &  300,301,302,303,304,305,306,307/
C---     END (NORTH) MODULE LIST
      INTEGER MODENM(35)
      DATA NMDENM/35/
      DATA MODENM/
     &  61,62,64,65,67,150,153,160,161,162,163,164,165,166,167,
     &  250,251,253,255,260,261,262,263,264,265,266,267,
     &  270,271,272,273,274,275,276,277/
C
C----------------------------------------------------------------------
C-
C--- Check do picking...
C-
      IF ( PU_PICK_ACTIVE() ) THEN
        GO TO 999
      ENDIF
C---
      CALL PUGETV('MUON DRAW HITS',DRWHITS)
      CALL PUGETV('MUON DRAW 3D MODULE',ISELC)
      IF(ISELC.EQ.0)GO TO 999
C-
      IF (DRWHITS .LT. 1)         GO TO 999
      NUMDO = 0
C-
C--- ISELC = 4 MEANS DRAW ALL(CEN+ENS+ENN) WAMUS MODULES
C-
      IF (ISELC .EQ. 1) THEN      ! CENTRAL
        NUMDO = 1
        JVIEW = 1
      ELSEIF (ISELC .EQ. 2) THEN  ! END( SOUTH )
        NUMDO = 1
        JVIEW = 2
      ELSEIF (ISELC .EQ. 3) THEN  ! END( NORTH )
        NUMDO = 1
        JVIEW = 3
      ELSEIF (ISELC .EQ. 4) THEN  ! ALL MODULES
        NUMDO = 3
      ENDIF
C-
C--- Fill Module List which has hits
C-
      CALL VZERO(IHTMOD,307)
      LMUOF = GZMUOF(0)
      NDATA = IQ(LMUOF-1)
      NDATA = NDATA/10
      DO I = 1,NDATA
        IMOD = IQ(LMUOF+1+10*(I-1))
        IRAW = IQ(LMUOF+2+10*(I-1))
        IPROC= IQ(LMUOF+4+10*(I-1))
        IHTMOD(IMOD) = IPROC
      ENDDO
C-
      CALL PUOPEN
      CALL PXCOLR('FOR')
      DO IND = 1,NUMDO
        IF (NUMDO .EQ. 3) THEN
          JVIEW = IND
        ENDIF
        IF(JVIEW.EQ.1) THEN
          NMODS=NMDCEN
        ELSEIF(JVIEW.EQ.2) THEN
          NMODS=NMDENP
        ELSE
          NMODS=NMDENM
        ENDIF
C-
        DO 100 NMD=1,NMODS
          IF(JVIEW.EQ.1) THEN
            NMOD=MODCEN(NMD)
          ELSEIF(JVIEW.EQ.2) THEN
            NMOD=MODENP(NMD)
          ELSEIF(JVIEW.EQ.3) THEN
            NMOD=MODENM(NMD)
          ENDIF
C-
C--- DRAW MODULES WHICH HAVE HITS( DRWHITS=1 ) OR ALL( DRWHITS>1 )
C-
          IF(DRWHITS .EQ. 1)   THEN
            IF(IHTMOD(NMOD) .LE. 1) GO TO 100
          ENDIF
          CALL MUMODU(NMOD,HSHAPE,NSPAR,SPAR,XPAR,ROTM,NBUF,IBUF)
          IF ((NSPAR .NE. 0) .AND. (HSHAPE .NE. '    ')) THEN
            DO I = 1,3
              SPARM(I) = 0.
              DO J = 1,3
                SPARM(I) = SPARM(I) + SPAR(J)*ROTM(I,J)
              ENDDO
            ENDDO
            MODXC = (XPAR(1))
            MODYC = (XPAR(2))
            MODZC = (XPAR(3))
            MODXD = ABS(SPARM(1))
            MODYD = ABS(SPARM(2))
            MODZD = ABS(SPARM(3))
            CALL PXBOX(MODXC,MODYC,MODZC,MODXD,MODYD,MODZD)
          ENDIF
  100   CONTINUE
      ENDDO
      CALL PUCLOSE
  999 RETURN
      END
