      SUBROUTINE MUCELL(NMOD,NPL,NCL,HSHAPE,NSPAR,SPAR,XPAR,ROTM,
     +                VOFF,NBUF,IBUF)
C-------------------------------------------------------------------
C     S/R MUMODU returens geometry parameters for a cell in
C     a muon PDT module.    The parameters are obtained from
C     MGEO bank in STP bank structure.
C
C  Input:
C     NMOD      module number.
C     NPL       plane number.   (=0,1,2,3)
C     NCL       cell number.    (=0,1,2,........,24)
C     NBUF      maximum number of spacial words (IBUF) to be filled.
C    MGEO-bank  muon geometry constants.
C
C  Output:
C     HSHAPE    volume shape.          (character*4,   'BOX ')
C     NSPAR     number of parameters.  (=3)
C     SPAR      shape parameters.      (DX,DY,DZ,   half widthe etc.)
C     XPAR      coordinate of the center of volume in lab system.
C     ROTM      3x3 rotation matrix.
C     VOFF      vernier offset
C     NBUF      number of words(IBUF) filled in this routine.
C     IBUF      spacial parameters (not yet defined)
C  dh   12/87    add ibuf(1)=orientation
C     note:   If the requested volume dose not exits, output
C             parameters will be,
C                  HSHAPE='    '    (i.e. blank)
C                  NSPAR =0
C                (others)=10000.
C
C  S.Kunori,    07-JAN-87  DH 12/23/87
C   DH 2/88 FIX INTERPLANE DISTANCE
C   DH 5/88 KLUGE AS DAQ READING OUT CHAMBERS IN WRONG ORDER
C   DH 7/88 MORE KLUGES; GEOMETRY FILE HAS WRONG STAGGER
C   DH 8/88 add pad point
C   DH 9/88 REMOVE KLUGES; PLANES NOW COUNT DOWN 0,1,2
C   DH 11/88 for library; planes count up 0,1,2 so that agrees
C         with Monte Carlo
C   SK 2/89 PLANES NOW COUNT DOWN AGAIN.
C           GAP BETWEEN PLANE 1 AND 2 INCLUDED.
C   AT 8/91 modification for V3.2 MGEO
C           survey mode or no-survey mode
C   AT 10/91 Put rotation correction dR
C   AT 10/91 Correct shape parameter for Survey version
C   AT 10/91 Correct rotation matrix
C   AT 12/91 Get type of MGEO from IC(LMGEO+8)
C   AT 05/92 fix wire length
C   AT 10/92 fix for GEANT production
C            Definition of chamber length, SPAR(3), for MC generation
C            and reconstruction is external size of chamber which 
C            include whole dead region such as aliminum wall and and 
C            electronics. This assumption has been implemented in D0
C            GEANT.For real data analysis, it is wire length which is
C            the distance between inside edge of both end wall. 
C  AT  12/92 MODIFY CHAMBER LENGTH FOR MC VSN>=3
C--------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$LINKS:IZSTPO.LINK/LIST'
      INCLUDE 'D0$LINKS:IZSTPC.LINK/LIST'
      INCLUDE 'D0$LINKS:IZSMUO.LINK/LIST'
      INCLUDE 'D0$LINKS:IZMGEH.LINK/LIST'
C  varables in i/o argument...
      INTEGER NMOD,NPL,NCL,NSPAR,NPLL
      REAL    SPAR(3),XPAR(3),ROTM(3,3),VOFF
      CHARACTER*4 HSHAPE
      INTEGER NBUF,IBUF(*)
C  external variables...
      INTEGER GZMGEH,GZMGEO
C  local variables...
      INTEGER I,J,K,LH,LM
      REAL    XYZLOC(3),XYZLAB(3)
C  MGEO version number
      INTEGER VSN
C  Data type
      LOGICAL MC
      LOGICAL FIRST
      LOGICAL MUGEANT_FLG
C
      REAL DET
      INTEGER INDEX,NERROR
C
      REAL  MUINEFL
      DATA FIRST/.TRUE./
C
      IF ( FIRST ) THEN             ! data type identify
        FIRST = .FALSE.
        IF (LHEAD.NE.0) THEN
          MC = IQ(LHEAD+1).GT.999
        ELSE
          MC = MUGEANT_FLG(2)
        END IF
      END IF
C
C -- get pointer to MGEH bank...
      LH=GZMGEH(I)
C -- get pointer to MGEO bank...
      LM=GZMGEO(NMOD)
C -- check if the bank exist...
      IF(LM.EQ.0) THEN
C     -- MGEO bank dose not exist...
        GO TO 800
      ELSE
C     -- MGEO bank exists.   Now calculate parameters...
C
C     -- check if plane no and cell no are valid...
        IF(NPL.LT.0.OR.NPL.GE.IC(LM+10).OR.
     +      NCL.LT.0.OR.NCL.GE.IC(LM+11))   GO TO 800
C     -- store shape of volume and number of parameters...
C
        HSHAPE='BOX '
        NSPAR=3
C
C     -- store rotation matrix.
C
        K=0
        DO 100 I=1,3
          DO 100 J=1,3
            K = K + 1
            ROTM(J,I) = C(LM+K+22) + C(LM+K+49)
  100   CONTINUE
C
C
C      get version number of each MGEO STP
C                       0      - no survey mode
C                       Others - survey mode
        VSN = IC(LM+8)
C
C     -- shape parameters...
C        half width of cell volume is defined as half length
C        of wire spacing minus half thickness of alminum wall.
C
        SPAR(1)=(C(LM+33)-C(LH+11))/2.
        SPAR(2)=(C(LM+32)-C(LH+11))/2.
        IF ( MC ) THEN
          IF ( IC(LM+1).GE.3 ) THEN
            SPAR(3)=C(LM+14)/2.0               ! MC with survey geo 
          ELSE
            SPAR(3)=C(LM+34)/2.0               ! no survey version
          END IF
        ELSE 
          IF (VSN.NE.0) THEN
            SPAR(3)=C(LM+14)/2.0             ! survey version
          ELSE
            SPAR(3)=(C(LM+34)-MUINEFL())/2.0 ! real data with MC geometry
          END IF
        END IF 
C
C      STORE ORIENTATION
C
        IBUF(1)=IC(LM+12)
C
C     -- calculate the center of cell...
C

        XYZLOC(1)=C(LM+37)-C(LM+33)*FLOAT(NPL)
C        -- gap between plane 1 and 2...
        IF(NPL.GE.2) THEN
          XYZLOC(1)=XYZLOC(1)-C(LM+42)
        ENDIF
        XYZLOC(2)=C(LM+NPL+38)+C(LM+32)*FLOAT(NCL)
        IF ( VSN.EQ.0 ) THEN                           ! for no survey mode
          XYZLOC(3)=C(LM+34)/2.
        ELSE                                           ! for survey mode
          XYZLOC(3) = 0.0
        END IF
        VOFF=C(LM+43+NPL)        ! PAD OFFSET
C     -- rotate back to lab system.   (s/r vmatr from kernlib)
        CALL VMATR(XYZLOC,ROTM,XYZLAB,3,3)
C     -- center of module at lab system...
        XPAR(1)=C(LM+20)+C(LM+47)+XYZLAB(1)
        XPAR(2)=C(LM+21)+C(LM+48)+XYZLAB(2)
        XPAR(3)=C(LM+22)+C(LM+49)+XYZLAB(3)

      END IF
C
      GO TO 900
C
C  -- module/plane/cell dose not exist...
  800 CONTINUE
      HSHAPE='    '
      NSPAR=0
      CALL VFILL(SPAR,3,10000.)
      CALL VFILL(XPAR,3,10000.)
      CALL VFILL(ROTM,9,10000.)
      IBUF(1)=0
C
  900 CONTINUE
      RETURN
      END
