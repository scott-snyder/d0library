      SUBROUTINE MUMODU(NMOD,HSHAPE,NSPAR,SPAR,XPAR,ROTM
     +                 ,NBUF,IBUF)
C-------------------------------------------------------------------
C     S/R MUMODU returens geometry parameters for a muon PDT
C  module.
C
C  Input:
C     NMOD      module number.     
C     NBUF      maximum no. of spacial parameters to be returned.
C               If 0, no word will be returned.
C
C  Output:
C     HSHAPE    volume shape.          (character*4,   'BOX ')
C     NSPAR     number of parameters.  (=3)
C     SPAR      shape parameters.      (DX,DY,DZ,   half widthe etc.)
C     XPAR      coordinate of the center of volume in lab system.
C     ROTM      3x3 rotation matrix.
C         note:   If the requested volume dose not exits, returned
C                 parameters will be, 
C                   HSHAPE= '    '    (i.e.  blank)
C                   NSPAR = 0
C     NBUF      no of spacial parameters filled in this program.
C     IBUF      spacial parameters
C     
C  S.Kunori,    07-JAN-87
C  SK  2/89    PLANES NOW COUNT DOWN.  
C              GAP BETWEEN PLANE 1 AND 2 INCLUDED FOR 4-PLANE MODULE.
C  A.Taketani  01-JUL-91 Modify for MGEO V3.2
C  A.T.        13-DEC-91 Get informationt about type of MGEO from 
C                        IC(LMHGEO+8)
C  A.T.        07-MAY-92 Correct wire length
C  A.T.        05-OCT-92 fix for GEANT production
C            Definition of chamber length, SPAR(3), for MC generation
C            and reconstruction is external size of chamber which
C            include whole dead region such as aliminum wall and and
C            electronics. This assumption has been implemented in D0
C            GEANT. For real data analysis, it is wire length which is
C            the distance between inside edge of both end wall.
C  A.T.      DEC-92 CHANGE CHAMBER LENGTH DEFINITION FOR GEANT
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
      INTEGER NMOD,NSPAR
      REAL    SPAR(3),XPAR(3),ROTM(3,3)
      CHARACTER*4 HSHAPE
      INTEGER NBUF,IBUF(*)
C  external variables...
      INTEGER GZMGEO, GZMGEH
C  local variables...
      INTEGER I,J,K,LM,NPLANE, LH
      INTEGER IDBFLG            ! debug flag, 0=no print, 1=print
      REAL    DYMIN,DYMAX,DY,XYZLOC(3),XYZLAB(3)
C
      LOGICAL MC, FIRST
      INTEGER VSN
      REAL    MUINEFL
      LOGICAL MUGEANT_FLG
      DATA FIRST/.TRUE./
      DATA IDBFLG/0/
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        IF ( LHEAD.NE.0 ) THEN
          MC = IQ(LHEAD+1).GT.999
        ELSE
          MC = MUGEANT_FLG(2)
        END IF
      END IF
C
C -- get pointer to MGEO bank...
      LM=GZMGEO(NMOD)
      LH=GZMGEH(1)
C -- check if the bank exist...
      IF(LM.EQ.0) THEN                                  
C     -- MGEO bank dose not exist...
         HSHAPE='    '
         NSPAR=0
         CALL VFILL(SPAR,3,10000.)
         CALL VFILL(XPAR,3,10000.)
         CALL VFILL(ROTM,9,10000.)
      ELSE
C     -- MGEO bank exists.   Now calculate parameters...   
C
C     -- store shape of volume and number of parameters...
C
         HSHAPE='BOX '
         NSPAR=3        
C
C     -- store rotation matrix.
C
         K=0
         DO 100 I=1,3
         DO 110 J=1,3
           K=K+1
           ROTM(J,I)=C(LM+K+22)+C(LM+K+49)      
  110    CONTINUE
  100    CONTINUE 
C
C     -- shape parameters, which is calculated from number of planes
C        number of cells, wire spacing in x and y direction, etc.
C
         DYMIN=9999.0
         DYMAX=-9999.0
         NPLANE=IC(LM+10)
         IF(NPLANE.GE.0) THEN
            DO 200 I=1,NPLANE
               DY=C(LM+I+37)
               IF(DY.LT.DYMIN) THEN
               DYMIN=DY
               ENDIF
               IF(DY.GT.DYMAX) THEN
               DYMAX=DY
               ENDIF
  200       CONTINUE      
         ENDIF
         DY=DYMAX-DYMIN     
C     -- Geometry version
         VSN = IC(LM+8)
C     -- full width of volume...
         SPAR(1)=C(LM+33)*FLOAT(NPLANE)+C(LM+42)
         SPAR(2)=C(LM+32)*FLOAT(IC(LM+11))+DY
         IF ( MC ) THEN
           IF ( IC(LM+1).GE.3 ) THEN
             SPAR(3) = C(LM+14)
           ELSE
             SPAR(3) = C(LM+34)            ! GEANT DATA
           END IF
         ELSE
           IF ( VSN.NE.0 ) THEN 
             SPAR(3) = C(LM+14)          ! Survey
           ELSE
             SPAR(3) = C(LM+34) - MUINEFL()  ! Non survey
           END IF
         END IF
C     -- convert to half width...
         SPAR(1)=SPAR(1)/2.0
         SPAR(2)=SPAR(2)/2.0
         SPAR(3)=SPAR(3)/2.0
C
C     -- calculate the center of gravity of th e module...
C            
         XYZLOC(1)=C(LM+37)-(C(LM+33)*FLOAT(IC(LM+10)-1)+C(LM+42))/2.
         XYZLOC(2)=(DYMIN+DYMAX+C(LM+32)*FLOAT(IC(LM+11)-1))/2.
         IF ( VSN.EQ.0 ) THEN 
           XYZLOC(3)=C(LM+34)/2.
         ELSE
           XYZLOC(3)=0.0        
         END IF
C     -- rotate back to lab system.   (s/r vmatr from kernlib)
         CALL VMATR(XYZLOC,ROTM,XYZLAB,3,3)
C     -- center of module at lab system...
         XPAR(1)=C(LM+20)+C(LM+47)+XYZLAB(1)
         XPAR(2)=C(LM+21)+C(LM+48)+XYZLAB(2)
         XPAR(3)=C(LM+22)+C(LM+49)+XYZLAB(3)
      ENDIF
C
C--------debug---------------------------------------------------
      IF(IDBFLG.NE.0) THEN
         IF(LM.NE.0) THEN
            WRITE(6,61) LM,NMOD,HSHAPE,NSPAR,SPAR,XPAR,ROTM
61          FORMAT(' DEBUG IN S/R MUMODU...'/' LM',I10,' NMOD',I10
     1      /'  HSHAPE,NSPAR=',A4,I3/'  SPAR(1:3)',3F7.1
     2      ,'  XPAR(1:3)',3F7.1     
     3      /'  ROTM(1:9)',9F7.3)                            
            WRITE(6,62) XYZLOC,XYZLAB
62          FORMAT('  XYZLOC(1:3)',3F7.1,' XYZLAB(1:3)',3F7.1)
         ENDIF
      ENDIF
C-------end of debug---------------------------------------------
      RETURN
      END
