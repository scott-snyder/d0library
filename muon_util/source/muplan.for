      SUBROUTINE MUPLAN(NMOD,NPL,HSHAPE,NSPAR,SPAR,XPAR,ROTM
     +                 ,NBUF,IBUF)
C-------------------------------------------------------------------
C     S/R MUMODU returens geometry parameters for a plane in
C     a muon PDT module.
C
C  Input:
C     NMOD      module number.
C     NPL       plane number.     (=0,1,2,3)
C     NBUF      maximum no. of words(IBUF) to be filled in this
C               routine.
C    MGEO-bank  muon geometry constants.
C
C  Output:
C     HSHAPE    volume shape.          (character*4,   'BOX ')
C     NSPAR     number of parameters.  (=3)
C     SPAR      shape parameters.      (DX,DY,DZ,   half widthe etc.)
C     XPAR      coordinate of the center of volume in LAB system.
C     ROTM      3x3 rotation matrix.
C     NBUF      number of words (IBUF) filled in this routine.
C     IBUF      spacial parameters.   (not yet defined)
C             (1) Reserved
C             (2) Number of plane
C             (3) Number of cell per plane 
C
C     note:   If the requested volume dose not exits, output
C             parameters will be, 
C                  HSHAPE='    '    (i.e. blank)
C                  NSPAR =0
C                (others)=10000.
C
C  S.Kunori,    07-JAN-87
C  SK  2/89   PLANE COUNT DOWN.
C             GAP BETWEEN PLANE 1 AND 2 INCLUDED.
C  AT  8/91   Modify for MGEO V3.2
C  AT  12/91  Get type of MGEO from IC(LMGEO+8)
C  AT  05/92  Correct wire length
C  AT  10/92  fix for GEANT production
C            Definition of chamber length, SPAR(3), for MC generation
C            and reconstruction is external size of chamber which
C            include whole dead region such as aliminum wall and and
C            electronics. This assumption has been implemented in D0
C            GEANT. For real data analysis, it is wire length which is
C            the distance between inside edge of both end wall.
C  AT  12/92 CHAMBER LENGTH DEFINITON CHANGE FOR GEANT
C  AT  2/93  Fill special parameter
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
      INTEGER NMOD,NPL,NSPAR
      REAL    SPAR(3),XPAR(3),ROTM(3,3)
      CHARACTER*4 HSHAPE
      INTEGER NBUF,IBUF(*)
C  external variables...
      INTEGER GZMGEO 
C  local variables...
      INTEGER I,J,K,LM 
      INTEGER IDBFLG          ! debug flag,  =0 no print, =1 print
      REAL    XYZLOC(3),XYZLAB(3)                                
C
      INTEGER VSN
      LOGICAL FIRST, MC
      REAL    MUINEFL 
      LOGICAL MUGEANT_FLG
      DATA FIRST/.TRUE./
      DATA IDBFLG/0/
C
      IF ( FIRST ) THEN             ! data type identify
        FIRST = .FALSE.
        IF ( LHEAD.NE.0 ) THEN
          MC = IQ(LHEAD+1).GT.999
        ELSE
          MC = MUGEANT_FLG(2)
        END IF
      END IF
C -- get pointer to MGEO bank...
      LM=GZMGEO(NMOD)
C -- check if the bank exist...        
      IF(LM.EQ.0) THEN 
C     -- MGEO bank dose not exist...
         GO TO 800
      ELSE
C     -- MGEO bank exists.   Now calculate parameters...   
C                 
C     -- check input plane number if it is valid...
         IF(NPL.LT.0.OR.NPL.GE.IC(LM+10)) GO TO 800
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
         VSN = IC(LM+8)         
C
C     -- shape parameters...
C
         SPAR(1)=C(LM+33)/2.0
         SPAR(2)=C(LM+32)*FLOAT(IC(LM+11))/2.0
         IF ( MC ) THEN
           IF ( IC(LM+1).GE.3 ) THEN
             SPAR(3) = C(LM+14)/2.0
           ELSE
             SPAR(3)=C(LM+34)/2.0
           END IF
         ELSE
           IF ( VSN.NE.0 ) THEN
             SPAR(3) = C(LM+14)/2.0
           ELSE
             SPAR(3) = (C(LM+34) - MUINEFL())/2.0
           END IF
         END IF
C
C     -- calculate the center of graity of the plane...
C            
         XYZLOC(1)=C(LM+37)-C(LM+33)*FLOAT(NPL)
C        -- GAP BETWEEN PLANE 1 AND 2.
         IF(NPL.GE.2) THEN
            XYZLOC(1)=XYZLOC(1)-C(LM+42)
         ENDIF
         XYZLOC(2)=C(LM+NPL+38)+C(LM+32)*FLOAT(IC(LM+11)-1)/2.
         IF ( IC(LM+8).EQ.0 ) THEN      ! non survey mode
           XYZLOC(3)=C(LM+34)/2.
         ELSE                            ! survey mode
           XYZLOC(3)=0.0 
         END IF
C     -- rotate back to lab system.   (s/r vmatr from kernlib)
         CALL VMATR(XYZLOC,ROTM,XYZLAB,3,3)
C     -- center of module at lab system...
         XPAR(1)=C(LM+20)+C(LM+47)+XYZLAB(1)
         XPAR(2)=C(LM+21)+C(LM+48)+XYZLAB(2)
         XPAR(3)=C(LM+22)+C(LM+49)+XYZLAB(3)
C
C  -- fill special parameter
         IF ( NBUF.GE.2 ) THEN
           IBUF(2) = IC(LM+10) 
         END IF
         IF ( NBUF.GE.3 ) THEN
           IBUF(3) = IC(LM+11)
         END IF
      ENDIF                                             
C
      GO TO 900
C
C  -- module or plane dose not exist...
800   CONTINUE
         HSHAPE='    '
         NSPAR=0
         CALL VFILL(SPAR,3,10000.)
         CALL VFILL(XPAR,3,10000.)
         CALL VFILL(ROTM,9,10000.)
C
900   CONTINUE
C
C--------debug---------------------------------------------------
      IF(IDBFLG.NE.0) THEN
      IF(LM.NE.0.AND.NSPAR.NE.0) THEN
         WRITE(6,61) NMOD,NPL,HSHAPE,NSPAR,SPAR,XPAR,ROTM
61       FORMAT(' DEBUG IN S/R MUPLAN...'
     +   /'  NMOD',I5,'   NPL=',I5
     1   /'  HSHAPE,NSPAR=',A4,I3/'  SPAR(1:3)',3F7.1
     2   ,'  XPAR(1:3)',3F7.1     
     3   /'  ROTM(1:9)',9F7.3)                            
         WRITE(6,62) XYZLOC,XYZLAB
62       FORMAT('  XYZLOC(1:3)',3F7.1,' XYZLAB(1:3)',3F7.1)
      ENDIF           
      ENDIF
C-------end of debug---------------------------------------------
      RETURN
      END
