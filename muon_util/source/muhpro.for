C VAX/DEC CMS REPLACEMENT HISTORY, Element MUHPRO.FOR
C *1    15-SEP-1993 17:52:55 DARIEN "New MF code for 1B MUD1"
C VAX/DEC CMS REPLACEMENT HISTORY, Element MUHPRO.FOR
      SUBROUTINE MUHPRO(NMOD,NCEL,NTIME,HIT,IORENT,POS)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Convert hit signals to space points
C-
C-    Input  :  NMOD   - Module ID
C-              NCEL   - Cell address
C-              HIT(6) - Input hit values
C-                       HIT(1)= drift time 1
C-                       HIT(2)= drift time 2
C-                       HIT(3)= pad pulse  1
C-                       HIT(4)= pad pulse  2
C-                       HIT(5)= delta time 1
C-                       HIT(6)= delta time 2
C-
C-    Output :  IORENT - Module orientation
C-              POS(10)- Output position values
C-                       POS(1)= drift distance 1
C-                       POS(2)= drift distance 2
C-                       POS(3)= time division 1
C-                       POS(4)= time division 2
C-                       POS(5)= vernier position 1
C-                       POS(6)= vernier position 2
C-                       POS(7)= x center of wire
C-                       POS(8)= y center of wire
C-                       POS(9)= z center of wire
C-                       POS(10)= wire length
C-
C-    Created :  2-SEP-93  M. Fortner
C-     7/94 DH  allow hits missing drift times
C-----------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMOD,NCEL,NTIME,IORENT,IERR
      INTEGER NWIR,NPLN,IODD,J
      REAL HIT(6),POS(10)
      REAL VECT(3),WLEN,VOFF
      REAL DELT,XTDV,TCOR,TIME
      REAL PAD1,PAD2,VER1,VER2
      REAL MUDRFT
      EXTERNAL MUDRFT
C
CC               Get wire and plane address
      NWIR = NCEL/4
      NPLN = NCEL - NWIR*4
      IF(MOD(NWIR,2).EQ.0) THEN
          IODD = 0     ! EVEN CELL
      ELSE
          IODD = 1     ! ODD CELL
      ENDIF
C
CC               Get chamber constants
      CALL MUGEOM(NMOD,NPLN,NWIR,VECT,WLEN,VOFF,IORENT)
      DO J = 1,3
          POS(6+J) = VECT(J)
      ENDDO
      POS(10) = WLEN
C
CC               Process wire information
      DO J=1,4
        POS(J)=99999.
      ENDDO
      IF(NTIME.GE.1) THEN
        DO J = 1,NTIME
            DELT = HIT(4+J)
            CALL MUTDV(DELT,WLEN,XTDV)
            IF(IORENT.LT.0) XTDV = -XTDV
            POS(2+J) = XTDV
            XTDV = 0.
            CALL MUTCOR(IORENT,IODD,VECT,XTDV,WLEN,TCOR)
            TIME = HIT(J) - TCOR
            POS(J) = MUDRFT(TIME,0.,NMOD)
        ENDDO
      ENDIF
C
CC               Determine vernier positions
      PAD1 = HIT(3)
      PAD2 = HIT(4)
      CALL MUVERN(IORENT,PAD1,PAD2,VOFF,WLEN,VER1,VER2)
      POS(5) = VER1
      POS(6) = VER2
C
      RETURN
      END
