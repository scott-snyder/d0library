      SUBROUTINE OBJECT_VERTEX(IOBJ,NSIZE,VARRAY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get information for the IOBJth vertex
C-
C-   Inputs  : IOBJ: the IOBJth vertex
C-             NSIZE: the size of the array
C-   Outputs : VARRAY: Vertex array contains informations for this vertex
C-             VARRAY(1): X of the vertex
C-             VARRAY(2): Y of the vertex
C-             VARRAY(3): Z of the vertex
C-             VARRAY(4): DX
C-             VARRAY(5): DY
C-             VARRAY(6): DZ
C-             VARRAY(7): WEIGHT -- percentage of track number for this vertex
C-             VARRAY(8): flag word: 1.0 -- vertex constructed by CDC tracks
C-                                   2.0 -- vertex constructed by FDC tracks
C-                                   3.0 -- vertex constructed by VTX hits
C-             VARRAY(9): Quality_flag  1.0 -- main primary vertex
C-                                     >1.0 -- additional primary vertex
C-
C-   Created  23-NOV-1992   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IOBJ, NSIZE
      REAL    VARRAY(*)
      INTEGER NOBJ, OBJSIZE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      INTEGER LVERT, GZVERT, IBIT
      INTEGER LVERH, GZVERH, NVERT
      LOGICAL YES
      INTEGER IWORD
      REAL    RWORD
      EQUIVALENCE(IWORD,RWORD)
C----------------------------------------------------------------------
C
      CALL VZERO(VARRAY,9)
      LVERT = GZVERT(IOBJ)
      IF (LVERT .LE. 0) GOTO 999
      CALL UCOPY(Q(LVERT+3),VARRAY,6)
      IF (NSIZE .LE. 6) GOTO 999
      VARRAY(7) = IBITS(IQ(LVERT+2),0,7)
      DO 100 IBIT = 24, 28
        YES = BTEST(IQ(LVERT+2),IBIT)
        IF (YES) THEN
          VARRAY(8) = 1.0
          IF (IBIT .EQ. 26) THEN
            VARRAY(8) = 2.0
          ELSE
            IF (IBIT .EQ. 28) THEN
              VARRAY(8) = 3.0
            ENDIF
          ENDIF
          GOTO 200
        ENDIF
  100 CONTINUE
  200 CONTINUE
      IWORD = IBITS(IQ(LVERT+2),31,1)
      VARRAY(9) = RWORD
      IF ((VARRAY(9) .EQ. 0.0) .AND. BTEST(IQ(LVERT+2),30)) THEN
        IWORD = 2
        VARRAY(9) = RWORD
      ENDIF
C
      RETURN
      ENTRY NOBJ_VERTEX(NOBJ,OBJSIZE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get number of vertices 
C-
C-   Inputs  : none
C-   Outputs : NOBJ: number of vertices
C-             OBJSIZE: how many information per vertex
C-
C-   Created  23-NOV-1992   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
C
      NOBJ = 0
      LVERH = GZVERH()
      IF (LVERH .LE. 0) GOTO 999
      NVERT = IQ(LVERH+2)
      NOBJ = NVERT
      OBJSIZE = 9
C
  999 RETURN
      END
