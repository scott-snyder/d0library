      SUBROUTINE TRNUMB(ITRA,ISVER,ISTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DETERMINE ISAJET TRACK AND VERTX NUMBER
C-                         FOR THE CURRENT TRACK
C-
C-   Inputs  : ZEBCOM
C-   Outputs :
C-
C-   Created   1-DEC-1987   A. ZYLBERSTEJN
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      REAL VERT(3),PVERT(4),UBUF(10)
C
       INTEGER IPART,NVERT,NUBUF
       INTEGER ITRA,ISVER,ISTR
C
       IF (ITRA.NE.0) THEN
         CALL GFKINE(ITRA,VERT,PVERT,IPART,NVERT,UBUF,NUBUF)
          ISVER= UBUF(4)
          ISTR=UBUF(5)
       ENDIF
C      CALL GPKINE(ITRA)
C      PRINT*,' DANS TRNUMB,ITRA',ITRA,' ISAJET TRACK',ISTR
C      PRINT*,' NUBUF',NUBUF
      END
