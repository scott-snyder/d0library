      SUBROUTINE GNSOUT(X,ISH,PAR,IDIST,SNEXT,SAFE)
C.
C.            ********************************************************
C.            *                                                      *
C.            *       ROUTINE TO FIND DISTANCE FROM POINT X(1-3)     *
C.            *       ALONG VECTOR X(4-6) TO SHAPE TYPE SH WITH      *
C.            *       PARAMETERS PAR. IF IDIST EQ 0 OR DISTANCE      *
C.            *       IS LESS THAN CURRENT VERSION OF SNEXT NEW      *
C.            *       DISTANCE IS RETURNED IN SNEXT AND IDIST IS     *
C.            *       SET TO 1.                                      *
C.            *                                                      *
C.            *    ==>Called by : GNEXT                              *
C.            *         Author  A.McPherson  *********               *
C.            *                                                      *
C.            ********************************************************
C.
      COMMON/GCUNIT/LIN,LOUT,NUNITS,LUNITS(5)
      INTEGER LIN,LOUT,NUNITS,LUNITS
      COMMON/GCMAIL/CHMAIL
      CHARACTER*132 CHMAIL
C
C
      DIMENSION X(6),PAR(*)
C.
C.    ----------------------------------------------------------------
C.
C
C             SHAPE FLAG = 5 IS A TUBE.
C
      IF(ISH.EQ.5)THEN
         CALL GNOTUB(X,PAR,IDIST,SNEXT,SAFE,1)
C
C             SHAPE FLAG = 6 IS A SEGMENTED TUBE.
C
      ELSEIF(ISH.EQ.6)THEN
         CALL GNOTUB(X,PAR,IDIST,SNEXT,SAFE,2)
C
C             SHAPE FLAG = 1 IS A BOX.
C
      ELSEIF(ISH.EQ.1)THEN
         CALL GNOBOX(X,PAR,IDIST,SNEXT,SAFE)
C
C             SHAPE FLAG = 2 IS TRAPEZOID ONLY X WIDTH VARYING.
C
      ELSEIF(ISH.EQ.2)THEN
         CALL GNOTRA(X,PAR,IDIST,SNEXT,SAFE,1)
C
C             SHAPE FLAG = 3 IS TRAP. X AND Y WIDTHS VARYING.
C
      ELSEIF(ISH.EQ.3)THEN
         CALL GNOTRA(X,PAR,IDIST,SNEXT,SAFE,2)
C
C            SHAPE FLAG = 4 IS A GENERAL TRAPEZOID.
C
      ELSEIF(ISH.EQ.4)THEN
         CALL GNOTRP(X,PAR,IDIST,SNEXT,SAFE)
C
C             SHAPE FLAG = 7 IS A CONE.
C
      ELSEIF(ISH.EQ.7)THEN
         CALL GNOCON(X,PAR,IDIST,SNEXT,SAFE,1)
C
C             SHAPE FLAG = 8
C
      ELSEIF(ISH.EQ.8)THEN
         CALL GNOCON(X,PAR,IDIST,SNEXT,SAFE,2)
C
C             SHAPE FLAG = 9 IS A SPHERICAL SHELL.
C
      ELSEIF(ISH.EQ.9)THEN
         CALL GNOSPH(X,PAR,IDIST,SNEXT,SAFE)
C
C             SHAPE FLAG 10 = PARALLELEPIPED.
C
      ELSEIF(ISH.EQ.10)THEN
         CALL GNOPAR(X,PAR,IDIST,SNEXT,SAFE)
C
C            SHAPE FLAG 11 = POLYGON.
C            I CHANGED IT A BIT
C
      ELSEIF(ISH.EQ.11)THEN
         CALL GNOPGO(X,PAR,2,SNEXT,SNXT,SAFE)
         IF(SNXT.LT.SNEXT) THEN
          SNEXT = SNXT
          IDIST = 1
         ENDIF
C
C           SHAPE FLAG 12 = POLYCONE.
C
      ELSEIF(ISH.EQ.12)THEN
         CALL GNOPCO(X,PAR,IDIST,SNEXT,SAFE)
C
C           Shape flag 28 = general twisted trapezoid.
C
      ELSEIF(ISH.EQ.28) THEN
         CALL GSNGTR(X,PAR,SNEXT,IDIST,0)
         CALL GSAGTR(X,PAR,SAFE,0)
C
C           USER SHAPE ?
C
      ELSE
         CALL GUNSOU(X,SH,PAR,IDIST,SNEXT,SAFE,IFLG)
      ENDIF
C
  999 CONTINUE
      END
