      SUBROUTINE MNMINT( ITRAK, DRFT_MAX, WIRE_MAX, IMOD, ISCN, ICAT )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find sicnt-track intersection
C-
C-   Inputs  : ITRAK  : MUOT track ID
C_             DRFT_MAX : error on drift direction
C-             WIRE_MAX : error on wire dircetion
C-   Outputs : IMOD   : module ID
C-             ISCN   : scinti ID
C-             ICAT   : edge category
C-   Controls: none
C-
C-   Created  23-FEB-1994   Atsushi Taketani
C-   Updated   7-NOV-1994   Tao Hu ,  for  Octant 4 and 7 .
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  ITRAK, IMOD, ISCN, ICAT
      REAL     DRFT_MAX, WIRE_MAX
C-- Include
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'     ! D0 ZEBRA bank.
C-- Local 
      INTEGER NPTRAK, IADD(40)
      REAL    X(40), Y(40), Z(40)
      INTEGER I, IBMOD, DUM1, DUM2, IER
      INTEGER NMOD, ISMOD(9)
C
      REAL    TPOS(3), TVEC(3)
      INTEGER LM, GZMUOT
C
      CHARACTER*4  HSHAPE    ! for MUMODU
      INTEGER      NSPAR, NBUF, IBUF(10)
      REAL         SPAR(3),XPAR(3), ROTM(3,3)
      REAL         DPAR(3)
      INTEGER      K1,K2
      INTEGER      MUORIENT, IOCT,K
C
      REAL         TINT(3), DEV
      INTEGER      ICMOD, J, IREJ
      REAL         TERR(3)
C
      INTEGER      MUSCNT_ADR
C
      REAL         LOWEDGE, HIGHEDGE
C----------------------------------------------------------------------
      IMOD = 0
      TERR(1) = WIRE_MAX
      TERR(2) = WIRE_MAX
      TERR(3) = DRFT_MAX
C
C SCAN B-LAYER MODULE
C
      CALL GTMHOT( 0,ITRAK,NPTRAK,X,Y,Z,IADD)
      IF ( NPTRAK.EQ.0 ) GOTO 999
C
      DO 100 I=1,NPTRAK
        IF ( ABS(X(I)).GE.1000.0 ) GOTO 100
        IF ( ABS(Y(I)).GE.1000.0 ) GOTO 100
        IF ( ABS(Z(I)).GE.1000.0 ) GOTO 100
        CALL MUADD( IADD(I), IBMOD, DUM1, DUM2, IER )
        IF ( IBMOD.GE.100.AND.IBMOD.LE.149 ) GOTO 199
  100 CONTINUE
      GOTO 999
C
C get possible C-layer module
C
 199  CALL MNMBGE( IBMOD, NMOD, ISMOD )
C
C get track
C
      LM = GZMUOT(ITRAK)
      DO I=1,3
        TVEC(I) = Q(LM+16+I)
        TPOS(I) = Q(LM+10+I)
      END DO
C
C Scan module
C
      DO 200 I=1,NMOD
        CALL MUMODU(ISMOD(I),HSHAPE,NSPAR,SPAR,XPAR,ROTM,NBUF,IBUF)
        DO 210 K1=1,3
          DPAR(K1) = 0.0
        DO 220 K2=1,3
  220     DPAR(K1) = DPAR(K1) + ROTM(K1,K2)*SPAR(K2)
  210   DPAR(K1) = ABS(DPAR(K1))
        IF ( MUORIENT(ISMOD(I)).EQ.1 ) THEN    ! top
          IREJ = 2
        ELSE                                  ! side
          IREJ = 1
        END IF
        DEV = (XPAR(IREJ) - TPOS(IREJ))/TVEC(IREJ)
        DO J=1,3
          TINT(J) = TPOS(J) + TVEC(J)*DEV
          IF ( ABS(TINT(J)-XPAR(J)).GT.DPAR(J)+TERR(J) ) GOTO 200
        END DO
        ICMOD = ISMOD(I)
        GOTO 300
  200 CONTINUE
      GOTO 999
C
C scan each scinti
C
  300 CONTINUE
C
      DO 310 I=1,8
        MUSCNT_ADR = ICMOD + I*1000
        CALL MUSCNT(MUSCNT_ADR,HSHAPE,NSPAR,SPAR,XPAR,ROTM,NBUF,IBUF)
        DO 320 K1=1,3
          DPAR(K1) = 0.0
        DO 330 K2=1,3
  330     DPAR(K1) = DPAR(K1) + ROTM(K1,K2)*SPAR(K2)
  320   DPAR(K1) = ABS(DPAR(K1))
        DEV = (XPAR(IREJ) - TPOS(IREJ))/TVEC(IREJ)
        DO J=1,3
          TINT(J) = TPOS(J) + TVEC(J)*DEV
          IF ( ABS(TINT(J)-XPAR(J)).GT.DPAR(J)+TERR(J) ) GOTO 310
        END DO
        ISCN = I
        IMOD = ICMOD
        GOTO 400
  310 CONTINUE
      GOTO 999
C
C edge
C
  400 CONTINUE
      ICAT = 0
      LOWEDGE  = XPAR(3) - DPAR(3)
      HIGHEDGE = XPAR(3) + DPAR(3)
      IF      ( ABS(TINT(3)-LOWEDGE).LE.DRFT_MAX ) THEN
        ICAT = IBSET( ICAT,1)
      ELSE IF ( ABS(TINT(3)-HIGHEDGE).LE.DRFT_MAX ) THEN
        ICAT = IBSET( ICAT,0)
      END IF
C
      IOCT = MOD(IMOD,10)
C
      IF ( IOCT.EQ.0 .OR. IOCT.EQ.7 ) THEN
        K = 2
        LOWEDGE  = XPAR(K) - DPAR(K)
        HIGHEDGE = XPAR(K) + DPAR(K)
      ELSE IF ( IOCT.EQ.1.OR.IOCT.EQ.2 ) THEN
        K = 1
        LOWEDGE  = XPAR(K) + DPAR(K)
        HIGHEDGE = XPAR(K) - DPAR(K)
      ELSE IF ( IOCT.EQ.3.OR.IOCT.EQ.4 ) THEN
        K = 2
        LOWEDGE  = XPAR(K) + DPAR(K)
        HIGHEDGE = XPAR(K) - DPAR(K)
      ELSE
        GOTO 999
      END IF
C
      IF      ( ABS(TINT(K)-LOWEDGE).LE.WIRE_MAX ) THEN
        ICAT = IBSET( ICAT,3)
      ELSE IF ( ABS(TINT(K)-HIGHEDGE).LE.WIRE_MAX ) THEN
        ICAT = IBSET( ICAT,2)
      END IF
C
  999 RETURN
      END
