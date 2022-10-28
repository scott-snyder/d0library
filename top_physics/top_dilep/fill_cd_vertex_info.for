      SUBROUTINE FILL_CD_VERTEX_INFO(NVER,WGTVER,METVER,NVERTRK)
C------------------------------------------------------------------
C
C     Returns number primary vertices and z and dz
C
C     Input from banks VERT
C     Output:
C               NVER           : number of primary vertices
C               WGTVER(1:NVER) : percent of track number
C               METVER(1:NVER) : method of vertex reconstruction, 1=CDC,3=FDC
C               NVERTRK(1:NVER) : number of tracks used for vertex calculations
C
C-   Created   2-OCT-1992   Jeffrey Bantly  from ZVERTE.FOR orig 
C-   Modified 27-FEB-1996   RE Hall change name Fill_*, prevent lib conflict
C
C------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:VERTEX_MULTI.PARAMS'
C
      INTEGER NVER,IVER,ICONT(10),TYPVER(MAXVER), WGTVER(MAXVER)
      INTEGER METVER(MAXVER),NVERTRK(MAXVER)
      INTEGER IVERT(14)
      REAL VERT(14),ZVER(MAXVER),DZVER(MAXVER),CZZVER(MAXVER)
      EQUIVALENCE ( IVERT,VERT )
C------------------------------------------------------------------
      NVER=0
      CALL VZERO(ZVER,MAXVER)
      CALL VZERO(DZVER,MAXVER)
      CALL VZERO_i(TYPVER,MAXVER)
      CALL VZERO_i(WGTVER,MAXVER)
      CALL VZERO_i(METVER,MAXVER)
      CALL VZERO(CZZVER,MAXVER)
      CALL VZERO_i(NVERTRK,MAXVER)
C
      CALL GTVERH(ICONT)
      NVER=ICONT(2)
      IF ( NVER.GT.MAXVER ) NVER = MAXVER
      DO 100 IVER=1,NVER
        CALL GTVERT(IVER,VERT)
        ZVER(IVER)=VERT(5)
        DZVER(IVER)=VERT(8)
        IF ( BTEST(IVERT(2),31) ) TYPVER(IVER)=1
        IF ( BTEST(IVERT(2),30) ) TYPVER(IVER)=2
        IF ( BTEST(IVERT(2),29) ) TYPVER(IVER)=3
        NVERTRK(IVER)=IBITS(IVERT(2),8,8)
        WGTVER(IVER)=IBITS(IVERT(2),0,8)
        IF ( BTEST(IVERT(2),28) ) METVER(IVER)=5
        IF ( BTEST(IVERT(2),27) ) METVER(IVER)=4
        IF ( BTEST(IVERT(2),26) ) METVER(IVER)=3
        IF ( BTEST(IVERT(2),25) ) METVER(IVER)=2
        IF ( BTEST(IVERT(2),24) ) METVER(IVER)=1
        CZZVER(IVER)=VERT(14)
  100 CONTINUE
C------------------------------------------------------------------
 1000 RETURN
      END
