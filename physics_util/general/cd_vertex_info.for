      SUBROUTINE CD_VERTEX_INFO(NVER,ZVER,DZVER,TYPVER,WGTVER,
     &         METVER,CZZVER,NVERTRK)
C------------------------------------------------------------------
C
C     Returns number primary vertices and z and dz
C
C     Input from banks VERT
C     Output:
C               NVER           : number of primary vertices
C               ZVER(1:NVER)   : z coordinates
C               DZVER(1:NVER)  : errors of z coordinates
C               TYPVER(1:NVER) : 1=primary, 2=other primary, 3=secondary
C               WGTVER(1:NVER) : percent of track number
C               METVER(1:NVER) : method of vertex reconstruction, 1=CDC,3=FDC
C               CZZVER(1:NVER) : correlation matrix elem CZZ
C               NVERTRK(1:NVER) : number of tracks used for vertex calculations
C
C-   Created   2-OCT-1992   Jeffrey Bantly  from ZVERTE.FOR orig 
C-   Updated   8-DEC-1995   Tracy L. Taylor  Add option for PATH=MDST 
C
C------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:VERTEX_MULTI.PARAMS'
C
      INTEGER NVER,IVER,ICONT(10),TYPVER(MAXVER), WGTVER(MAXVER)
      INTEGER METVER(MAXVER),NVERTRK(MAXVER)
      INTEGER IVERT(14)
      REAL VERT(14),ZVER(MAXVER),DZVER(MAXVER),CZZVER(MAXVER)
      REAL DUMMY(3,3)
      LOGICAL OK
      CHARACTER*4 PATH
      EQUIVALENCE ( IVERT,VERT )
C------------------------------------------------------------------
      CALL PATHGT(PATH)
      IF ( PATH.EQ.'MDST' ) THEN
        CALL VERTEX_INFO(3, NVER, DUMMY, OK )
      ELSE
        CALL GTVERH(ICONT)
        NVER=ICONT(2)
        IF ( NVER.GT.MAXVER ) NVER = MAXVER
      ENDIF
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
