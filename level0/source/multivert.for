      SUBROUTINE MULTIVERT(NVER,ZVER,DZVER,TYPVER,WGTVER,METVER,CZZVER,
     &                     TRKVER)
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
C               METVER(1:NVER) : method of vertex reconstruction
C               CZZVER(1:NVER) : correlation matrix elem CZZ
C               TRKVER(1:NVER) : number of tracks contributing to vertex
C
C-   Created   2-OCT-1992   Jeffrey Bantly  from ZVERTE.FOR orig
C-   Updated   1-JUL-1993   Jeffrey Bantly  add METVER,CZZVER,TRKVER.
C
C------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NVER,IVER,ICONT(10),TYPVER(10),WGTVER(10),IVERT(14)
      INTEGER METVER(10),TRKVER(10)
      REAL VERT(14),ZVER(10),DZVER(10),CZZVER(10)
      EQUIVALENCE ( IVERT,VERT )
C------------------------------------------------------------------
      NVER=0
      CALL VZERO(ZVER,10)
      CALL VZERO(DZVER,10)
      CALL VZERO(TYPVER,10)
      CALL VZERO(WGTVER,10)
      CALL VZERO(TRKVER,10)
      CALL VZERO(CZZVER,10)
      CALL VZERO(METVER,10)
      CALL GTVERH(ICONT)
      NVER=ICONT(2)
      IF ( NVER.GT.10 ) NVER = 10
      DO 100 IVER=1,NVER
        CALL GTVERT(IVER,VERT)
        ZVER(IVER)=VERT(5)
        DZVER(IVER)=VERT(8)
        IF ( BTEST(IVERT(2),31) ) TYPVER(IVER)=1
        IF ( BTEST(IVERT(2),30) ) TYPVER(IVER)=2
        IF ( BTEST(IVERT(2),29) ) TYPVER(IVER)=3
        WGTVER(IVER)=IBITS(IVERT(2),0,8)
        IF ( BTEST(IVERT(2),28) ) METVER(IVER)=5
        IF ( BTEST(IVERT(2),27) ) METVER(IVER)=4
        IF ( BTEST(IVERT(2),26) ) METVER(IVER)=3
        IF ( BTEST(IVERT(2),25) ) METVER(IVER)=2
        IF ( BTEST(IVERT(2),24) ) METVER(IVER)=1
        CZZVER(IVER)=VERT(14)
        TRKVER(IVER)=IBITS(IVERT(2),8,8)
  100 CONTINUE
C------------------------------------------------------------------
 1000 RETURN
      END
