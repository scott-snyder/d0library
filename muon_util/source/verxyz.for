      SUBROUTINE VERXYZ(IVER,VERTEX,NV)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC    RETURNS XYZ LOCATION OF VERTEX
C                   IVER = 0 THEN central, IVER = 1 random cosmic
C      OUTPUT       VERTEX(3)  coordinates of vertex chosen
C                   NV  vertex number chosen
CC    In RCP if MUVERT = 0  then use central tracking
C               = 1  then assume cosmic ray moving downward
C               = 2  then use ISAJET vertex
C               = 3  Level 2 (beam)
CC     HEDIN 10-6-86
C      DH 6/90 ADD MUVERT
C      Modified  02-11-91  S. Abachi   Vertex with smaller error chosen
C      Modified  03-07-91  S. Abachi   Vertex number added to argument
C       DH 10/91 check on nver=0
C       DH 1/92 add Level 2
C      Modified  08-10-92  S. Abachi   Always choose first vertex
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INTEGER I,GZMUOT,MUVERT,FIRST,NVER,N1,N2,N3,N4,LMUOT
      INTEGER IVER,NV
      INTEGER NSAMUS,IFW3,ISPARE,IER
      REAL L2_VERT
      REAL ELCAL,ELFE,SPARE1,SPARE2,DZV
      REAL VERTEX(3),ZV,DZ,ZVER(10),DZVER(10),A1,A2,A3,A4,A5,
     A A6,A7,A8,A9,A0,X,Y,Z,U,V,W
      DATA FIRST/0/
C
      NV = 0
      IF(FIRST.EQ.0) THEN
        CALL EZGET('MUVERT',MUVERT,IER)
        FIRST=1
      ENDIF
      DO  I=1,3
        VERTEX(I)=0.
      ENDDO
      IF(MUVERT.EQ.0) THEN          ! USE CENTRAL TRACKING
        IVER=0
        CALL ZVERTE(NVER,ZVER,DZVER)
        IF(NVER.GE.1) THEN
          VERTEX(3) = ZVER(1)
          DZV = DZVER(1)
          NV = 1
        ENDIF
      ELSE IF(MUVERT.EQ.1) THEN      ! COSMIC; USE MUON TRACK
        IVER=1
        VERTEX(2)= 600.
        LMUOT=GZMUOT(1)
        IF(LMUOT.NE.0) THEN
          CALL GTMUOT(1,N1,NSAMUS,N2,N3,N4,IFW3,ISPARE,
     X                 A1,A2,A3,X,Y,Z,
     A                 A4,A5,A6,U,V,W,A7,A8,A9,A0,
     X                 ELCAL,ELFE,SPARE1,SPARE2)
          VERTEX(1)=X+U/V*(VERTEX(2)-Y)
          VERTEX(3)=Z+W/V*(VERTEX(2)-Y)
        ENDIF
      ELSE IF(MUVERT.EQ.2) THEN
        IVER=0
        CALL ZVERTX(ZV,DZ)           ! USE ISAJET
        VERTEX(3)=ZV
       ELSE IF(MUVERT.EQ.3) THEN     ! LEVEL 2
        IVER=0
        VERTEX(3) = L2_VERT()
      ENDIF
      RETURN
      END
