      SUBROUTINE MVERXYZ(IVER,MAXV,VERTEX,NV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns all 3D vertesis
C-
C-   Inputs  : IVER     (*IVER* IS INPUT & OUTPUT)
C-                    -1   Value is automatically obtained from MURECO.RCP
C-                     0   central
C-                     1   random cosmic
C-                     2   Isajet vertex
C-                     3   level 2
C-                   MAXV  maximum vertex allowed
C-
C-   Outputs :      IVER
C-                  VERTEX(3,*)  coordinates of all vertces
C-                  NV  number of verteces
C-   Controls:
C-
C-   Created   27-DEC-1992   SHAHRIAR ABACHI
C-   Modified  12-APR-1993   SHAHRIAR ABACHI  IVERT correctly implimented
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,GZMUOT,MUVERT,FIRST,NVER,N1,N2,N3,N4,LMUOT
      INTEGER IVER,NV,MUV,MAXV,JBIT
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NSAMUS,IFW3,ISPARE,IER,J,IV,IBT30,IBT31
      INTEGER LVERH,GZVERH,NPV,IVERT(14)
      REAL VERT(14)
      EQUIVALENCE (VERT,IVERT)
      REAL ELCAL,ELFE,SPARE1,SPARE2,DZV,L2_VERT
      REAL VERTEX(3,*),ZV,DZ,ZVER(10),DZVER(10),A1,A2,A3,A4,A5,
     A  A6,A7,A8,A9,A0,X,Y,Z,U,V,W
      DATA FIRST/0/
C
      NV = 0
      IF(IVER .EQ. -1) THEN
        IF(FIRST.EQ.0) THEN
          CALL EZGET('MUVERT',MUVERT,IER)
          FIRST=1
        ENDIF
        MUV = MUVERT
      ELSE
        MUV = IVER
      ENDIF
C
      DO J=1,MAXV
        DO I=1,3
          VERTEX(I,J)=0.
        ENDDO
      ENDDO
C
      IF(MUV.EQ.0) THEN          ! USE CENTRAL TRACKING
        LVERH = GZVERH()
        NPV = 0
        IF(LVERH .GT. 0) NPV = IQ(LVERH + 2)      ! Number of primary vertexis
        DO IV=1,MIN(NPV,MAXV)
          CALL GTVERT(IV,VERT)
          IBT30 = IBITS(IVERT(2),30,1)
          IBT31 = IBITS(IVERT(2),31,1)
          IF(IVERT(2) .EQ. 0.0 .OR.
     &              IBT30 .EQ. 1 .OR. IBT31 .EQ. 1) THEN
            NV = NV + 1
            DO I=1,3
              VERTEX(I,NV) = VERT(I + 2)
            ENDDO
          ENDIF
        ENDDO
      ELSE IF(MUV.EQ.1) THEN      ! COSMIC; USE MUON TRACK
        NV = 1
        VERTEX(2,1)= 600.
        LMUOT=GZMUOT(1)
        IF(LMUOT.NE.0) THEN
          CALL GTMUOT(1,N1,NSAMUS,N2,N3,N4,IFW3,ISPARE,
     X                 A1,A2,A3,X,Y,Z,
     A                 A4,A5,A6,U,V,W,A7,A8,A9,A0,
     X                 ELCAL,ELFE,SPARE1,SPARE2)
          VERTEX(1,1)=X+U/V*(VERTEX(2,1)-Y)
          VERTEX(3,1)=Z+W/V*(VERTEX(2,1)-Y)
        ENDIF
      ELSE IF(MUV.EQ.2) THEN
        NV = 1
        CALL ZVERTX(ZV,DZ)           ! USE ISAJET
        VERTEX(3,1)=ZV
      ELSE IF(MUV.EQ.3) THEN     ! LEVEL 2
        NV = 1
        VERTEX(3,1) = L2_VERT()
      ENDIF
C
      IF(IVER .EQ. -1) THEN
        IVER = MUV
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
