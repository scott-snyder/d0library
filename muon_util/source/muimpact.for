      SUBROUTINE MUIMPACT(MX,MIMP_DIST,MIMP_3D,MIMP_BEND,MIMP_NONB,
     &  VTX,DVTX,VERT_OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given mu & CD tracks return distance of
C-                         track to closest primary vertex
C-
C-   Inputs  : MX(6)  MX(1-3)=X0,Y0,Z0  MX(4-6)=DIR. COSINES OF TRACK
C-   Outputs : MIMP_DIST  - distance of mu track from closest vertex
C-             MIMP_3D    - coords. of (point of closest approach on
C-                          track) - (closest vertex).
C-             MIMP_BEND  - Muon bend view distance of mu track from
C-                          closest vertex.
C-             MIMP_NONB  - Muon non-bend view distance of mu track from
C-                          closest vertex.
C-             VERT_OK - logical: FALSE if no VERT banks
C-             
C-   Controls:
C-
C-   Created  27-AUG-1992   Jim Cochran
C-   Updated  08-OCT-1992   Cary Y. Yoshikawa - added return values
C-                            MIMP_3D, MIMP_BEND, MIMP_NONB.
C-            14-OCT-1992   S.Kunori   Change s/r name from IMPACT.
C-                            and MIMP_BEND,MIMP_NONB=-999. for init.
C-            11-JAN-1994   R. Markeloff  Now works for SSQ files
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LVERT,GZVERT,BIT30,BIT31,I,JBIT
      INTEGER MODE,LDUMMY,IER
      REAL MIMP_DIST,MX(6),VERT(3),MPOINT(3),MPNT(3),MIMPD
      REAL MIMP_3D(3),MIMP_BEND,MIMP_NONB,VTX(3),DVTX(3)
      LOGICAL VERT_OK
C----------------------------------------------------------------------
      MIMP_DIST = -999.
      MIMP_BEND = -999.
      MIMP_NONB = -999.
      LVERT = GZVERT(1)
      IF (LVERT .NE. 0) THEN
        VERT_OK = .TRUE.
        DO WHILE (LVERT.GT.0)          !-loop over all primary vertices
          BIT30 = JBIT(IQ(LVERT+2),31)           ! Vert bank counts from 0
          BIT31 = JBIT(IQ(LVERT+2),32)
          IF (BIT30 .EQ. 1 .OR. BIT31 .EQ. 1) THEN
            VERT(1) = Q(LVERT+3)
            VERT(2) = Q(LVERT+4)
            VERT(3) = Q(LVERT+5)
            CALL MUIMPP(VERT,MX,MPNT,MIMPD)      ! mu dist to Vertex
            IF (MIMPD .LT. MIMP_DIST) THEN
              MIMP_DIST = MIMPD
              DO I=1,3
                MPOINT(I) = MPNT(I)
                MIMP_3D(I) = MPOINT(I) - VERT(I)
                VTX(I) = VERT(I)
                DVTX(I) = Q(LVERT+5+I)
              ENDDO
              MIMP_BEND = MIMP_3D(3)
              MIMP_NONB = SQRT(MIMP_3D(1)**2+MIMP_3D(2)**2)
            ENDIF
          ENDIF
          LVERT = LQ(LVERT)
        ENDDO
      ELSE
C
C       VERT bank does not exist; assume SSQ data
C
        IF (IQ(LHEAD+6) .LT. 70000) THEN     ! Run 1a
          MODE = 1
        ELSE
          MODE = 2                           ! Run 1b
        ENDIF
        CALL MUGET_VERT(MODE, LDUMMY, VERT, DVTX, IER)
        VERT_OK = IER .EQ. 0
        CALL MUIMPP(VERT,MX,MPOINT,MIMP_DIST)
        DO I=1,3
          MIMP_3D(I) = MPOINT(I) - VERT(I)
        ENDDO
        MIMP_BEND = MIMP_3D(3)
        MIMP_NONB = SQRT(MIMP_3D(1)**2+MIMP_3D(2)**2)
      ENDIF
C
  999 RETURN
      END
