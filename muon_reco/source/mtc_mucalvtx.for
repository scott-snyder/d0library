      SUBROUTINE MTC_MUCALVTX(NTRK,XM,NV,VERTEX,NC,IVTX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : In case of multiple vertices, determine
C-                         which vertex is prefered by calorimeter
C-
C-   Inputs  :  NTRK             MUON track number
C-              XM(3)            Mid mag coordinate
C-              NV               Numver of vertices in this event 
C-              VERTEX(3,*)      Coordinates for all vertices
C-              NC               Number of cal neighboring cells to consider
C-
C-   Outputs :  IVTX         Vertex number chosen by Cal
C-   Controls:
C-
C-   Created  10-FEB-1994   Daria Zieminska  
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:MTC.INC'
      INTEGER NV,IVTX,NC,MAXV,NTRK,MUVERT 
      INTEGER RUN,ID
      PARAMETER (MAXV=9)
      REAL EVERT(3),CHITR(MAXV),CHITRV(MAXV),CHIEN(MAXV),CHIEN2(MAXV),
     1     FRACT(MAXV),HFRACT(MAXV),HFRMIN,FRMIN,EN5MIN
      REAL XM(3),VERTEX(3,*)
      REAL VL,THETA,PHI,DIRC(3),EEM,ETOT,ET,EPS
      REAL TANT,ETA,VERT(3),CONT(14),PI
      INTEGER I,IER,IVTRY,IV
      LOGICAL FIRST
      DATA EPS,PI /1.0E-5,3.1415/
      DATA FRMIN,EN5MIN/0.499,0.5/
      DATA FIRST/.TRUE./
C
      IVTX = 0
      IF (NV.EQ.0) GO TO 999
      HFRMIN=0.499
      IF (FIRST.EQ..TRUE.) THEN
        FIRST=.FALSE.
        CALL EZGET('MUVERT',MUVERT,IER)
      END IF
      CALL EVNTID(RUN,ID)
      DO 100 IV=1,NV
        VL=0.
        DO I=1,3
          VERT(I)=VERTEX(I,IV)
          VL = VL + (VERT(I) - XM(I))**2
        END DO
        VL = SQRT(VL)
        DO I=1,3
          DIRC(I) = (XM(I) - VERT(I)) / VL
        ENDDO
        PHI = ATAN2(DIRC(2), DIRC(1))
        IF(DIRC(2) .LT. 0.0) PHI = PHI + 2.0 * PI
        THETA = ACOS(DIRC(3))
C
        IF(THETA .LT. EPS) THETA = THETA + EPS
        IF(THETA .GT. 3.1) THETA = THETA - EPS
        TANT = TAN(THETA/2.0)
        ETA = -ALOG(TANT)
        EVERT(1)=0.006         
        EVERT(2)=0.006          
        EVERT(3)=1.0           ! only EVERT(3) is used in MTC
        IF (MUVERT.EQ.0) THEN  ! CD vertex, get the z error from VERT
          CALL GTVERT(IV,CONT)
          EVERT(3)=MAX(CONT(8),0.1)
        END IF
        CALL MTC_MUCALTRACK(VERT,EVERT,ETA,PHI)
        CHITR(IV)=XMTC_TRES
        CHITRV(IV)=XMTC_TRES_V
        CHIEN(IV)=XMTC_ECHI
        CHIEN2(IV)=XMTC_ECHI2
        FRACT(IV)=XMTC_FRACT
        HFRACT(IV)=XMTC_HFRACT
        IF (XMTC_EN5.LT.EN5MIN) GO TO 90
        IF (FRACT(IV).LT.FRMIN) GO TO 90
        IF (HFRACT(IV).GT.HFRMIN) THEN
          IVTX=IV
          CALL MTCAFL(NTRK,IVTX)
          HFRMIN=HFRACT(IV)
        END IF
        IF (IV.GT.1) THEN
          IF (IVTX.EQ.IV-1) THEN
            IF (HFRACT(IV).EQ.HFRACT(IV-1)) THEN
              IF (FRACT(IV).GT.FRACT(IV-1)) THEN
                IVTX=IV
                CALL MTCAFL(NTRK,IVTX)
              END IF
            END IF
          END IF
        END IF
   90   CONTINUE
  100 CONTINUE 
  999 RETURN
      END
