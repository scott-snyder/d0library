      SUBROUTINE BEAMXY(LVERT)
C----------------------------------------------------------------------
C-
C-   Fill x,y of primary vertex in bank VERT. 
C-   For real data: copy the average values x,y and the error matrix from run 
C                   dependent parameter file.
C-   For MC events: set (x,y) to (0,0) if SMEAR=0 or smear around 0 with 
C-                  smearing parameter SIGMA if SMEAR=1 
C-                  (SMEAR, SIGMA stored  in VERTEX.RCP) 
C-
C-   Input:    LVERT   = location of primary vertex bank 
C-   Outputs : none (bank words filled)
C-
C-   Created  22-NOV-1989   Daria Zieminska
C-   Updated   7-DEC-1992   Qizhong Li-Demarteau  added EZRSET and EZERROR,
C-                                                also added SAVE statement
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INTEGER IER,ICALL,SMEAR,LVERT,OPT
      REAL TWOPI,SIGMA,RANDOM,RNDM,PHI,SPHI,CPHI,RADIUS 
      PARAMETER (TWOPI=6.283185)
      LOGICAL MC, EZERROR
      DATA MC/.FALSE./
      SAVE ICALL
      DATA ICALL/0/
C----------------------------------------------------------------------
C
      IF (IQ(LHEAD+1) .GT. 1000) MC=.TRUE.
      IF (ICALL.EQ.0) THEN
        IF (MC) THEN
          CALL EZPICK('VERTEX_RCP')
          IF ( EZERROR(IER) ) THEN
            CALL ERRMSG('VERTEX','BEAMXY',
     &       'Unable to find bank VERTEX_RCP','W')
            GOTO 999
          ENDIF
          CALL EZGET('SMEAR_XY',SMEAR,IER)
          CALL EZGET('SIGMA',SIGMA,IER)
          CALL EZRSET
        END IF
        ICALL=1
      END IF
      IF (MC) THEN
        IF (SMEAR.EQ.0) THEN
          GO TO 999
        ELSE IF (SMEAR.EQ.1) THEN
          CALL NORRAN(RANDOM)
          RADIUS=ABS(RANDOM)*SIGMA
          PHI=RNDM(0)*TWOPI
          SPHI=SIN(PHI)
          CPHI=COS(PHI)
          Q(LVERT+3)=RADIUS*CPHI ! x_beam
          Q(LVERT+4)=RADIUS*SPHI ! y_beam
          Q(LVERT+6)=SIGMA/1.4142 
          Q(LVERT+7)=SIGMA/1.4142 
          Q(LVERT+9) =1. ! correlation matrix elem. cxx
          Q(LVERT+10)=0. ! cxy
          Q(LVERT+11)=1. ! cyy
          Q(LVERT+12)=1. ! cxz
          Q(LVERT+13)=0. ! cyz
          Q(LVERT+14)=1. ! czz
        END IF
      END IF
  999 RETURN
      END
