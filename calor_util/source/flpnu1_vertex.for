      SUBROUTINE FLPNU1_VERTEX(XYZV,DXYZV,NV,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Provide vertex position and its error for  FLPNU1 routine.
C-
C-   Inputs  : None
C-   Outputs : XYZV(3,*)    [R] Vertex positon
C-             DXYZV(3,*)   [R] Error in vertex position
C-             NV           [I] Number of additional primary vertices 
C-                              + object vertices found
C-                              
C-             IOK          [I] Success status:  0 = OK
C-                                              -1 = No VERT found
C-                                              -2 = No VERH found
C-   Controls: None
C-
C-   Created   12-SEP-1995   Dhiman Chakraborty
C-   Updated    2-OCT-1995   Dhiman Chakraborty   
C-                           Include object vertices
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PNU1.INC'
C----------------------------------------------------------------------
      INTEGER NV,NVERT,IOK,ISTAT
      INTEGER LVERH,LVERT,GZVERH
      INTEGER JBIT,JBYT
      REAL    XYBEAM(2)
      REAL    XYZV(3,*),DXYZV(3,*)
      LOGICAL USE_VERT_XY,USE_RCP_XY
      LOGICAL FIRST
      EXTERNAL GZVERH,JBIT,JBYT
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      NV = 0
      NVERT = 0
      IOK = 0
      CALL VZERO(XYZV,3*NVMAX)
      CALL VZERO(DXYZV,3*NVMAX)
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('CAHITS_RCP')
        CALL EZGET_l('USE_VERT_XY',USE_VERT_XY,IOK)
        CALL EZGET_l('USE_RCP_XY',USE_RCP_XY,IOK)
        IF(USE_VERT_XY.AND.USE_RCP_XY) CALL ERRMSG('RCP Mismatch',
     &    'CAEHFL_VERTEX',' Default is to use VERT bank XY','W')
        IF(USE_RCP_XY) CALL EZGET('RCP_BEAM_XY',XYBEAM,IOK)
        CALL EZRSET
      ENDIF
C
      NV = 0
      LVERH = GZVERH()
      IF(LVERH.LE.0) THEN
        IOK = -2
        GOTO 999
      ENDIF
      LVERT = LQ(LVERH-1)
      IF(LVERT.LE.0) THEN
        IOK = -1
        GOTO 999
      ENDIF
      DO WHILE (LVERT.GT.0)
        ISTAT = IQ(LVERT+2)
        IF((JBIT(ISTAT,31).EQ.0)           ! Not an Additional Primary Vertex
     &       .AND.(JBYT(ISTAT,17,3).EQ.0))  ! nor an Object Vertex
     &        GOTO 888  
        NV = NV+1
        IF(NV.LE.NVMAX)THEN
          XYZV(3,NV) = Q(LVERT+5)
          DXYZV(3,NV) = Q(LVERT+8)
          IF (USE_RCP_XY) THEN
            XYZV(1,NV) = XYBEAM(1)
            XYZV(2,NV) = XYBEAM(2)
          ELSE
            XYZV(1,NV) = Q(LVERT+3)
            XYZV(2,NV) = Q(LVERT+4)
            DXYZV(1,NV) = Q(LVERT+6)
            DXYZV(2,NV) = Q(LVERT+7)
          ENDIF
        ENDIF
  888   LVERT = LQ(LVERT)
      ENDDO
C
  999 RETURN
      END
