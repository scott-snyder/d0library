      SUBROUTINE VERTEX_XYRCP_RESTORE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to read beam XY position from the run dependent
C-                average values beam_pos_run etc and write in to beam_pos 
C-                in VERTEX_RCP
C-
C-   Inputs  : none
C-   Outputs : Parameters beam_pos etc are rewritten in VERTEX_RCP
C-
C-   Created  19-SEP-1994   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER IER, ERR, III, NSIZE
      PARAMETER( NSIZE = 10 )
      INTEGER BRUNNO(10)
      REAL    BEAM_POS_RUN(30), BEAM_ERR_RUN(30)
      REAL BEAM_POS(3), BEAM_ERR(3)
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VERTEX_RCP')
        ERR = 0
        CALL EZGET('BRUNNO(1)',BRUNNO(1),IER)
        ERR = ERR + IER
        CALL EZGET('BEAM_POS_RUN(1)',BEAM_POS_RUN(1),IER)
        ERR = ERR + IER
        CALL EZGET('BEAM_ERR_RUN(1)',BEAM_ERR_RUN(1),IER)
        ERR = ERR + IER
        IF (ERR.NE.0) CALL ERRMSG('VERTEX','XYVERT',
     &    'UNABLE TO FIND BEAM PARAMs in RCP','W') 
C
        IF (BRUNNO(1) .NE. 0) THEN
          DO 150 III = NSIZE, 1, -1
            IF (BRUNNO(III) .NE. 0) THEN
              IF (IQ(LHEAD+6) .GE. BRUNNO(III)) THEN
                BEAM_POS(1) = BEAM_POS_RUN(1+(III-1)*3)
                BEAM_POS(2) = BEAM_POS_RUN(2+(III-1)*3)
                BEAM_POS(3) = BEAM_POS_RUN(3+(III-1)*3)
                BEAM_ERR(1) = BEAM_ERR_RUN(1+(III-1)*3)
                BEAM_ERR(2) = BEAM_ERR_RUN(2+(III-1)*3)
                BEAM_ERR(3) = BEAM_ERR_RUN(3+(III-1)*3)
                GOTO 160
              ENDIF
            ENDIF
  150     CONTINUE
        ELSE
          CALL ERRMSG('VERTEX','XYVERT',
     &    'UNABLE TO FIND BEAM PARAMs in RCP','W') 
          GOTO 999
        ENDIF
  160   CALL EZSET('BEAM_POS',BEAM_POS(1),IER)
        CALL EZSET('BEAM_ERR',BEAM_ERR(1),IER)
        IF (IER.NE.0) CALL ERRMSG('VERTEX','VERTEX_XYRCP_RESTORE',
     &    'UNABLE TO STORE BEAM PARAMs in RCP','W')
        CALL EZRSET
      END IF
C
  999 RETURN
      END
