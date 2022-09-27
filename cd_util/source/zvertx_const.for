      SUBROUTINE ZVERTX_CONST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Perform a constrianed z-vertex fit
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  15-OCT-1993   Srini Rajagopalan
C-   Updated  13-DEC-1993   Srini Rajagopalan  Fix bug Q->IQ for word 2 in VFIT 
C-   Updated  13-DEC-1993   Srini Rajagopalan  Fix Theta error calculation 
C-   Updated  31-JAN-1994   Srini Rajagopalan  Fix loop back to find new window
C-                          even if NTRACK=0 is returned by zconst_fit.
C-   Updated   3-MAR-1994   Srini Rajagopalan  Add an estimated error due to
C-                    multiple scattering to the overall z measurement error. 
C-   Updated  12-JUL-1995   Srini Rajagopalan  Fetch BEAM_POS every new run 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZVFIT.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      LOGICAL FLAG
      LOGICAL DO_CONSTRAINED_FIT
C
      REAL LEAD,TAIL
      REAL ZV,EZV,CHISQ
C
      INTEGER IER,IVERT,ITRK,NTRACK
      INTEGER LVFIT,LDTRK,GZDTRK,LZFIND
C
      REAL BEAM_POS(3),BEAM_ERR(3)
      REAL THETA,ERR_THETA
C
      INTEGER RUN,RUNSAV,RUNNO
      DATA RUNSAV /-1/
C----------------------------------------------------------------------
C
      RUN = RUNNO()
      IF (RUN .NE. RUNSAV) THEN
        RUNSAV = RUN
        CALL EZPICK('VERTEX_RCP')
        CALL EZGET_l('DO_CONSTRAINED_FIT',DO_CONSTRAINED_FIT,IER)
        CALL EZGET_rarr('BEAM_POS',BEAM_POS,IER)
        CALL EZGET_rarr('BEAM_ERR',BEAM_ERR,IER)
        CALL EZRSET
      ENDIF
C
      IF (.NOT.DO_CONSTRAINED_FIT) GO TO 999
C
      NVERT = 0
C
    1 CONTINUE
      CALL ZCONST_PEAK(LEAD,TAIL,FLAG)
C
      IF (FLAG) THEN
        CALL ZCONST_FIT(LEAD,TAIL,NTRACK,ZV,EZV,CHISQ)
        IF (NTRACK.NE.0 .AND. NVERT.LT.MAX_VERT) THEN
          NVERT = NVERT + 1
          ZVERT(NVERT) = ZV
          ZERR(NVERT) = SQRT(EZV**2 + 0.25)
          ZCHI(NVERT) = CHISQ
          ZENT(NVERT) = NTRACK
C
          DO ITRK = 1,NTRACK
            LDTRK = LZFIND(IXCOM,GZDTRK(1),PID(ITRK),-5)
            IQ(LDTRK+15) = NVERT
C
            IF (ABS(SLOPE(ITRK)).GT.0.0) THEN
              THETA = ATAN(1.0/SLOPE(ITRK))
              ERR_THETA = ERR_SLOPE(ITRK)/(1.+SLOPE(ITRK)**2)
              IF (THETA.LT.0.0) THETA = THETA + PI
              Q(LDTRK+23) = THETA
              Q(LDTRK+24) = ERR_THETA
            ENDIF
          ENDDO
        ENDIF
C
        IF (NVERT.LT.MAX_VERT) GO TO 1
      ENDIF
C
C Fill VFIT banks
C
      DO IVERT = 1,NVERT
        CALL BKVFIT(LVFIT)
        IF (LVFIT.GT.0) THEN
          IQ(LVFIT+2) = ZENT(IVERT)
          Q(LVFIT+3) = BEAM_POS(1)
          Q(LVFIT+4) = BEAM_POS(2)
          Q(LVFIT+5) = ZVERT(IVERT)
          Q(LVFIT+6) = BEAM_ERR(1)
          Q(LVFIT+7) = BEAM_ERR(2)
          Q(LVFIT+8) = ZERR(IVERT)
          Q(LVFIT+9) = ZCHI(IVERT)
        ELSE
          CALL ERRMSG('NO BANK','ZVERTX_CONST',
     &                  'LVFIT=0, Z vertex not stored','W')
          GO TO 999
        ENDIF
      ENDDO
C
  999 RETURN
      END
