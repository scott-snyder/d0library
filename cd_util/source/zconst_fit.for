      SUBROUTINE ZCONST_FIT(LEAD,TAIL,NTRACK,ZV,EZV,PCHISQ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Perform a z-vertex fit constraining all tracks to
C-                         originate from a single vertex point
C-
C-   Inputs  : LEAD   =  Lower z-window. Z(tracks) > lead to be accepted in fit
C-             TAIL   =  Upper z-windoe. Z(tracks) < tail to be accepted in fit
C-   Outputs : NTRACK =  Number of tracks in fit
C-             Z      =  Z vertex (cm)
C-             EZ     =  Error in z
C-             PCHISQ =  Global chisquare of fit
C-   Controls: none
C-
C-   Created  23-AUG-1993   Srini Rajagopalan
C-   Updated   2-DEC-1993   Srini Rajagopalan  Fix bug in storing fit-coeffs.
C-   Updated   6-DEC-1993   Srini Rajagopalan  Replace call to ZVFIT with
C-                          ZVCFIT - conflict with common block name
C-   Updated  31-JAN-1994   Srini Rajagopalan  Set reject flag = -2 for tracks
C-                          where too few exist to make a const. fit. 
C-   Updated   1-MAR-1994   Srini Rajagopalan  Insert diferent error flags
C-                          (-1 to -4) for different track failures.
C-   Updated  12-JUL-1995   Srini Rajagopalan  Fetch BEAM_POS every new run 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:ZVFIT.INC'
C
      INTEGER NTRACK,MIN_TRACK,MAX_DL
      PARAMETER (MAX_DL=8)
C
      INTEGER IER,JHIT,LDTRK,GZDTRK,LZFIND,ITRK,NITER
      INTEGER NDOF,NZHIT,IREJ,MIN_DLHIT,MAX_ITER
      INTEGER RUN,RUNNO,RUNSAV
      DATA RUNSAV /-1/
C
      REAL CHISQ,MAX_CHISQ,WT
      REAL THETA,LEAD,TAIL
      REAL ZV,EZV,TZV,TEZV
      REAL XVERT,YVERT,BEAM_POS(3),TANPHI,IMPXY
      REAL TCHISQ,PCHISQ,MIN_CHISQ
      REAL MAX_DIFF,BXYLMT
      REAL    R(MAX_DL),Z(MAX_DL),EZ(MAX_DL)
      REAL X0,Y0,R0
      REAL CPID(MAX_TRACK)
C
      LOGICAL FIRST,DO_VTX_ITER
      DATA FIRST /.TRUE./
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VERTEX_RCP')
        CALL EZGET('DO_VTX_ITER',DO_VTX_ITER,IER)
        IF (IER.EQ.0) CALL EZGET('MAX_VTX_CHISQ_DIFF',MAX_DIFF,IER)
        IF (IER.EQ.0) CALL EZGET('BXYLMT',BXYLMT,IER)
        IF (IER.EQ.0) CALL EZGET('MAX_VTX_ITER',MAX_ITER,IER)
        IF (IER.EQ.0) CALL EZGET('MIN_VTX_TRACKS',MIN_TRACK,IER)
        IF (IER.EQ.0) CALL EZGET('MAX_VTX_XYCHISQ',MAX_CHISQ,IER)
        IF (IER.EQ.0) CALL EZGET('MIN_VTX_DLHIT',MIN_DLHIT,IER)
        IF (IER.EQ.0) CALL EZGET('MIN_VTX_CHISQ',MIN_CHISQ,IER)
        IF (IER.NE.0)
     &     CALL ERRMSG('RCP PARAMETER NOT FOUND','ZCONST_FIT',' ','F')
        CALL EZRSET
      ENDIF
C
      RUN = RUNNO()
      IF (RUN.NE.RUNSAV) THEN
        RUNSAV = RUN
        CALL EZPICK('VERTEX_RCP')
        CALL EZGET('BEAM_POS',BEAM_POS,IER)
        CALL EZRSET
        XVERT = BEAM_POS(1)
        YVERT = BEAM_POS(2)
      ENDIF
C
      ITRK = 1
      NTRACK = 0
C
      CALL VZERO(SXY(1),MAX_TRACK)
      CALL VZERO(SXX(1),MAX_TRACK)
      CALL VZERO(SX(1),MAX_TRACK)
      CALL VZERO(SY(1),MAX_TRACK)
      CALL VZERO(SWT(1),MAX_TRACK)
C
    1 LDTRK = GZDTRK(ITRK)
      IF (LDTRK.LE.0) GO TO 200
C
C If too many tracks, do fit with only MAX_NTRACKS.
C
      IF (NTRACK.GE.MAX_TRACK) THEN
        CALL ERRMSG('Too many tracks','ZCONST_FIT',
     &      'Fit made with MAX_TRACKS','I')
        GO TO 10
      ENDIF
C
      IF (IQ(LDTRK+15).NE.0) GO TO 10  ! Track has been previously used
C
C Check if this track belongs to the selected vertex cluster
C
      X0 = Q(LDTRK + 7)
      Y0 = Q(LDTRK + 8)
      THETA = Q(LDTRK+9)
      IF (THETA.EQ.0.0) GO TO 10       ! Should not happen for CDC track
      R0 = SQRT((X0-XVERT)**2 + (Y0-YVERT)**2)
C
      ZV = Q(LDTRK+11) - R0 / TAN(THETA)
      IF (ZV.LT.LEAD .OR. ZV.GT.TAIL) GO TO 10
C
C
C Check x-y impact parameter of this track. If it is too large, skip track
C
      TANPHI = TAN(Q(LDTRK+6))
      IMPXY = ABS(XVERT*TANPHI - YVERT + Y0 - X0*TANPHI) /
     &        SQRT(TANPHI**2 + 1)
      IF (IMPXY.GT.BXYLMT) THEN
        IQ(LDTRK+15) = -1
        GO TO 10
      ENDIF
C
C
C Check Chi-Square of track (xy fit)
C
      CHISQ = 0.
      NDOF = IQ(LDTRK+14)
      IF (NDOF.GT.0) CHISQ = Q(LDTRK+12)/FLOAT(NDOF)
      IF (CHISQ.GT.MAX_CHISQ) THEN
        IQ(LDTRK+15) = -2
        GO TO 10
      ENDIF
C
C Fetch delay line contents for this track from DTRK bank
C
      CALL DLINFO(LDTRK,NZHIT,R,Z,EZ,IER)
      IF (IER.NE.0 .OR. NZHIT.LT.MIN_DLHIT) THEN
        IQ(LDTRK+15) = -3
        GO TO 10
      ENDIF
C
C Passed all cuts, this track may be used for z fit. store track id.
C
      NTRACK = NTRACK + 1
      PID(NTRACK) = IQ(LDTRK-5)
C
C Calculate the necessary coefficients for this track
C Also store Track ID number for later refernce.
C
      DO JHIT = 1,MAX_DL
        IF (EZ(JHIT).NE.0.0) THEN
          WT = EZ(JHIT)
C
          SXY(NTRACK) = Z(JHIT)*R(JHIT)*WT + SXY(NTRACK)
C
          SXX(NTRACK) = R(JHIT)*R(JHIT)*WT + SXX(NTRACK)
C
          SX(NTRACK) = R(JHIT)*WT + SX(NTRACK)
C
          SWT(NTRACK) = SWT(NTRACK) + WT
C
          SY(NTRACK) = Z(JHIT)*WT + SY(NTRACK)
        ENDIF
      ENDDO
C
   10 ITRK = ITRK + 1
      GO TO 1
C-----------------------------------------------------------------------
  200 CONTINUE
C
      IF (NTRACK.LT.MIN_TRACK) THEN
        CALL ERRMSG('Too few tracks','ZCONST_FIT',
     &    'Too few tracks to perform fit','I')
        DO ITRK = 1,NTRACK
          LDTRK = LZFIND(IXCOM,GZDTRK(1),PID(ITRK),-5)
          IQ(LDTRK+15) = -4
        ENDDO
        NTRACK = 0
        GO TO 999
      ENDIF
C
C Perform global fit with all tracks (first pass)
C
      NITER = 1
      CALL ZVCFIT(NTRACK,ZV,EZV,TCHISQ)
      IF (TCHISQ.LT.MIN_CHISQ) THEN
        PCHISQ = TCHISQ
        CALL UCOPY(TSLOPE(1),SLOPE(1),NTRACK)
        CALL UCOPY(TERR_SLOPE(1),ERR_SLOPE(1),NTRACK)
        CALL UCOPY(TERR_INT(1),ERR_INT(1),NTRACK)
        GO TO 999
      ENDIF
C
C Perform global fit again excluding one track at a time
C
      IF (DO_VTX_ITER) THEN
  201   CONTINUE
        IF (TCHISQ.LT.MIN_CHISQ) GO TO 999            ! Stop if chisq is small
        IF (NTRACK.LE.MIN_TRACK) GO TO 999
        IF (NITER.GE.MAX_ITER) GO TO 999
C
        IREJ = 0
        PCHISQ = 99999.
        NITER = NITER + 1
        DO ITRK = 1,NTRACK
          INDEX(ITRK) = 1             ! Set bit to ignore track in fit
          CALL ZVCFIT(NTRACK,TZV,TEZV,CHISQ)
          INDEX(ITRK) = 0             ! Reset bit
           IF (CHISQ.LT.PCHISQ) THEN
            PCHISQ = CHISQ
            ZV = TZV
            EZV = TEZV
            IREJ = ITRK
            CALL UCOPY(TPID(1),CPID(1),NTRACK-1)
            CALL UCOPY(TSLOPE(1),SLOPE(1),NTRACK-1)
            CALL UCOPY(TERR_SLOPE(1),ERR_SLOPE(1),NTRACK-1)
            CALL UCOPY(TERR_INT(1),ERR_INT(1),NTRACK-1)
          ENDIF
        ENDDO
C
C Remove the track which contributes to the largest chi**2
C If significant change has not occurred in chi**2 >>> return
C
        NTRACK = NTRACK - 1
        CALL UCOPY(CPID(1),PID(1),NTRACK)
        IF (TCHISQ-PCHISQ.GE.MAX_DIFF) THEN
          TCHISQ = PCHISQ
          CALL UCOPY(SXY(IREJ+1),SXY(IREJ),NTRACK+1-IREJ)
          CALL UCOPY(SXX(IREJ+1),SXX(IREJ),NTRACK+1-IREJ)
          CALL UCOPY(SWT(IREJ+1),SWT(IREJ),NTRACK+1-IREJ)
          CALL UCOPY(SX(IREJ+1) ,SX(IREJ) ,NTRACK+1-IREJ)
          CALL UCOPY(SY(IREJ+1) ,SY(IREJ) ,NTRACK+1-IREJ)
          GO TO 201 
        ENDIF
      ELSE
        PCHISQ = TCHISQ
      ENDIF
C
  999 RETURN
      END
