      SUBROUTINE ZVERTX_PEAK(HSTID,ILOW,IHI,ZPEAK,ILEAD,ITAIL,NEWHID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : find peak
C-
C-   Inputs  : HSTID: histogram id
C-             ILOW,IHI: region for peak finding
C-   Outputs : ZPEAK: peak position in Z
C-             ILEAD: the leading edge of the peak
C-             ITAIL: the tailing edge of the peak
C-             NEWHID: new histogram ID (ID=0, if no new histogram built)
C-
C-   Created   7-AUG-1991   Qizhong Li-Demarteau
C-   Updated   1-NOV-1991   Qizhong Li-Demarteau   handle flat cluster
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ILOW, IHI, ILEAD, ITAIL
      INTEGER I, IPEAK(10), IENTRY, HSTID, NEWHID
      INTEGER RUNSAV, IDSAV, RUN, ID, NENTRY
      INTEGER J, JJ, K, KK
      REAL    CONTEN(100), PEAK, ERROR, LEAD, TAIL, ZPEAK
      REAL    NEWHST(100), PEAK1, HSUM, ZPEAK1, ZPEAK2
      REAL    HSTATI
      LOGICAL CLUST1, MULTIP
      SAVE  RUNSAV, IDSAV
      DATA  RUNSAV,IDSAV/-1,-1/
C----------------------------------------------------------------------
C
      IF (HSTID .LE. 0) GOTO 999
C
      CALL EVNTID(RUN,ID)
      IF (RUN .NE. RUNSAV .OR. ID .NE. IDSAV) THEN
        RUNSAV = RUN
        IDSAV = ID
        CALL HUNPAK(HSTID,CONTEN,' ',1)
      ENDIF
      CLUST1 = .FALSE.
      CALL VZERO(IPEAK,10)
C
C  find the peak
C
      PEAK1 = 0.0
      J = 1
      MULTIP = .FALSE.
      DO 201 I = ILOW, IHI-1
        IF (CONTEN(I) .GT. PEAK1) THEN
          PEAK1 = CONTEN(I)
          IPEAK(1) = I
          CALL VZERO(IPEAK(2),9)
          J = 1
        ELSE
          IF (CONTEN(I) .GT. 0.0 .AND. CONTEN(I) .EQ. PEAK1) THEN
            J = J + 1
            IF (J .LE. 10) IPEAK(J) = I
          ENDIF
        ENDIF
  201 CONTINUE
      IF (PEAK1 .LE. 0.0) THEN
        NEWHID = 0
        ILEAD = 0
        ITAIL = 0
        ZPEAK = 9999.9
        GOTO 999
      ELSE
        IF (PEAK1 .EQ. 1.0) THEN
          CLUST1 = .TRUE.
        ELSE
          CALL HIX(1099,IPEAK(1),ZPEAK)
        ENDIF
      ENDIF
C
C determine the the peak region
C
      IF (CLUST1) THEN
        IF (ILOW .EQ. 1 .AND. IHI .EQ. 100) THEN
          CALL HNOENT(HSTID,NENTRY)	
          IF (NENTRY .EQ. 1) THEN
            ZPEAK = HSTATI(HSTID,1,'HIST',0)
            ILEAD = IPEAK(1)
            ITAIL = IPEAK(1) + 1
            NEWHID = -1 
            GOTO 999
          ENDIF
        ENDIF
        J = 0
        K = 0
        JJ = 1
        KK = 1
        DO 211 I = IPEAK(1), IHI
          IF (CONTEN(I) .GT. 0.0) THEN
            IF (J .EQ. 0) THEN
              J = I
            ELSE
              JJ = JJ + 1
            ENDIF
          ELSE
            IF (JJ .GT. KK) THEN
              KK = JJ
              K = J
            ENDIF
            J = 0
            JJ = 1
          ENDIF
  211   CONTINUE
        IF (KK .GT. 1) THEN
          KK = K + kk
          ILEAD = K 
          ITAIL = KK
          CALL HIX(1099,K,ZPEAK1)
          CALL HIX(1099,KK,ZPEAK2)
          ZPEAK = (ZPEAK1 + ZPEAK2) / 2.0
          NEWHID = -1
        ELSE
          NEWHID = 0
          ILEAD = ILOW
          ITAIL = IHI
          ZPEAK = 9999.9
        ENDIF
        GOTO 999
      ENDIF
C
      ILEAD = 1
      ITAIL = 100
      DO 202 I = IPEAK(1)+1, IHI-1
        IF (CONTEN(I) .EQ. 0.0 .AND. CONTEN(I+1) .EQ. 0.0) THEN
          ITAIL = I
          GOTO 205
        ENDIF
  202 CONTINUE
  205 DO 203 I = IPEAK(1)-1, ILOW+1, -1
        IF (CONTEN(I) .EQ. 0.0 .AND. CONTEN(I-1) .EQ. 0.0) THEN
          ILEAD = I + 1
          GOTO 204
        ENDIF
  203 CONTINUE
C
  204 CALL HIX(HSTID,ILEAD,LEAD)
      CALL HIX(HSTID,ITAIL,TAIL)
      CALL ZMKHST(LEAD,TAIL,NEWHID)
      MULTIP = .FALSE.
      DO 300 J = 2, 10
        IF (IPEAK(J) .LE. 0) GOTO 301
        IF (IPEAK(J) .GE. ILEAD .AND. IPEAK(J) .LE. ITAIL) THEN
          MULTIP = .TRUE.
          GOTO 301
        ENDIF
  300 CONTINUE
  301 IF (MULTIP) THEN
        ZPEAK = HSTATI(NEWHID,1,'HIST',0)
      ENDIF
C
  999 RETURN
      END
