      SUBROUTINE D0HUPH(IFLAG)
C========================================================================
C
C  Description:  Plots updating histogram
C  ============  IFLAG=0 set up new plot, IFLAG=1, update old plot
C                IFLAG=-1 to stop updating histogram
C  Author:
C  ========
C  Sharon Hagopian
C
C  Revision History:
C  =================
C  Original Creation - February 13, 1990
C   based on D0HPID
C-   Updated  24-FEB-1990   Harrison B. Prosper
C-      Make default to run forever in updating plot
C-   Updated   8-AUG-1990   Chip Stewart  - added zoning
C-   Updated   8-JUL-1991   James Richardson
C-      fixed bug so that now it remembers in which hbook directory the
C-      histograms that are being update reside
C
C==========================================================================
C
      IMPLICIT NONE
C
C  Local Declarations:
C  ====================
C
      INTEGER IDNUM,IDARR(10),CONINT
      INTEGER IFLAG
      INTEGER DEVICE
      INTEGER trulen
      INTEGER NEVT,NMAX,NUPD
      INTEGER I,J,K,L,N,NX,NY,NEMPTY
      CHARACTER ELABEL*40,MSG*80,STRING*80
      CHARACTER*80 hbook_dirname        ! name of dir where hist are upd
      INTEGER len_hbook_dirname         ! len of dir name
      LOGICAL LEXIST,HEXIST,FIRST
      LOGICAL FLGVAL,HAH,REPLOT
      REAL MAXBIN(10),HMAX
      EXTERNAL FLGVAL
      DATA DEVICE/1/
      DATA FIRST/.TRUE./
      DATA NEVT,NUPD/0,0/
      SAVE hbook_dirname, len_hbook_dirname
C
C  Executable Code:
C  ================
C
C ****  stop if IFLAG=-1
C
      IF(IFLAG.LT.0)THEN
        CALL FLGSET('D0HAUT',.FALSE.)        ! STOP
        IF(FLGVAL('D0HUPH'))THEN
          CALL FLGSET('D0HUPH',.FALSE.)
          CALL INTMSG(' STOP UPDATING HIST')
          CALL HPLCOM(10.,15.,' STOP UPDATE')
          CALL HPLOPT('STA',1)      ! TURN ON STATISTICS
          CALL HPLZON(1,1,1,' ')
          CALL HPLSET('TSIZ',0.40  )
          CALL HPLSET('VSIZ',0.28  )
          CALL D0HEND
        ENDIF
        IFLAG=0
        GO TO 999
      ENDIF

C
C ****  initialize if IFLAG=0
C
      IF(IFLAG.EQ.0)THEN
        CALL VZERO(IDARR,10)
        NMAX  = 0
        CALL INTMSG(' UPDATING HISTOGRAMS ')
        CALL INTMSG
     &    (' RECOMMEND ENTERING 1,2,4,6, or 9 H IDs ')
        CALL GETPAR(1,' HISTOGRAM ID(s) to UPDATE >','C',
     &    STRING)
        CALL GETPAR
     &    (1,' NUMBER OF TIMES TO UPDATE ? [0--forever] > ','I',NMAX)
C
C ****  DETDERMINE THE NUMBER OF HISTOGRAM AND SET THE WINDOWS ACCORDINGLY
C
        N = 0
        L = LEN(STRING)
  100   CALL WORD(STRING,I,J,K)
        IF (J.LT.I) GOTO 105
        K = INDEX (STRING(I:J),',')
        IF (K.GT.0) THEN
          J = K - 1
          STRING(J+1:L) = STRING(J+2:L)
        END IF
        IF (J.LT.I) GOTO 105
        IDARR(N+1)=CONINT(STRING(I:J))
        IF(IDARR(N+1).GT.0 .and. IDARR(N+1).NE.9999999) THEN
          N = N + 1
          IF(J+1 .LT. L) THEN
            STRING = STRING(J+1:L)
            IF (N.LT.10) GOTO 100
          END IF
        END IF
  105   IF ( N.EQ.0) THEN
          CALL INTMSG(STRING)
          CALL INTMSG(' ID NOT ALLOWED - NO UPDATING HISTOGRAMS')
          J = 0
          GOTO 990
        END IF
        NX = NINT ( SQRT (FLOAT (N)) )
        NY = NINT ( SQRT (N-0.999999) + 0.501 )
        NEMPTY = NX*NY - N
        WRITE (MSG,110) N,NX,NY

  110   FORMAT(1X,I3,' UPDATING HISTOGRAMS  ZONED TO ',
     &    I3,' IN X AND ',I3,' IN Y')
        CALL INTMSG(MSG)
        IF (NEMPTY.GT.0) CALL INTMSG
     &    (' PLEASE IGNORE ERROR MESSAGES FROM IS* ROUTINES FOR NOW ')
C
C ****  INITIALIZE HPLOT & SOME COUNTERS
C
        IF ( NMAX .LE. 0 ) THEN
          NMAX = 99999999            ! Run "forever"
        ENDIF
        CALL D0HINT
        CALL HPLSET('TSIZ',0.40 / NX )
        CALL HPLSET('VSIZ',0.28 / NX )
        CALL HPLZON(NX,NY,1,' ')
        NEVT = 1
        CALL FLGSET('D0HAUT',.TRUE.)  ! START
        J = 0
C
C ****  LOOP OVER HISTOGRAMS
C
        CALL HPLOPT('STA',1)      ! TURN ON STATISTICS
        CALL hcdir(hbook_dirname,'R')   ! get hbook directory
        len_hbook_dirname = trulen(hbook_dirname)       ! and len of it
        DO I = 1, N
          IDNUM = IDARR(I)
          LEXIST = HEXIST(IDNUM)
          IF (LEXIST) THEN
            MAXBIN(I) = HMAX(IDNUM)
            J = J + 1
            CALL HPLOT(IDNUM,'HK','HIST',0)
          ELSE
            CALL HPLNUL
          END IF
        END DO
        IF (NEMPTY.GT.0) THEN
          DO I = 1, NEMPTY
            CALL HPLNUL
          END DO
        END IF
        IF (J.EQ.0) THEN
          CALL ERRMSG('NO PLOTS TO UPDATE','D0HUPH',
     &      'BACK TO MENU','W')
          GOTO 990
        END IF
        CALL HPLOPT('NSTA',1)      ! TURN OFF STATISTICS

      ELSE IF (IFLAG.EQ.1) THEN
        NEVT=NEVT+1
        CALL hcdir(hbook_dirname(1:len_hbook_dirname),' ')
        REPLOT =   MOD( (NEVT-1), 100 )  .EQ. 0
        IF (REPLOT) GOTO 108
        DO I = 1, N
          IF(HEXIST(IDARR(I) )) THEN
            IF ( HMAX(IDARR(I)) .GT. ( 3.0*MAXBIN(I) ) ) REPLOT = .TRUE.
          END IF
        END DO
  108   IF (REPLOT) THEN
          CALL HPLZON(NX,NY,1,' ')
          CALL HPLOPT('STA',1)      ! TURN ON STATISTICS
          DO I = 1, N
            IDNUM = IDARR(I)
            MAXBIN(I) = HMAX(IDNUM)
            LEXIST = HEXIST(IDNUM)
            IF (LEXIST) THEN
              CALL HPLOT(IDNUM,'HK','HIST',0)
            ELSE
              CALL HPLNUL
            END IF
          END DO
          IF (NEMPTY.GT.0) THEN
            DO I = 1, NEMPTY
              CALL HPLNUL
            END DO
          END IF
          CALL HPLOPT('NSTA',1)      ! TURN OFF STATISTICS
        ELSE
          CALL HPLZON(NX,NY,1,'S')
          DO I = 1, N
            IDNUM = IDARR(I)
            LEXIST = HEXIST(IDNUM)
            IF (LEXIST) THEN
              CALL HPLOT(IDNUM,'HU','HIST',0)
            ELSE
              CALL HPLNUL
            END IF
          END DO
          IF (NEMPTY.GT.0) THEN
            DO I = 1, NEMPTY
              CALL HPLNUL
            END DO
          END IF
        END IF
      ENDIF
      IF(NEVT.GT.NMAX)THEN
        CALL FLGSET('D0HUPH',.FALSE.)
        CALL FLGSET('D0HAUT',.FALSE.)        ! STOP
        CALL HPLCOM(10.,15.,' STOP UPDATE')
        CALL D0HEND
        CALL INTMSG(' STOP UPDATING HIST')
        IFLAG=0
      ENDIF
  990 CONTINUE
      IF( J.EQ.0) THEN
        CALL INTMSG(' HISTOGRAM DOES NOT EXIST')
        CALL FLGSET('D0HUPH',.FALSE.)
        CALL FLGSET('D0HAUT',.FALSE.)
      ENDIF
  999 RETURN
      END
