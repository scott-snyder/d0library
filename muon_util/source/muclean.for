      SUBROUTINE MUCLEAN
C     ======================================
C     CLEANS TRACKS IN WAMUS-SAMUS OVERLAP REGION.
C     If WAMUS track goes through all three SAMUS stations
C     +100 is added to IFW4 word.
C     A.Kozelov 1-93
C     DIEHL 5/93 NO ACTION TAKEN ON SASBWC TRACKS
C     DH 11/93 dummy this routine for the time being
C     =================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INTEGER LMUOT,GZMUOT,IER,NTRACKS,IT
      INTEGER QUAD, ARM
      LOGICAL FIRST
      INTEGER IST,LSSTA,GZSSTA
      INTEGER IXY, LMUS
      REAL XY,XYC,XY1(2,6),XY2(2,6),Z0,TANG,Z(6)
      SAVE FIRST, LSSTA, XY1, XY2, Z
      DATA FIRST /.TRUE./

C      IF (FIRST) THEN                     ! Fill geometry arrays
C        FIRST = .FALSE.
C        DO IST = 1,6
C          LSSTA = GZSSTA(IST)
C          DO IXY = 1,2
C            XY1(IXY,IST) = C(LSSTA+IXY+9) - C(LSSTA+IXY+15)   
C            XY2(IXY,IST) = C(LSSTA+IXY+9) + C(LSSTA+IXY+15)
C            Z(IST) = C(LSSTA+12)
C          ENDDO ! IXY
C        ENDDO ! IST
C      ENDIF ! FIRST

C      CALL GTMTRH(NTRACKS)

C      DO IT = 1, NTRACKS

C        LMUOT = GZMUOT(IT)

C        QUAD = IQ(LMUOT+3)
C        IF (QUAD.LE.4) GOTO 400
C        IF (IQ(LMUOT+1).EQ.0) GOTO 400    ! Do not check SAMUS tracks
C        IF (BTEST(IQ(LMUOT+5),10)) GOTO 400 ! Do not check SASBWC tracks.
C        ARM = (QUAD-5)/4+1                ! 1-North , 2-South

C        DO IXY = 1,2

C          DO IST = ARM*3-2, ARM*3         ! Check all stations in arm

C            IF (IST.EQ.ARM*3-2) THEN
C              LMUS = LMUOT
C            ELSE
C              LMUS = LMUOT + 3
C            ENDIF

C            Z0 = Q(LMUS+10)
C            IF (Q(LMUS+16).EQ.0.) GOTO 200
C            TANG = Q(LMUS+IXY+13) / Q(LMUS+16)
C            XY = Q(LMUS+IXY+7)  +  (Z(IST)-Z0) * TANG
C            IF ( XY.LE.XY1(IXY,IST) .OR. XY.GT.XY2(IXY,IST) ) GOTO 400

C          ENDDO ! IST

C        ENDDO ! IXY

C  200   IQ(LMUOT+7) = IQ(LMUOT+7) + 100     ! Mark track (IFW4)

C  400 ENDDO ! IT

  999 RETURN
      END
