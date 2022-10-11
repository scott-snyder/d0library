      SUBROUTINE ISAINFO(SKIP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Manipulate ISAJET information
C-
C-   Inputs  : None
C-   Outputs : SKIP - When .TRUE., further processing is skipped for this event
C-   Controls:
C-
C-   Created  30-AUG-1992   Guilherme Lima
C-   Updated  13-OCT-1992   Kamel A. Bazizi   Add OTC stuff
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      LOGICAL SKIP,GOODCCT
      INTEGER NPART,ID(100),ID2(100),IMU
      REAL PX(100),PY(100),PZ(100),PA(100),PT(100),
     X   PHI(100),TH(100),ETA(100),X(100),Y(100),Z(100)
      REAL THMIN,THMAX,PHIMIN,PHIMAX,PTMIN,VSAVE(20),PMIN
      INTEGER I,J,IOCT,TRREG,ISALAY,NCA,NCB,NCC
      INTEGER IER,LISAE,IERP,IBIT,L1_BITS(16)
      LOGICAL IPRISA
      CHARACTER*80 STRING
      LOGICAL FIRST /.TRUE./

C----------------------------------------------------------------------
C.. If not ISAJET data, returns
      LISAE=LQ(LHEAD-IZISAE)
      IF(LISAE.EQ.0) GOTO 999

      IF(FIRST) THEN
        FIRST=.FALSE.

C.. Read in RCP switch
        CALL EZPICK('MUSIM_RCP')
        CALL EZERR(IERP)     ! Check if error
        IF(IERP.EQ.0)THEN
          CALL EZGET_l('IPR_ISA',IPRISA,IER)
          CALL EZGET('TH_MIN',THMIN,IER)
          CALL EZGET('TH_MAX',THMAX,IER)
          CALL EZGET('PHI_MIN',PHIMIN,IER)
          CALL EZGET('PHI_MAX',PHIMAX,IER)
          CALL EZGET('PT_MIN',PTMIN,IER)
          CALL EZGET('P_MIN',PMIN,IER)
          CALL EZRSET()
	ELSE
           CALL EZGET_ERROR_TEXT(IER,STRING)
           CALL ERRMSG(' CANNOT PICK MUSIM.RCP',
     &          'IAINFO',STRING,'F')
           GOTO 999
	ENDIF
      ENDIF

  100 SKIP=.FALSE.

C.. Find prompt muons in isajet block
      CALL GISAMU(NPART,ID,ID2,PX,PY,PZ,PA,PHI,TH,ETA,X,Y,Z)

      CALL HFILL(730,FLOAT(NPART),0.,1.)
      IF(IPRISA) WRITE(6,20) NPART
   20 FORMAT(' # OF PROMPT MUONS= ',I3)

      IF(NPART.LE.0) GOTO 999
      DO  J=1,NPART
        PT(J) = PA(J)*SIN(TH(J))
        TH(J) = TH(J)/RADIAN
        PHI(J)= PHI(J)/RADIAN
        IF(ABS(ID(J)).EQ.14) THEN 

        IF(IPRISA) WRITE(6,11) ID(J),ID2(J),PX(J),PY(J),PZ(J)
     &        ,PA(J),PT(J),TH(J),PHI(J),ETA(J)
   11   FORMAT(1X,'ID/PID       =',2I4,3X,/,
     &         1X,'PX/PY/PZ/P/PT=',5(F6.1,1X),/,
     &         1X,'TH/PH/ETA    =',3(F6.1,1X),3X)

        IF(NPART.EQ.1) THEN
          IF( TH(J).LT.THMIN .OR. TH(J).GT.THMAX) SKIP=.TRUE.
          IF( PHI(J).LT.PHIMIN .OR. PHI(J).GT.PHIMAX) SKIP=.TRUE.
          IF(PT(J).LT.PTMIN)SKIP=.TRUE.
          IF(ABS(PA(J)).LT.PMIN)SKIP=.TRUE.
        ENDIF
        ENDIF
      END DO
  99  CONTINUE

  999 RETURN
C----------------------------------------------------------------------
C.. Entry point to fill HBOOK objects with ISAJET information

      ENTRY ISA_FILL_L1


          GOODCCT=.FALSE.
          CALL MU_L1_RAW_BITS(L1_BITS)

          DO IBIT=1,16
            IF(L1_BITS(IBIT).ne.0) GOODCCT=.TRUE.
          ENDDO

C- fill ISAJET info
      IF(NPART.GE.1) THEN
        DO J=1,NPART

          CALL HFILL(3001,TH(J),0.,1.)
          CALL HFILL(3002,PHI(J),0.,1.)
          CALL HFILL(3003,ABS(PA(J)),0.,1.)
          CALL HFILL(3004,ABS(PT(J)),0.,1.)
          CALL HFILL(3005,TH(J),PHI(J),1.)
          CALL HFILL(3006,ETA(J),0.,1.)
          CALL HFILL(3007,ETA(J),PHI(J),1.)

C- fill info for good CCT triggers

          IF (GOODCCT) THEN
            CALL HFILL(3101,TH(J),0.,1.)
            CALL HFILL(3102,PHI(J),0.,1.)
            CALL HFILL(3103,abs(PA(J)),0.,1.)
            CALL HFILL(3104,abs(PT(J)),0.,1.)
            CALL HFILL(3105,TH(J),PHI(J),1.)
            CALL HFILL(3106,ETA(J),0.,1.)
            CALL HFILL(3107,ETA(J),PHI(J),1.)
          ENDIF

          CALL MU_TRIG_REGION(TH(J)*RADIAN,PHI(J)*RADIAN,IOCT,TRREG)

          IF(IOCT.NE.-1) CALL HFILL(3015,FLOAT(IOCT),0.,1.)
        ENDDO

      ENDIF

	RETURN
      END
