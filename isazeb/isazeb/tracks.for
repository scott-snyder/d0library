	PROGRAM TRACKS
C-----------------------------------------------------------------
C-
C-        CREATE A FILE OF TRACKS FOR DEBUGGING
C-
C-         SDP Sept., 1985
C-
C------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:IDRUN.INC'
      INCLUDE 'D0$INC:FIXPAR.INC'
      INCLUDE 'D0$INC:SGNPAR.INC'
      INCLUDE 'D0$INC:ITAPES.INC'
      INCLUDE 'D0$INC:JETLIM.INC'
      INCLUDE 'D0$INC:JETSET.INC'
      INCLUDE 'D0$INC:KEYS.INC'
      INCLUDE 'D0$INC:XKEYS.INC'
      INCLUDE 'D0$INC:PARTCL.INC'
      INCLUDE 'D0$INC:PRIMAR.INC'
      INCLUDE 'D0$INC:TOTALS.INC'
      INCLUDE 'D0$INC:ISABNK.INC'
      INCLUDE 'D0$INC:ISAUNT.INC'
C
      REAL XYZ(3)
      INTEGER I,NREAC
      CHARACTER*1 YN
      LOGICAL ONETRK
C
      ISUNIT=10
      FILISA='TRACKS'
      BANK='ISAP'
      CALL MZEBRA(0)
      CALL ISAZEB('O')
C
C  Fake data for beginning and end records
C 
      IDVER=0
      NREAC=0
      NEVENT=1
      ECM=0
      CALL VZERO(PMIN,36)
      SUMWT=.001
      NKINPT=1
      IEVT=1
      NJET=0
      NJSET=0
      DO 6 I=1,6
    6 KEYS(I)=.FALSE.
      KEYS(4)=.TRUE.
      ITCOM=0 
      CALL ISAWBG
      NPTCL=0
      CALL ISBKST(.TRUE.,.FALSE.,.TRUE.)
   10 CONTINUE
      IF(ONETRK()) GOTO 10
      PRINT*,' Give x,y,z for primary vertex'
      READ *,XYZ
      CALL SETVTX(XYZ)
      PRINT*,' How many identical events do you wish?'
      READ *,NEVENT
      DO 11 IEVT=1,NEVENT
   11 CALL ISAWEV
      NPTCL=0
      PRINT *,' Do you wish to generate another set? (Y/N)'
      READ 111,YN
  111 FORMAT(A)
      IF(YN.EQ.'Y') GOTO 10
      CALL ISAWND
      PRINT *,' '
      PRINT *,' Events are on file TRACKS.DAT'
	STOP
	END
