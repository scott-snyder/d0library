      SUBROUTINE CAHITS_ERRMSG(ERROR_TYPE,IETA,IPHI,ILYR,SCALE,
     &  CRATE,IDATA,MSG,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : BOOK_KEEP CAHITS CELL CALIBRATION ERRORS
C-   
C-
C-   Inputs  : ERROR_TYPE           [I] =1: BAD PEDESTAL (KILL CELL)
C-                                      =2: BAD GAIN     (KILL CELL)
C-                                      =3: BAD GAIN     (IGNORE CALIB GAIN)
C-                                      =4: KILL CELL FROM RCP 
C-                                      =5: KEEP CELL FROM RCP 
C-                                      =6: HOT CELL     (KILL CELL)
C-             IETA,IPHI,ILYR,SCALE [I] CELL ADDRESS IN PHYSICS COORDINATES
C-             CRATE                [I] CRATE OF CHANNEL 
C-             IDATA                [I] WORD IN BAD CHANNEL BANK 
C-                                      BITS 31-16 - CAD ADDRESS
C-                                      BITS 15-0  - BAD CHANNEL BITS
C-             MSG                  [C] TEXT DESCRIBING PROBLEM AND ACTION
C-             
C-   Outputs : IER                  [I] ERROR IN CAHITS_ERRMSG ROUTINE
C-                                      =0 OK
C-                                      =-1 TOO MANY ERRORS 
C-   Controls: CAHITS_RCP
C-
C-   Created  12-JUN-1992   Chip Stewart
C-   Updated  26-JAN-1993   Joan Guida  add "hot" channel flag
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER ERROR_TYPE,IETA,IPHI,ILYR,SCALE,IDATA,CRATE
      CHARACTER*(*) MSG
      INTEGER IER
      INTEGER I,NTYPE
      PARAMETER( NTYPE = 6 )
      INTEGER LUN,LENF,NBAD(NTYPE),RUNNO
      INTEGER IBAD(NTYPE),ERROR_TYPEP,IETAP,IPHIP,ILYRP,SCALEP
      CHARACTER LOGFILE*132,FILE*132,ACTION(NTYPE)*11,CBAD(NTYPE)*20
      LOGICAL FIRST,LFILE,OK
      SAVE FIRST
      DATA FIRST /.TRUE./
      DATA ACTION/'PED_KILL','GAIN_KILL','GAIN_IGNORE',
     &  'RCP_KILL','RCP_KEEP','HOT_KILL'/
      DATA NBAD/NTYPE*0/,ERROR_TYPEP/0/
      DATA IETAP/0/,IPHIP/0/,ILYRP/0/,SCALEP/0/
C-    IF NEEDED TO BOOK_KEEP LIST WITH NO DOUBLE COUNTING 
C-    USE PTCAEP IF ONLY AT INITIALZATION TIME
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST = .FALSE.
C
C ****  FETCH LOG FILE NAME  CONSTANTS FROM CAHITS_RCP
C
        LFILE = .FALSE.
        CALL EZPICK('CAHITS_RCP')
        CALL EZERR(IER)
        IF ( IER.EQ.0) THEN
          CALL EZGETS('CAHITS_ERROR_LOG',1,LOGFILE,LENF,IER)
          CALL UPCASE(LOGFILE,FILE)
          LOGFILE = FILE(1:LENF)
          IF((IER.NE.0) .OR. (LENF.EQ.0) ) THEN
            CALL ERRMSG(' NO_CAHITS_ERROR_LOG_IN_RCP','CAHITS_ERRMSG',
     &        ' PROBLEM CELLS NOT RECORDED','W')
          ELSE IF(LOGFILE(1:6).EQ.'SCREEN') THEN
            LUN = 6
            LFILE = .TRUE.
          ELSE
            CALL GTUNIT(77,LUN,IER)
            CALL D0OPEN(LUN,LOGFILE,'OF',OK)
            IF(OK) LFILE = .TRUE.
          END IF
          IF(LFILE) THEN
            WRITE(LUN,'(/'' RUN '',I9.7)')RUNNO()
            WRITE(LUN,101)'N','ETA','PHI','LYR','S',
     &      'CR','WORD','ACTION','MSG'
  101       FORMAT(1X,A5,3A4,A2,A3,A12,A12,1X,A4)
          END IF
        END IF
        CALL EZRSET
      END IF
      IER = 0
      IF((ERROR_TYPE.GT.NTYPE).OR.(ERROR_TYPE.LE.0)) THEN
        IER = -1
        GOTO 999
      END IF
      IF(LFILE) THEN
        WRITE(LUN,100)NBAD(ERROR_TYPE)+1,IETA,IPHI,ILYR,SCALE,
     &    CRATE,IDATA,ACTION(ERROR_TYPE),MSG(1:30)
  100   FORMAT(1X,I5,3I4,I2,I3,Z12.9,A12,1X,A30)
      END IF
      NBAD(ERROR_TYPE)  = NBAD(ERROR_TYPE) + 1
C
C ****  IF PREVIOUS CALL HAD ERROR ON SAME CHANNEL THEN UNDO IT
C
      IF((IETA.EQ.IETAP).AND.(IPHI.EQ.IPHIP).AND.(ILYR.EQ.ILYRP)
     &  .AND.(SCALEP.EQ.SCALE)) THEN
        NBAD(ERROR_TYPEP)  = NBAD(ERROR_TYPEP) - 1
      END IF
      ERROR_TYPEP = ERROR_TYPE
      IETAP = IETA
      IPHIP = IPHI
      ILYRP = ILYR
      SCALEP = SCALE
  999 RETURN
C#######################################################################
      ENTRY CAHITS_ERRMSG_SUM(IBAD,CBAD,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  12-JUN-1992   Chip Stewart
C-
C----------------------------------------------------------------------
      IER  = 0
      CALL UCOPY(NBAD(1),IBAD(1),NTYPE)
      DO I = 1, NTYPE
        CBAD(I) = ACTION(I)
      END DO
 1999 RETURN
C#######################################################################
      ENTRY CAHITS_ERRMSG_RESET
      IF(LFILE) THEN
        WRITE(LUN,'(/'' RUN '',I9.7)')RUNNO()
        WRITE(LUN,101)'N','ETA','PHI','LYR','S',
     &      'CR','WORD','ACTION','MSG'
        CALL VZERO(NBAD,NTYPE)
      END IF
      END
