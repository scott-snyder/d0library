      SUBROUTINE GT_PED_GNS_ADDR(TASK,CRATE,CARD,SEQ,SCALE,
     &  VALUE,SIGMA,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns Values and Sigmas for PED/GAINS
C-
C-   Inputs  : TASK =1,2 PEDS, 2=PEDS (same as 1 but SINGLE DIGITIZED X1 & X8), 
C-                  =3 GAINS
C-             CRATE = ADC CRATE
C-             SEQ = ADC work sequential number
C-             SCALE = 0 X8 Gain. =1 X1 Gain
C
C-   OUTPUTS : VALUE Value for channels
C-             SIGMA FOR channel
C-             IER   error code 0=OK
C-                   -1=bad ped bank
C-
C-   Created   23-MAR-1990   Chip Stewart , Dharmaratna
C-   Updated  13-NOV-1990   Jan Guida  Added CRATE argument to GT_PED_GNS 
C-   Updated  13-NOV-1993   Jan Guida  Add DATA statement for O_CRATE (FLINT) 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$LINKS:IZCPDH.LINK'
      INCLUDE 'D0$LINKS:IZCGNH.LINK'
      INCLUDE 'D0$LINKS:IZCPD1.LINK'
      INCLUDE 'D0$LINKS:IZCPD8.LINK'
      INCLUDE 'D0$LINKS:IZCGN1.LINK'
      INCLUDE 'D0$LINKS:IZCGN8.LINK'
      INTEGER PDGN_LINK(2,0:NADCRC-1,0:NADCC-1,3,0:1)
      INTEGER TASK,SCALE,CRATE,CARD,IER,SEQ,O_CRATE,NCAD,NCRATE
      INTEGER GZCPDH,GZCGNH,LZFIND,LINKH,LINK,ICAD,ICRT,IADC,IT,IS
      REAL VALUE,SIGMA
      CHARACTER MSG*40
      LOGICAL FIRST
      SAVE O_CRATE
      DATA O_CRATE/0/
C----------------------------------------------------------------------
      IER = 0
C
      IF ( CRATE .NE. O_CRATE) THEN
        O_CRATE = CRATE
        NCAD = MOD(CRATE,10) - 6
        NCRATE = CRATE/10
        IF(CRATE.EQ.87) THEN !TB90 load 2
          NCRATE = 1
        ELSE IF (NCAD.LT.1 .OR. NCAD.GT.2 .OR. 
     &    NCRATE.LT.0 .OR. NCRATE.GE.NADCRC) THEN
          WRITE(MSG,'('' CRATE '',I10)')CRATE
          CALL ERRMSG(' BAD CRATE ','GT_PED_GNS_ADDR',MSG,'W')
          IER = -7
          GOTO 999
        END IF
      END IF
      IF( CARD.LT.0 .OR. CARD.GE.NADCC) THEN
        WRITE(MSG,'('' CARD '',I10)')CARD
        CALL ERRMSG(' BAD CARD','GT_PED_GNS_ADDR',MSG,'W')
        IER = -7
        GOTO 999
      ELSE IF(TASK.LT.1 .OR. TASK.GT.3) THEN
        WRITE(MSG,'('' TASK '',I10)')TASK
        CALL ERRMSG(' BAD TASK','GT_PED_GNS_ADDR',MSG,'W')
        IER = -7
        GOTO 999
      ELSE IF(SCALE.LT.0 .OR. SCALE.GT.1) THEN
        WRITE(MSG,'('' SCALE '',I10)')SCALE
        CALL ERRMSG(' BAD SCALE','GT_PED_GNS_ADDR',MSG,'W')
        IER = -7
        GOTO 999
      END IF
      IF (PDGN_LINK(NCAD,NCRATE,CARD,TASK,SCALE).EQ.0) THEN
         IF(TASK.LT.3) THEN
           LCPDH = GZCPDH ()
           LCPDH = LZFIND(IDVSTP,LCPDH,CRATE,9)   !Finds Bank with Crate
           IF (LCPDH.EQ.0) THEN
             WRITE(MSG,10)CRATE
   10        FORMAT(' No pedestal bank for crate ',I3)
             CALL ERRMSG(' NO CPDH BANK','GT_PED_GNS_ADDR',
     &          MSG,'W')
             IER = -5
             PDGN_LINK(NCAD,NCRATE,CARD,TASK,SCALE) = -1
             GOTO 999
           ELSE
             IF ( SCALE.EQ.0 ) THEN             !X8 gains
               LINKH = LC(LCPDH-IZCPD8)
             ELSE IF ( SCALE.EQ.1 ) THEN        !X1 gains
               LINKH = LC(LCPDH-IZCPD1)
             ENDIF
           END IF
         ELSE IF(TASK.EQ.3) THEN
           LCGNH = GZCGNH ()
           LCGNH = LZFIND(IDVSTP,LCGNH,CRATE,9)   !Finds Bank with Crate
           IF (LCGNH.EQ.0) THEN
             WRITE(MSG,12)CRATE
   12        FORMAT(' No gains bank for crate ',I3)
             CALL ERRMSG(' NO CGNH BANK','GT_PED_GNS_ADDR',
     &          MSG,'W')
             IER = -5
             PDGN_LINK(NCAD,NCRATE,CARD,TASK,SCALE) = -1
             GOTO 999
           ELSE
             IF ( SCALE.EQ.0 ) THEN             !X8 gains
               LINKH = LC(LCGNH-IZCGN8)
             ELSE IF ( SCALE.EQ.1 ) THEN        !X1 gains
               LINKH = LC(LCGNH-IZCGN1)
             ENDIF
           END IF
         END IF
         LINK  = LZFIND(IDVSTP,LINKH,CARD,11)   !Finds Bank with Card
         IF (LINK.LE.0) THEN
           WRITE(MSG,14)Card,TASK,SCALE
   14      FORMAT(' CRATE TASK SCALE',3I5)
           CALL ERRMSG(' NO BANK','GT_PED_GNS_ADDR',MSG,'W')
           IER = -5
           PDGN_LINK(NCAD,NCRATE,CARD,TASK,SCALE) = -1
           GOTO 999
         END IF
         CALL STP_GSLINK('GT_PED_GNS_ADDR',
     &     PDGN_LINK(NCAD,NCRATE,CARD,TASK,SCALE) )
         STP_LSLINK(PDGN_LINK(NCAD,NCRATE,CARD,TASK,SCALE) ) = LINK
      END IF
      IF ( PDGN_LINK(NCAD,NCRATE,CARD,TASK,SCALE).LT.0) THEN
        IER = -1
        GOTO 999
      END IF
      VALUE = C(STP_LSLINK(PDGN_LINK(NCAD,NCRATE,CARD,TASK,SCALE))
     &    +30+2*SEQ+1)
      SIGMA = C(STP_LSLINK(PDGN_LINK(NCAD,NCRATE,CARD,TASK,SCALE))
     &    +30+2*SEQ+2)
  999 RETURN
      ENTRY GT_PED_GNS_ADDR_RESET
      O_CRATE = 0
      DO 1000, ICAD = 1, 2
        DO 1000,ICRT = 0,NADCRC-1
          DO 1000, IADC = 0,NADCC-1
            DO 1000, IT = 1,3
              DO 1000, IS = 0,1
                IF ( PDGN_LINK(ICAD,ICRT,IADC,IT,IS).NE.0) THEN
                  CALL STP_RSLINK('GT_PED_GNS_ADDR',
     &            PDGN_LINK(ICAD,ICRT,IADC,IT,IS) )
                  PDGN_LINK(ICAD,ICRT,IADC,IT,IS)  = 0
                END IF
 1000 CONTINUE
      END
