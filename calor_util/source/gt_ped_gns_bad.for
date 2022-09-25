      SUBROUTINE GT_PED_GNS_BAD(TASK,ISCL,ICRATE,NBAD,IBAD,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack the cal calib bad channel bank (crate)
C-
C-   Inputs  : TASK - 1 for peds, 3 for gains
C-             ISCL - 0 for x8, 1 for x1
C-             ICRATE - ADC crate number
C-             
C-   Outputs : NBAD - total number of bad channels for this crate, scale 
C-                    and calibrarion type
C-             IBAD - array containing bad channel flags for all channels 
C-                    of that ADC crate, chan 1-4608.
C-             IER  - error code, 0 =ok
C-             
C-   Controls: none
C-
C-   Created   4-MAR-1993   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCPD8.LINK'
      INCLUDE 'D0$LINKS:IZCPD1.LINK'
      INCLUDE 'D0$LINKS:IZCGN8.LINK'
      INCLUDE 'D0$LINKS:IZCGN1.LINK'
      INCLUDE 'D0$LINKS:IZCPB8.LINK'
      INCLUDE 'D0$LINKS:IZCPB1.LINK'
      INCLUDE 'D0$LINKS:IZCGB8.LINK'
      INCLUDE 'D0$LINKS:IZCGB1.LINK'
      CHARACTER*64 STRING
      INTEGER TASK,NBAD,IBAD(4608),IER
      INTEGER ICRATE,IADC,IBLS,ITWR,IDEP,ISCL,NEG,IWRD
      INTEGER IMASK,NTOTBD,ICHAN,I
      INTEGER GZCPDH,GZCGNH,LZFIND,LINKH
      DATA IMASK/z'FFFF'/
C----------------------------------------------------------------------
      IER = -1
      NBAD = 0
      CALL VZERO(IBAD,4608)
C
      IF (TASK.LT.3) THEN                   ! Pedestals
        LCPDH = GZCPDH()
        LCPDH = LZFIND(IDVSTP,LCPDH,ICRATE,9)   !Finds Bank with Crate
        IF (LCPDH.LE.0) THEN
          WRITE(STRING,50) ICRATE
   50     FORMAT(' Pedestal header bank does not exist for crate ',I2)
          CALL ERRMSG(' DBCALIB-BAD','GT_PED_GNS_BAD',STRING,'W')
          GO TO 999
        ENDIF
        IF (ISCL.EQ.0) THEN                    ! x8 peds
          LINKH = LC(LCPDH-IZCPD8)
          IF (LINKH.LE.0) THEN
            WRITE(STRING,51) ICRATE
   51       FORMAT(' Bank CPD8 does not exist for crate ',I2)
            CALL ERRMSG(' DBCALIB-BAD','GT_PED_GNS_BAD',STRING,'W')
            GO TO 999
          ENDIF
          LINKH = LC(LINKH-IZCPB8)
          IF (LINKH.LE.0) THEN
            WRITE(STRING,52) ICRATE
   52       FORMAT(' Bank CPB8 does not exist for crate ',I2)
            CALL ERRMSG(' DBCALIB-BAD','GT_PED_GNS_BAD',STRING,'W')
            GO TO 999
          ENDIF
        ELSE                                  ! x1 peds
          LINKH = LC(LCPDH-IZCPD1)
          IF (LINKH.LE.0) THEN
            WRITE(STRING,53) ICRATE
   53       FORMAT(' Bank CPD1 does not exist for crate ',I2)
            CALL ERRMSG(' DBCALIB-BAD','GT_PED_GNS_BAD',STRING,'W')
            GO TO 999
          ENDIF
          LINKH = LC(LINKH-IZCPB1)
          IF (LINKH.LE.0) THEN
            WRITE(STRING,54) ICRATE
   54       FORMAT(' Bank CPB1 does not exist for crate ',I2)
            CALL ERRMSG(' DBCALIB-BAD','GT_PED_GNS_BAD',STRING,'W')
            GO TO 999
          ENDIF
        ENDIF
      ELSE                                  ! Gains
        LCGNH = GZCGNH()
        LCGNH = LZFIND(IDVSTP,LCGNH,ICRATE,9)   !Finds Bank with Crate
        IF (LCGNH.LE.0) THEN
          WRITE(STRING,55) ICRATE
   55     FORMAT(' Gains header bank does not exist for crate ',I2)
          CALL ERRMSG(' DBCALIB-BAD','GT_PED_GNS_BAD',STRING,'W')
          GO TO 999
        ENDIF
        IF (ISCL.EQ.0) THEN                    ! x8 peds
          LINKH = LC(LCGNH-IZCGN8)
          IF (LINKH.LE.0) THEN
            WRITE(STRING,56) ICRATE
   56       FORMAT(' Bank CGN8 does not exist for crate ',I2)
            CALL ERRMSG(' DBCALIB-BAD','GT_PED_GNS_BAD',STRING,'W')
            GO TO 999
          ENDIF
          LINKH = LC(LINKH-IZCGB8)
          IF (LINKH.LE.0) THEN
            WRITE(STRING,57) ICRATE
   57       FORMAT(' Bank CGB8 does not exist for crate ',I2)
            CALL ERRMSG(' DBCALIB-BAD','GT_PED_GNS_BAD',STRING,'W')
            GO TO 999
          ENDIF
        ELSE                                  ! x1 peds
          LINKH = LC(LCGNH-IZCGN1)
          IF (LINKH.LE.0) THEN
            WRITE(STRING,58) ICRATE
   58       FORMAT(' Bank CGD1 does not exist for crate ',I2)
            CALL ERRMSG(' DBCALIB-BAD','GT_PED_GNS_BAD',STRING,'W')
            GO TO 999
          ENDIF
          LINKH = LC(LINKH-IZCGB1)
          IF (LINKH.LE.0) THEN
            WRITE(STRING,59) ICRATE
   59       FORMAT(' Bank CGB1 does not exist for crate ',I2)
            CALL ERRMSG(' DBCALIB-BAD','GT_PED_GNS_BAD',STRING,'W')
            GO TO 999
          ENDIF
        ENDIF
      ENDIF
C
        NTOTBD = IC(LINKH+1)
        IF ( NTOTBD.LE.0 ) GOTO 999
        DO I = 1, NTOTBD
          IWRD = IC(LINKH+I+1)
          NBAD = NBAD + 1
          CALL CADUPK(ICRATE,IWRD,ICRATE,IADC,IBLS,ITWR,IDEP,ISCL,NEG)
          ICHAN = 384*IADC + 48*IBLS + 12*ITWR + IDEP + 1
          IBAD(ICHAN) = IAND(IC(LINKH+I+1),IMASK)
        ENDDO
C
        IER = 0
  999   RETURN
        END
