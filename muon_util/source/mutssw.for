      SUBROUTINE MUTSSW (IQUAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : IQUAD 15: North
C-                   16: South
C-   Outputs : 
C-   Controls: 
C-
C-   Created  20-JUN-1994   Joao R.T. de Mello Neto
C-   Modified 11-JUL-1994   Andre Sznajder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER  LMTRH,LSAMT
      INTEGER  GZMTRH,GZSAMT
      INTEGER  DIR,IQUAD,IST,LST,LTRG1,NTRG1
      INTEGER  NTRG,NTRK,KEYTRG,IERR,SSWITCH
      LOGICAL  FIRST,OK
      CHARACTER*4 MODE
      DATA FIRST /.TRUE./
C
C ****  initializing
C
      NTRG=0
      LTRG1=0
      NTRK=0
      KEYTRG=0
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('SAMUS_UTIL_PARAM')
        CALL EZGET ('NTRG1', NTRG1, IERR)  ! Max SAMUS station triplets.
        CALL EZRSET
        CALL EZPICK ('SSW_UTIL_PARAM')
        CALL EZGET ('SSWITCH', SSWITCH, IERR)  ! Switch of the SSW code
        CALL EZRSET                            ! 1-> ON ; 0-> OFF
        CALL PATHGT(MODE) ! If L2 then mode='filt'
      ENDIF
C
C **** SSW code switch
C
      IF (SSWITCH.EQ.0) GOTO 999
C
C ****  Creation RECO banks
C
      LMTRH = GZMTRH(0)
      IF (LMTRH.GT.0) THEN
        LSAMT = GZSAMT()
        IF (LSAMT .EQ. 0) THEN
          CALL BKSAMT (LSAMT)
          CALL SAHTFL (OK, 0)
          IF (.NOT. OK) THEN                   ! no hits
            CALL MZDROP (IXCOM, LSAMT, ' ')
            GO TO 999
          END IF
        END IF
      ELSE
        CALL ERRMSG ('NO LMTRH BANK !!!','MUTSSW',' ','F')
        GO TO 999
      ENDIF
C
C ****  direction loop
C      
      IF (IQUAD .EQ. 15 ) THEN
        DIR=1
      ELSE 
        DIR=2
      ENDIF
C
C ****  check Level 1 trigger ( just in SAMUS A and B )
C
      LST = 3 * DIR - 2
      DO IST = LST, LST+1
        CALL SATG1A (IST, LTRG1)
        IF ((LTRG1.LE.0).OR.(LTRG1.GE.NTRG1)) GO TO 999
      END DO
C
C ****  Decides between L2 trigger and full tracks reconstruction
C
      IF (MODE.EQ.'FILT') THEN
        CALL SSWTL2(DIR,NTRG)
      ELSE
        CALL SSWTRK(DIR,NTRK)
      ENDIF
*
  999 CONTINUE
      RETURN
      END
