      SUBROUTINE MSRHFL(LMSRH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank MSRH
C-
C-   Inputs  :LMSRH = link of bank to be filled.
C-            LMSRH < 0, routine will get link using GZMSRH
C-            LMSRH = 0, routine will book bank.
C-
C-   Outputs :
C-   Controls:
C-
C-   Created  03-Aug-1990   S.T.Repond
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZMSRH.LINK'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER LMSRH, GZMSRH,IDUMMY
      INTEGER IVSN,WRMSRH
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
C do initialization here if necessary.
        FIRST = .FALSE.
      ENDIF

C
      IDUMMY=0
CCCC      IF(LMSRH.LT.0)
      LMSRH = GZMSRH(IDUMMY)    ! GET LINK.
      WRITE(*,*)'*MSRHFL*  LMSRH=',LMSRH
C   3 AUG 90
C
      IF(LMSRH.EQ.0)CALL BKMSRH(LMSRH)
C Book the bank if argument = 0.   !
C
C  If WRMSRH from SRCP is 1, either create or re write MSRH
C  Book the bank even if it already exists, with a different version #
      IF(WRMSRH.EQ.1) THEN
        IVSN = IC(LMSRH+1)
        IVSN = IVSN+1
      ENDIF
C  FILL MSRH BANK
      IC(LMSRH+1) = IVSN               ! Bank version
      IC(LMSRH+2) = 1                     !Status
      IC(LMSRH+3) = 0                    !Quality
      IC(LMSRH+4) = 0                    !Low_Run #
      IC(LMSRH+5) = 0                    !High_Run #
      IC(LMSRH+6) = 0                    !Generated Run
      IC(LMSRH+7) = 900803               !Generated Date
      IC(LMSRH+8) = 0                    !Generated Type of Run
      IC(LMSRH+9) = 0                    !spare
      IC(LMSRH+10) = 0                   !spare
C----------------------------------------------------------------------
      WRITE(*,*)' *MSRHFL*  msrh bank filled'
  999 RETURN
      END
