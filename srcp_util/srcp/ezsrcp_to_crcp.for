      SUBROUTINE EZSRCP_TO_CRCP(LSRCP,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert SRCP at LSRCP to CRCP bank
C-
C-   Inputs  : LSRCP address of SRCP bank to convert
C-   Outputs : IER - IER=0 OK
C-   Controls: 
C-
C-   Created  22-JUL-1992   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LSRCP,IER
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER I,J,K,LSUP,IL,NL,LBANK,ID
      CHARACTER BANK*4,BKNAME*80,MSG*80
C----------------------------------------------------------------------
C
      CALL DHTOC (4,IC(LSRCP-4),BANK)
      IF ( BANK .NE.'SRCP' ) THEN         !Not SRCP
        IER = -1
        GOTO 999
      END IF
      LSUP = LC(LSRCP+1) 
      NL = IC(LSUP-2)                     !Find link LSRCP hangs from
      DO 10, IL = 1, NL
        LBANK = LC(LSUP-IL)
        CALL DHTOC(4,IC(LSUP-4),BANK)
        IF(LBANK.EQ.LSRCP) GOTO 20
   10 CONTINUE
      IER = -1
      GOTO 999
   20 CONTINUE
      CALL EZGETNAME(LSRCP,BKNAME)        !Get RCP name (\START)
      CALL WORD(BKNAME,I,J,K)
      IF(BKNAME(I:J).EQ.'SCPH') K = 0
      IF(K.EQ.0) THEN
        WRITE(BKNAME,'(I2.2)')IL          
        BKNAME = BANK//BKNAME(1:2)
        CALL WORD(BKNAME,I,J,K)
      END IF
      MSG = ' CONVERT SRCP BANK '//BKNAME(I:J)//' TO CRCP '
      CALL INTMSG(MSG)
      CALL EZNAME (BKNAME,LSRCP,ID)      !Declare to SRCP
      CALL EZMOVE_OVERWRITE
      CALL EZPICK (BKNAME)               !Pick SRCP bank
      CALL EZMOVE (BKNAME,LSUP,IL)       !Book CRCP bank in it's spot
      LSRCP = LC(LSUP-IL)
      CALL EZDROP (BKNAME)               !Drop SRCP bank (keep CRCP)
      CALL EZRSET                        !un Pick SRCP bank
  999 RETURN
      END
