      SUBROUTINE EZZRFM (BKNAME,LBANK,IZLINK)
      ENTRY RFSRCP (BKNAME,LBANK,IZLINK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert old SRCP bank to new format. Old
C-                         banks have CHRCRD characters/record and a max-
C-                         imum of 200 records/bank.
C-
C-                         NOTE: If the SRCP bank is part of a structure then
C-                               LBANK and IZLINK should contain, respectively,
C-                               the address of the support bank and the link
C-                               number from which the SRCP bank hangs;
C-                               otherwise LBANK is the address of the SRCP
C-                               bank and IZLINK = 0.
C-
C-   Inputs  : BKNAME           Bank name
C-             LBANK            If IZLINK = 0 this is address of SRCP bank
C-                              IF IZLINK > 0 this is address of support bank
C-   Outputs : None
C-   Controls: IZLINK           If non-zero it is taken to be the link
C-                              number from which the SRCP bank hangs
C-
C-   Created  20-NOV-1988   Harrison B. Prosper
C-   Updated  12-Feb-1992   Herbert Greenlee
C-      UNIX version
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LIDS,IPNT,LUN,I,L,LBANK,IZLINK
      CHARACTER*(*) BKNAME
C
      PARAMETER( LUN = 1 )
      CHARACTER*132 RECORD
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:NMSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      LOGICAL OK
C----------------------------------------------------------------------
C
C ****  Clear error flag
C
      ERRSRC = 0
C
C ****  Get address of SRCP bank
C
      IF ( IZLINK .GT. 0 ) THEN
        LSRCP = LC(LBANK-IZLINK)
      ELSE
        LSRCP = LBANK
      ENDIF
C
C ****  Write out OLD SRCP bank to disk
C
      LIDS = IC(LSRCP+1)
      IPNT  = LSRCP + IC(LSRCP+2)
C
      CALL D0OPEN (LUN,'EZZRFM.DAT','OF',OK)
      IF(.NOT.OK)CALL D0_ABORT(' D0OPEN failed')
      WRITE(LUN,90)
   90 FORMAT(1X,'\SIZE 200 200')
      DO 200 I = 1, LIDS
        CALL UHTOC (IC(IPNT),4,RECORD,CHRCRD)
        IPNT = IPNT + WRDCRD
        IF (RECORD(1:2) .EQ. 'C-' ) THEN
          RECORD(1:2) = '!-'
        ELSE
          RECORD = ' '//RECORD
        ENDIF
        WRITE(LUN,100) RECORD
  100   FORMAT(A)
  200 CONTINUE
      CLOSE(UNIT=LUN)
C
C ****  Drop OLD SRCP bank and read file into new SRCP bank
C
      CALL MZDROP (IXSTP,LSRCP,' ')
      L = LEN(BKNAME)
      CALL D0OPEN (LUN,'EZZRFM.DAT','I',OK)
      IF(.NOT.OK)CALL D0_ABORT(' D0OPEN failed')
      IF ( IZLINK .GT. 0 ) THEN
        CALL EZREAD (LUN,BKNAME(1:L),WRDCRD,LBANK,IZLINK)
      ELSE
        CALL EZREAD (LUN,BKNAME(1:L),WRDCRD,0,0)
      ENDIF
      CLOSE(UNIT=LUN)
C
  999 RETURN
      END
