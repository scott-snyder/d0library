      SUBROUTINE DZSURV_ZEBCOM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : call dzsurv and dzveri for zebcom and for zebstp
C                     another pair of entries call dzshow
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  25-MAR-1992   James T. Linnemann
C-   Modified 26-MAR-1992   A. Boehnlein, Added close/open of FOR003.DAT
C-   Updated   3-APR-1992   James T. Linnemann  add DZSHOW entries
C-   Updated  19-AUG-1992   sss - make MESSAGE really 80 chars (for ibm)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:ZUNIT.INC'
      INTEGER ILINK1,ILINK2,IDAT1,IDAT2
      CHARACTER*40 VAR
      CHARACTER*80 MESSAGE
      CHARACTER*10 VERIF
      CHARACTER*20 TYPE
      LOGICAL OK
      DATA MESSAGE/
     &    ' You can now type FOR003.DAT in another window, <cr> to close
     &  file and continue'/
      DATA ILINK1,ILINK2,IDAT1,IDAT2/4*0/
C----------------------------------------------------------------------
C&IF VAXELN
C&C...set Zebra Log file temporarily to console
C&      IQPRNT = 6
C&ENDIF
      CALL DZSURV('ZEBCOM SURVEY',IXCOM,LHEAD)
      TYPE = ' ZEBCOM VERIFY'
      CALL DZVERI(TYPE,IXCOM,'CFLSU')
      GO TO 999
C<<
      ENTRY DZSURV_ZEBSTP
C&IF VAXELN
C&C...set Zebra Log file temporarily to console
C&      IQPRNT = 6
C&ENDIF
      CALL DZSURV('ZEBSTP SURVEY',IXSTP,LSTPH)
      TYPE = ' ZEBSTP VERIFY'
      CALL DZVERI(TYPE,IXSTP,'CFLSU')
      GO TO 999
C<<
      ENTRY DZSHOW_ZEBCOM
C&IF VAXELN
C&C...set Zebra Log file temporarily to console
C&      IQPRNT = 6
C&ENDIF
      CALL DZSHOW(
     $   'ZEBCOM dump',IXCOM,LHEAD,'BLV',ILINK1,ILINK2,IDAT1,IDAT2)
      GO TO 999
C<<
      ENTRY DZSHOW_ZEBSTP
C&IF VAXELN
C&C...set Zebra Log file temporarily to console
C&      IQPRNT = 6
C&ENDIF
      CALL DZSHOW(
     $   'ZEBSTP dump',IXSTP,LSTPH,'BLV',ILINK1,ILINK2,idat1,idat2)
C<<
  999 CONTINUE
C&IF VAXELN
C&C...now reset unit to 3
C&      IQPRNT = 3
C&ELSE
      CLOSE(UNIT=3,STATUS='KEEP')   !Close FOR003.DAT
      IF(IQUEST(1).EQ.0) THEN
        VERIF = ' GOOD'
      ELSE
        WRITE(VERIF,50)IQUEST(1)
   50   FORMAT(' BAD: ',I3)
      ENDIF
      WRITE(VAR,100)TYPE//VERIF
  100 FORMAT(A)
      CALL INTMSG(VAR)
      CALL INTMSG(MESSAGE)
      READ (5,*)
      CALL D0OPEN(3,'FOR003.DAT','A',OK)  !Reopen FOR003.DAT
C&ENDIF
      RETURN
      END
