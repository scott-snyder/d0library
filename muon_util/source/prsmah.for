      SUBROUTINE PRSMAH ( PRUNIT, LSMAH, NSMAH, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print the content of SMAH bank
C-
C-   Inputs  : PRUNIT - Unit number for printout
C-             LSMAH - Bank address
C-             NSMAH - Bank number
C-             CFL - Flag to control printout
C-             IFL - Amount of printing 
C-   Outputs : on unit PRUNIT
C-   Controls: 
C-
C-   Created  02-MAY-1991   Andrei Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER PRUNIT,LSMAH,NSMAH,IFL
      CHARACTER*(*) CFL
      CHARACTER*4 NAME
      INTEGER I
      CHARACTER*25 TITLE(10)
      DATA TITLE/'Type',
     &           'Status',
     &           'Quality',
     &           'Lowest run number',
     &           'Highest run number',
     &           'Run when generated',
     &           'Date generated',
     &           'Generated for type of run',
     &           'Spare',
     &           'Spare'/
C----------------------------------------------------------------------
C
      IF (LSMAH.EQ.0) GOTO 990
C
C ****  Print bank
C
      CALL UHTOC (IC(LSMAH-4),4,NAME,4)
      WRITE (PRUNIT,1000) NAME
 1000 FORMAT (1H1/25X,30(1H*)/25X,'*  Contents of Bank :  ',A4,'  *'/
     &25X,30(1H*)//)
      DO 10 I=1,10
        IF (I.NE.3) THEN
          WRITE (PRUNIT,1010) I,TITLE(I),IC(LSMAH+I)
 1010     FORMAT (1X,I3,2H. ,A25,2H :,I10)
        ELSE
          WRITE (PRUNIT,1020) I,TITLE(I),C(LSMAH+I)
 1020     FORMAT (1X,I3,2H. ,A25,2H :,F10.5)
        ENDIF
  10  CONTINUE
      GOTO 999
C
C ****  Error: bank is absent
C
  990 WRITE (PRUNIT,1030)
 1030 FORMAT (1H1/1X,'*** PRSMAH: bank SMAH is absent'/)
C
  999 RETURN
      END
