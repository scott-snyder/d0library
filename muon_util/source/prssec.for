      SUBROUTINE PRSSEC ( PRUNIT, LSSEC, NSSEC, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print the content of SSEC bank(s)
C-
C-   Inputs  : PRUNIT - Unit number for printout
C-             LSSEC  - Address of the bank if CFL='ONE'
C-             NSSEC  - Bank number
C-             CFL    - Flag to control printout ('ONE' only)
C-             IFL    - Amount of printing: 0 means full printout,
C-   Outputs : on unit PRUNIT
C-   Controls:
C-
C-   Created  02-MAY-1991   Andrei Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER PRUNIT,LSSEC,NSSEC,IFL
      CHARACTER*(*) CFL
      CHARACTER*4 NAME
      INTEGER I, LSSEC1, NVERT
      CHARACTER*53 TITLE(11)
      DATA TITLE/
     &'Section number                                       ',
     &'Section type ( 1 - X, 2 - Y, 3 - U )                 ',
     &'Phill Martin number                                  ',
     &'Number of channels in the section                    ',
     &'Shift for extra tubes in the C stations              ',
     &'X coord. of the first tube from center of station    ',
     &'Y coord. of the first tube from center of station    ',
     &'Z coord. of the section center from center of station',
     &'Step of tubes                                        ',
     &'Layer number for the first tube                      ',
     &'Number of vertices for tubes positioning             '/
C----------------------------------------------------------------------
C
      LSSEC1=LSSEC
      IF ( CFL.EQ.'ONE' ) THEN
        IF (LSSEC1.EQ.0) GOTO 990
      ELSE
        WRITE (PRUNIT,1000) CFL
 1000   FORMAT (1H1/1X,'*** PRSSEC: illegal value of CFL = ',A/)
        GOTO 999
      ENDIF
C
C ****  Print bank
C
      CALL UHTOC (IC(LSSEC1-4),4,NAME,4)
      WRITE (PRUNIT,1010) NAME
 1010 FORMAT (1H1/25X,30(1H*)/25X,'*  Contents of Bank :  ',A4,'  *'/
     &25X,30(1H*)//)
      DO 50 I=1,11
        IF (I.LT.6.OR.I.GT.8) THEN
          WRITE (PRUNIT,1020) I,TITLE(I),IC(LSSEC1+I)
 1020     FORMAT (1X,I4,2H. ,A45,2H :,I10)
        ELSE
          WRITE (PRUNIT,1030) I,TITLE(I),C(LSSEC1+I)
 1030     FORMAT (1X,I4,2H. ,A45,2H :,F10.4)
        ENDIF
   50 CONTINUE
C
C ****  Print vertices corrdinates
C
      NVERT=IC(LSSEC1+11)
      DO 60 I=1,NVERT
        WRITE (PRUNIT,1050) I,C(LSSEC1+10+2*I),C(LSSEC1+11+2*I)
 1050   FORMAT (1X,6X,'Vertex #',I2,' with X =',F10.5,' & Y =',F10.5)
   60 CONTINUE
C
      GOTO 999
C
C ****  Errors: bank is absent
C
  990 WRITE (PRUNIT,1060)
 1060 FORMAT (1H1/1X,'*** PRSSEC: bank SSEC is absent'/)
C
  999 RETURN
      END
