      SUBROUTINE PRSSTA ( PRUNIT, LSSTA, NSSTA, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print the content of SSTA bank(s)
C-
C-   Inputs  : PRUNIT - Unit number for printout
C-             LSSTA  - Address of the bank if CFL='ONE' 
C-             NSSTA  - Bank number
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
      INTEGER PRUNIT,LSSTA,NSSTA,IFL
      CHARACTER*(*) CFL
      CHARACTER*4 NAME
      INTEGER I, LSSTA1
      CHARACTER*45 TITLE(18)
      DATA TITLE/'SAMUS drift tube station number              ',  
     &           'SAMUS station orientation (+1 or -1)         ',  
     &           'Name of station volume                       ',  
     &           'Shape of station volume                      ',  
     &           'GEANT tracking medium number                 ',  
     &           'Mother volume under which this hangs         ',  
     &           'Keyword to position (e.g POS)                ',  
     &           'Rotation matrix number                       ',  
     &           'Copy number of this volume                   ',  
     &           'X position of station centre in Mother volume',  
     &           'Y position of station centre in Mother volume',  
     &           'Z position of station centre in Mother volume',  
     &           'X half size of the SAMUS station             ',  
     &           'Y half size of the SAMUS station             ',  
     &           'Z half size of the SAMUS station             ',  
     &           'X half size of the SAMUS station hole        ',  
     &           'Y half size of the SAMUS station hole        ',  
     &           'Z half size of the SAMUS station hole        '/  
C----------------------------------------------------------------------
C
      LSSTA1=LSSTA
      IF ( CFL.EQ.'ONE' ) THEN
        IF (LSSTA1.EQ.0) GOTO 990
      ELSE
        WRITE (PRUNIT,1000) CFL
 1000   FORMAT (1H1/1X,'*** PRSSTA: illegal value of CFL = ',A/)
        GOTO 999
      ENDIF
C
C ****  Print bank
C
      CALL UHTOC (IC(LSSTA1-4),4,NAME,4)
      WRITE (PRUNIT,1010) NAME
 1010 FORMAT (1H1/25X,30(1H*)/25X,'*  Contents of Bank :  ',A4,'  *'/
     &25X,30(1H*)//)
      DO 50 I=1,18
        IF (I.GE.10) THEN
          WRITE (PRUNIT,1020) I,TITLE(I),C(LSSTA1+I)
 1020     FORMAT (1X,I4,2H. ,A45,2H :,F10.4)
        ELSEIF (I.EQ.3.OR.I.EQ.4.OR.I.EQ.6.OR.I.EQ.7) THEN
          WRITE (PRUNIT,1030) I,TITLE(I),IC(LSSTA1+I)
 1030     FORMAT (1X,I4,2H. ,A45,2H :,6X,A4)
        ELSE
          WRITE (PRUNIT,1040) I,TITLE(I),IC(LSSTA1+I)
 1040     FORMAT (1X,I4,2H. ,A45,2H :,I10)
        ENDIF
   50 CONTINUE
      GOTO 999
C
C ****  Errors: bank is absent
C
  990 WRITE (PRUNIT,1060)
 1060 FORMAT (1H1/1X,'*** PRSSTA: bank SSTA is absent'/)
C
  999 RETURN
      END
