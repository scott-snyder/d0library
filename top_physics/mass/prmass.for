      SUBROUTINE PRMASS ( PRUNIT, LMASS, NMASS, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'MASS'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LMASS  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NMASS  [I] : Bank number, used only if CFL='ONE' and LMASS = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LMASS point to a bank, or if <0, NMASS is
C-                                  the bank number.
C-                          'LINEAR' : LMASS points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  22-JUN-1993 10:51:16.15  Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PRUNIT, LMASS, NMASS, IFL
      CHARACTER*(*) CFL
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMASS.LINK'
C----------------------------------------------------------------------
      INTEGER LMASS1, GZMASS, LZLOC, J
      CHARACTER*8 CFLAG
C----------------------------------------------------------------------
      CFLAG  = CFL(1:LEN(CFL))
      LMASS1 = LMASS
C
      IF    ( CFLAG .EQ. 'LINEAR' ) THEN
        IF( LMASS .LE. 0 ) GOTO 990
      ELSEIF( CFLAG .EQ. 'ONE' ) THEN
        IF( LMASS .LE. 0 ) THEN
          IF( NMASS .EQ. 0 ) GOTO 980          ! Error exit
          LMASS1 = LZLOC( IXMAIN, 'MASS', NMASS )
        ENDIF
      ELSEIF((CFLAG .EQ. 'ALL') .OR.
     &       (CFLAG .EQ. ' '  ) ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LMASS1 = GZMASS()
      ELSE
        WRITE( PRUNIT, 1000 ) CFLAG
 1000   FORMAT(/' ** PRMASS ** Illegal value of CFL = ',a/)
        GOTO 999
      ENDIF
C
C ****  Check address
C
      IF ( LMASS1 .LE. 0 ) THEN
        WRITE( PRUNIT, 1005 )
 1005   FORMAT(/' No MASS bank FOUND')
        GOTO 999
      ENDIF
C
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LMASS1
C
      WRITE( PRUNIT, 1100 )  IQ(LMASS1+1),
     & ( Q( LMASS1 + J ) , J = 2, IQ( LMASS1-1) )
 1100 FORMAT(/' Printout of MASS bank ',//,
     & ' VERSION    Bank Version Number (=1) ',I    ,/,
     & '    Low end  of top mass range ',F   ,/,
     & '    High end of top mass range ',F   ,//,
     & '    Most likely top mass RR method ICMB=1 ',F   ,/,
     & '    Maximum likelihood RR method ICMB=1 ',E12.3   ,/,
     & '    90% Lower end CL RR method ICMB=1 ',F   ,/,
     & '    90% Upper end CL RR method  ICMB=1 ',F   ,//,
     & '    Most likely top mass DG method ICMB=1 ',F   ,/,
     & '    Maximum likelihood DG method ICMB=1 ',F   ,/,
     & '    90% Lower end CL DG method ICMB=1 ',F   ,/,
     & '    90% Upper end CL DG method  ICMB=1 ',F   ,/,
     & '    turn on mass for ICMB=1 ',F   ,//,
     & '    Most likely top mass RR method ICMB=2 ',F   ,/,
     & '    Maximum likelihood RR method ICMB=2 ',E12.3   ,/,
     & '    90% Lower end CL RR method ICMB=2 ',F   ,/,
     & '    90% Upper end CL RR method  ICMB=2 ',F   ,//,
     & '    Most likely top mass DG method ICMB=2 ',F   ,/,
     & '    Maximum likelihood DG method ICMB=2 ',F   ,/,
     & '    90% Lower end CL DG method ICMB=2 ',F   ,/,
     & '    90% Upper end CL DG method  ICMB=2 ',F   ,/,
     & '    turn on mass for ICMB=2 ',F   ,//,
     & ' 4 Vectors of 3 High Et jets(see note 2)'  ,/, 3(4F/),/,
     & '    Combination number ',F   ,/,
     & '    Btag flag  ',F   ,/,
     & '    Number of btags ',F   ,/,
     & '    Name tag for 1st btag ',A4   ,/,
     & '    difference in R(ISAJ-JET) 1st tag ',F   ,/,
     & '    difference in ET(ISAJ-JET) 1st tag ',F   ,/,
     & '    Name tag for 2nd btag ',A4   ,/,
     & '    difference in R(ISAJ-JET) 2nd tag ',F   ,/,
     & '    difference in ET(ISAJ-JET) 2nd tag ',F  ,/,
     & '    Number of configurations generated ',F )
C
C  ***  Look if another bank is needed
C
      IF( CFLAG .EQ. 'ONE' ) GOTO 999
C
      IF( CFLAG .EQ. 'LINEAR' ) THEN
        LMASS1 = LQ( LMASS1 )
        IF( LMASS1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command.
C
        LMASS1 = GZMASS()
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 CONTINUE
      WRITE( PRUNIT, 2000 ) LMASS
 2000 FORMAT(/' ** PRMASS ** called for LINEAR without valid bank '
     &        'pointer, LMASS =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 CONTINUE
      WRITE( PRUNIT, 2100 ) LMASS, NMASS
 2100 FORMAT(/'  ** PRMASS ** called for ONE without bank pointer and '
     &        'bank number, LMASS =',I10,' NMASS =', I10/)
      GOTO 999
      END
