      SUBROUTINE PRESUM ( PRUNIT, LESUMIN, NESUM, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'ESUM'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LESUMIN [I]: Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NESUM  [I] : Bank number, used only if CFL='ONE' and LESUMIN = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LESUMIN point to a bank, or if <0, NESUM is
C-                                  the bank number.
C-                          'LINEAR' : LESUMIN points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-      Call ESUM_INIT to get names and resolutions
C-
C-   Created  10-DEC-1991   Richard V. Astur
C-   Updated  11-DEC-1991   James T. Linnemann  add counts; touchup
C-   Updated   6-JAN-1992   James T. Linnemann  do linear chain; rename ESUM
C-   Updated  16-Mar-1992   Herbert Greenlee
C-      Removed machine block
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZESUM.LINK'
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      CHARACTER*10 OBNAME( ID_ALL : LAST_TYPE )
      REAL OBRES( ID_ALL : LAST_TYPE ), OBTHRESH( ID_ALL : LAST_TYPE )
      INTEGER ID, J,K,POINT
      INTEGER NR,NFIX,NUM_OBJ, IFOUND
C
      INTEGER PRUNIT, LESUMIN, NESUM, IFL
      CHARACTER*(*) CFL
      CHARACTER*4 STYP  ! bank type
      INTEGER LESUM, GZESUM, LZLOC, TRULEN
C----------------------------------------------------------------------
C: Get object type names and resolutions
      CALL ESUM_INIT( OBRES, OBTHRESH, OBNAME  )
      LESUM = LESUMIN
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LESUMIN .LE. 0 ) GOTO 990 ! error exit
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LESUMIN .LE. 0 ) THEN
          IF( NESUM .EQ. 0 ) GOTO 980          ! Error exit
          LESUM = LZLOC( IXMAIN, 'ESUM', NESUM )
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LESUM = GZESUM('ANY')
        IF( LESUM .LE. 0 ) GOTO 990 ! error exit
      ELSE
        WRITE( PRUNIT, 100 ) CFL
  100   FORMAT(/' ** PRESUM ** Illegal value of CFL = ',A/)
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LESUM
C
      WRITE(PRUNIT,*)'0********** DUMPING ESUM BANK ****************'
      NUM_OBJ = IQ (LESUM + 4)
      NFIX=IQ(LESUM+2)
      CALL UHTOC(IQ(LESUM+NFIX),4,STYP,4)
      WRITE(PRUNIT,105) NUM_OBJ, STYP
  105 FORMAT(/1X,I5,' Objects found in ',A4,' summary :'/)
C: Dump one object at a time
      DO ID = 0 , LAST_TYPE
        IF (IQ(LESUM +5 +ID).NE.0) THEN
          IF (IFL.EQ.1) THEN  ! dump contents of objects, not just counts
            WRITE (PRUNIT, 110) OBNAME (ID), IQ(LESUM +5 +ID)
  110       FORMAT(' ',A10,': ',I3,' FOUND')
          ELSE
            IF( ID.EQ.ID_VERTEX) THEN
              WRITE( PRUNIT, 115 ) OBNAME (ID)
  115         FORMAT(' ',A10,': ',
     &          ' #      ET      X      Y         Z       FLAG   ')
            ELSE
              WRITE( PRUNIT, 120 ) OBNAME (ID)
  120         FORMAT(' ',A10,': ',
     &          ' #      ET   ETA-PHY  ETA-DET   PHI      FLAG   ')
            ENDIF
            NFIX = IQ( LESUM + 2)  !0th word of 1st repeating group
            NR = IQ( LESUM + 3 )
            IFOUND = 0
            DO J = 1, NUM_OBJ
              POINT = (J-1)*NR + NFIX + LESUM
              IF ( ID .EQ. IQ( POINT + 1 ) )  THEN
                IFOUND = IFOUND + 1
                WRITE(PRUNIT,125) IFOUND,(Q(POINT+K),K=3,NR),IQ(POINT+2)
  125           FORMAT(' ',11X,I3,F8.2,1X,3(F8.2),2x,Z10.9 )
              ENDIF
            END DO
          ENDIF
        ENDIF
      END DO
C
C  ***  Look if another bank is needed
C
      WRITE(PRUNIT,*)' '
      IF( CFL .EQ. 'ONE' ) GOTO 999
      IF( CFL .EQ. 'LINEAR' ) THEN
        LESUM = LQ( LESUM )
        IF( LESUM .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command.
C
        LESUM = LQ( LESUM )   ! treat ALL same as LINEAR: search linear chain
        IF( LESUM .NE. 0 ) GOTO 1
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LESUMIN
 2000 FORMAT(/' ** PRESUM ** called for LINEAR without valid bank ',
     &        'pointer, LESUMIN =',I10/)
      GOTO 999
C
C  *** Error : ALL without bank pointer
C
  995 WRITE( PRUNIT, 2005 ) LESUM
 2005 FORMAT(/' ** PRESUM ** called for LINEAR without valid bank ',
     &        'pointer, LESUM =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LESUMIN, NESUM
 2100 FORMAT(/'  ** PRESUM ** called for ONE without bank pointer and',
     &        ' bank number, LESUMIN =',I10,' NESUM =', I10/)
      GOTO 999
      END
