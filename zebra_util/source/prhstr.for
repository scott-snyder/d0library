      SUBROUTINE PRHSTR ( PRUNIT, LHSTR, NHSTR, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'HSTR'. 
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LHSTR  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NHSTR  [I] : Bank number, used only if CFL='ONE' and LHSTR = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LHSTR point to a bank, or if <0, NHSTR is
C-                                  the bank number.
C-                          'LINEAR' : LHSTR points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  Stephen Adler July 10, 1990
C-   Updated  29-APR-1992   James T. Linnemann  make it pass d0flavor 
C-   Updated  20-AUG-1992   sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZHSTR.LINK'
C
      INTEGER PRUNIT, LHSTR, NHSTR, IFL
      CHARACTER*(*) CFL
      CHARACTER*40 PROD_NAME
      CHARACTER*8 CNODE,PNODE
      CHARACTER*26 VAXTIM
      INTEGER LHSTR1, GZHSTR, LZLOC, J, ND
C----------------------------------------------------------------------
      LHSTR1 = LHSTR
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LHSTR .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LHSTR .LE. 0 ) THEN
          IF( NHSTR .EQ. 0 ) GOTO 980          ! Error exit
          LHSTR1 = LZLOC( IXMAIN, 'HSTR', NHSTR )
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LHSTR1 = GZHSTR()
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
1000    FORMAT(/' ** PRHSTR ** Illegal value of CFL = ',a/)
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LHSTR1
C
      WRITE(PRUNIT,1010)
C
      IF(LHSTR1.NE.0)THEN
      CALL UHTOC(IQ(LHSTR1+7),40,PROD_NAME,40)
      CALL UHTOC(IQ(LHSTR1+17),8,CNODE,8)
      CALL UHTOC(IQ(LHSTR1+19),8,PNODE,8)
C
      WRITE(PRUNIT,'('' Bank Version            : '',I6)')
     &   IQ(LHSTR1+1)
      WRITE(PRUNIT,'('' Production Package ID#  : '',I6)')
     &   IQ(LHSTR1+2)
      WRITE(PRUNIT,'('' Version#                : '',I6)')
     &   IQ(LHSTR1+3)
      WRITE(PRUNIT,'('' Pass#                   : '',I6)')
     &   IQ(LHSTR1+4)
      CALL OFTSTR( IQ(LHSTR1+5),VAXTIM)
      WRITE(PRUNIT,'('' Creation time           : '',A)') VAXTIM
      CALL OFTSTR( IQ(LHSTR1+6),VAXTIM)
      WRITE(PRUNIT,'('' Processing time         : '',A)') VAXTIM
      WRITE(PRUNIT,'('' Production Package Name : '',A)')
     &   PROD_NAME
      WRITE(PRUNIT,'('' Creation Site           : '',A)')
     &   CNODE
      WRITE(PRUNIT,'('' Processing Site         : '',A)')
     &   PNODE
      ND = IQ(LHSTR1-1)       !allow for old banks
      IF(ND.GE.21)  
     &  WRITE(PRUNIT,'('' TRD Tags                : '',Z10.8)')
     &  IQ(LHSTR1+21)
      IF(ND.GE.22)
     &  WRITE(PRUNIT,'('' L2 run for STP building :'',I10)')
     &  IQ(LHSTR1+22)

C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
      IF( CFL .EQ. 'LINEAR' ) THEN
        LHSTR1 = LQ( LHSTR1 )
        IF( LHSTR1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command. 
C
        LHSTR1 = GZHSTR()
      ENDIF
      ELSE
        WRITE(PRUNIT,1011) 
      ENDIF
  999 RETURN
 1010 FORMAT(//,' DUMP OF HISTORY BANK (HSTR)',/)
 1011 FORMAT(' *** NO HSTR BANK ***'//) 
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LHSTR
 2000 FORMAT(/' ** PRHSTR ** called for LINEAR without valid bank ',
     &        'pointer, LHSTR =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LHSTR, NHSTR
 2100 FORMAT(/'  ** PRHSTR ** called for ONE without bank pointer and ',
     &        'bank number, LHSTR =',I10,' NHSTR =', I10/)
      GOTO 999
      END
