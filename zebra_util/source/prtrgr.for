      SUBROUTINE PRTRGR ( PRUNIT, LTRGR, NTRGR, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'TRGR'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LTRGR  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NTRGR  [I] : Bank number, used only if CFL='ONE' and LTRGR = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LTRGR point to a bank, or if <0, NTRGR is
C-                                  the bank number.
C-                          'LINEAR' : LTRGR points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing.
C-                              0:  Perform all following dumps:
C-                              1:  Datablock Summary only
C-                              2:  ADC counts by tower only
C-                              3:  L1.5_CT header only
C-                              4:  L1.5_CT debug section only
C-                              5:  L1.5_CT all but no header and debug
C-
C-
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created   3-OCT-1990   Maris Abolins - Michigan State University
C-   Updated  20-JAN-1992   Philippe Laurens, Steven Klocek
C-                      Made routine follow PRXXXX template.
C-                      Print contents of actual TRGR bank.
C-   Updated 23-NOV-1993   Johannes V. (Djoko) Wirjawan
C-                      Include level 1.5
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTRGR.LINK'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$PARAMS:L15CALDBB_DATA_BLOCK.PARAMS'
C
      INTEGER PRUNIT, LTRGR, NTRGR, IFL, STATUS
      CHARACTER*(*) CFL
      INTEGER LTRGR1, LTRGR15, GZTRGR, LZLOC, J
C
      INTEGER GZFIND_CRATE
      EXTERNAL GZFIND_CRATE
      INTEGER LTRGR_LEVEL1, LTRGR_LEVEL15
      LOGICAL AHA, DO_LV1_PRTRGR, DO_L15_PRTRGR
      DATA AHA /.TRUE./
C
C----------------------------------------------------------------------
C
      LTRGR1 = LTRGR
      IF ( CFL .EQ. 'LINEAR' ) THEN
         IF( LTRGR .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
         IF( LTRGR .LE. 0 ) THEN
           IF( NTRGR .EQ. 0 ) GOTO 980          ! Error exit
           LTRGR1 = LZLOC( IXMAIN, 'TRGR', NTRGR )
         ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C >>>>  Here, you have to find the first bank to be printed
C
         LTRGR1 = GZTRGR( )
      ELSE
         WRITE( PRUNIT, 1000 ) CFL
 1000    FORMAT(/' ** PRTRGR ** Illegal value of CFL = ',A/)
         GOTO 999
      ENDIF
    1 CONTINUE
C
C  >>>  Print the content of the bank pointed by LTRGR1
C
C
C       Find the beginning of the Level 1 Datablock portion of the TRGR bank
C
      LTRGR_LEVEL1 = GZFIND_CRATE ( 'TRGR', LTRGR1, CRATE_ID )
C
C       Each type of information to dump has its own IFL number.
C       IFL = 0 means print all information
C
      IF ((IFL .EQ. 0) .OR. (IFL .EQ. 1)) THEN
         CALL PRTRGR_L1_FW_AND_CT_DBLOCK( PRUNIT, LTRGR_LEVEL1 )
      ENDIF
      IF ((IFL .EQ. 0) .OR. (IFL .EQ. 2)) THEN
         CALL PRTRGR_L1_FW_AND_CT_ADC( PRUNIT, LTRGR_LEVEL1 )
      ENDIF
C
C
 4000 CONTINUE
C
C
C  >>>  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
      IF(( CFL .EQ. 'LINEAR' ) .OR. ( CFL .EQ. 'ALL' )) THEN
         LTRGR1 = LQ( LTRGR1 )
         IF( LTRGR1 .NE. 0 ) GOTO 1
      ENDIF
C
C >>> Look for the first level 1.5 bank to be printed
C
      LTRGR15 = GZTRGR( )
    2 CONTINUE
C
C >>> Print the content of the bank pointed by LTRGR15
C     Find the beginning of the L15 datablock  portion fo the TRGR bank
C
      IF (LTRGR15.GT.0) THEN
         LTRGR_LEVEL15 = GZFIND_CRATE ('TRGR', LTRGR15, L15CAL_CRATE_ID)
         IF ((IFL.EQ.0) .OR. (IFL.EQ.1) .OR. (IFL.EQ.2)) THEN
           CALL PRTRGR_L15_CT_DBLOCK ( PRUNIT, LTRGR_LEVEL15 )
         ELSEIF (IFL.EQ.3) THEN
           CALL PRTRGR_L15_CT_HEADER ( PRUNIT, LTRGR_LEVEL15 )
         ELSEIF (IFL.EQ.4) THEN
           CALL PRTRGR_L15_CT_DEBUG ( PRUNIT, LTRGR_LEVEL15 )
         ELSE
           CALL PRTRGR_L15_CT_DATA ( PRUNIT, LTRGR_LEVEL15 )     
         ENDIF
      ENDIF
C
 4001 CONTINUE
C
C  >>>  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
      IF(( CFL .EQ. 'LINEAR' ) .OR. ( CFL .EQ. 'ALL' )) THEN
         LTRGR15 = LQ( LTRGR15 )
         IF( LTRGR15 .NE. 0 ) GOTO 2
      ENDIF
C
C
C
  999 RETURN
C
C  >>> Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LTRGR
 2000 FORMAT(/' ** PRTRGR ** called for LINEAR without valid bank ',
     &        'pointer, LTRGR =',I10/)
      GOTO 999
C
C  >>> Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LTRGR, NTRGR
 2100 FORMAT(/'  ** PRTRGR ** called for ONE without bank pointer and',
     &        ' bank number, LTRGR =',I10,' NTRGR =', I10/)
      GOTO 999
      END
