      SUBROUTINE PRGMUH ( PRUNIT, LGMUH, NGMUH, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'GMUH'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LGMUH  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NGMUH  [I] : Bank number, used only if CFL='ONE' and LGMUH = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LGMUH point to a bank, or if <0, NGMUH is
C-                                  the bank number.
C-                          'LINEAR' : LGMUH points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   CREATED : 20-APR-1993    Jasbir Singh
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZGMUH.LINK'
C
      INTEGER NHITS,L
      INTEGER PRUNIT, LGMUH, NGMUH, IFL
      CHARACTER*(*) CFL
      INTEGER LGMUH1, LZLOC, J, NV, NH, NR
      INTEGER LGHIT,GZGHIT
      INTEGER IBANK
C----------------------------------------------------------------------
      LGMUH1 = LGMUH
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LGMUH .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LGMUH .LE. 0 ) THEN
          IF( NGMUH .EQ. 0 ) GOTO 980          ! Error exit
          LGMUH1 = LZLOC( IXMAIN, 'GMUH', NGMUH )
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
        LGHIT = GZGHIT()
        IF ( LGHIT.LE.0 ) GOTO 999
        LGMUH1 = LQ(LGHIT-IZGMUH)
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
 1000   FORMAT(/' ** PRGMUH ** ILLEGAL VALUE OF CFL = ',A/)
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LGMUH1
C
      NV = IQ(LGMUH1+1)
      WRITE(PRUNIT,1050) NV
 1050 FORMAT(/
     &' ========================================================='/
     &'      GMUH: MUON CHAMB track hits bank : VERSION ', I3,' '/
     &' ========================================================='/)
C
C
      NH = IQ(LGMUH1+2)
      NR = IQ(LGMUH1+3)
C
C ****  PRINT WITH HADRONIC CELL ENERGY PART
C
      WRITE(PRUNIT,1102) 
     &      IQ(LGMUH1-5),
     &      (IQ(LGMUH1+J),J=2,3),
     &      (Q(LGMUH1+J),J=4,10),(IQ(LGMUH1+J),J=11,17),
     &      (Q(LGMUH1+J),J=18,20), (Q(LGMUH1+J),J=21,24)
      NHITS=IQ(LGMUH1+25)
      IF(NHITS.GT.0) THEN
        WRITE(PRUNIT,1106)NHITS

 1106   FORMAT( ' NUMBER OF HITS: ',I5/)
        WRITE(PRUNIT,1105)
C      
        WRITE(PRUNIT,1103)
C
        WRITE(PRUNIT,1105)
        DO J=1,NHITS
          IBANK = LGMUH1 + (J-1)*15+25
          WRITE(PRUNIT,1104)
     &          IQ(IBANK+15),(IQ(IBANK+L),L=1,5),(Q(IBANK+L),L=6,14)
        END DO       
      END IF

 1102 FORMAT(/
     &    ' TRACK NUMBER : ',I6/
     &    ' HEADER WORDS : ',I5, ' REPEATED WORD IN HITSUM DATA ',I5/
     &    ' TRACK VERTEX: ',3(F10.3,1X)/
     &    ' MOMENTUM:     ',4(F10.3,1X)/
     &    ' TRACK TYPE/ORIGIN VERTEX #/PARENT VERTEX #/PARENT TRACK: '/
     &    4(I5,1X)/
     &    ' REASON FOR TRACK CREATION: ',I5/
     &    ' ISAJET PARENT VERTEX/ISAJET TRACK #: ',2(I5,1X)/
     &    ' TRACK POSITION AT CAL EXIT  POINT:  ',3(F10.3,1X)/
     &    ' TRACK 4 MOMENTA AT CAL EXIT POINT: ',4(F10.3,1X)/)
 1105 FORMAT(1X,9('========='),/)
 1103 FORMAT(' IHIT ISET IDET ITRA    NUMB                 HITSUM',/,
     &25X,'1    2     ',
     &'1      2      3       4       5      6      7      8      9 ')
 1104 FORMAT(/6I5,9(F8.2))
C
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
      IF( CFL .EQ. 'LINEAR' ) THEN
        LGMUH1 = LQ( LGMUH1 )
        IF( LGMUH1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command.
C
        LGMUH1 = LQ( LGMUH1 )
        IF ( LGMUH1.NE.0 ) GOTO 1
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LGMUH
 2000 FORMAT(/' ** PRGMUH ** CALLED FOR LINEAR WITHOUT VALID BANK '
     &        ,'POINTER, LGMUH =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LGMUH, NGMUH
 2100 FORMAT(/
     &    '  ** PRGMUH ** CALLED FOR ONE WITHOUT BANK POINTER AND ' 
     &    ,'BANK NUMBER, LGMUH =',I10,' NGMUH =', I10/)
      GOTO 999
      END
