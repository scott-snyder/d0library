      SUBROUTINE PRGCAH ( PRUNIT, LGCAH, NGCAH, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'GCAH'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LGCAH  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NGCAH  [I] : Bank number, used only if CFL='ONE' and LGCAH = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LGCAH point to a bank, or if <0, NGCAH is
C-                                  the bank number.
C-                          'LINEAR' : LGCAH points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  17-FEB-1989   Alan M. Jonckheere
C-   Modified 24-OCT-1989   K. De - Fix bug, print track number
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZGCAH.LINK'
C
      INTEGER PRUNIT, LGCAH, NGCAH, IFL
      CHARACTER*(*) CFL
      INTEGER LGCAH1, GZGCAH, LZLOC, J, NV, NH, NR
      INTEGER LGHIT,GZGHIT
C----------------------------------------------------------------------
      LGCAH1 = LGCAH
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LGCAH .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LGCAH .LE. 0 ) THEN
          IF( NGCAH .EQ. 0 ) GOTO 980          ! Error exit
          LGCAH1 = LZLOC( IXMAIN, 'GCAH', NGCAH )
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
        LGHIT = GZGHIT()
        IF ( LGHIT.LE.0 ) GOTO 999
        LGCAH1 = LQ(LGHIT-IZGCAH)
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
 1000   FORMAT(/' ** PRGCAH ** ILLEGAL VALUE OF CFL = ',A/)
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LGCAH1
C
      NV = IQ(LGCAH1+1)
      WRITE(PRUNIT,1050) NV
 1050 FORMAT(/
     &' ========================================================='/
     &'      GCAH: Calorimeter track hits bank : VERSION ', I3,' '/
     &' ========================================================='/)
C
C ****  VERSION 2 PRINT 
C
      IF (NV.EQ. 2) THEN
        NH = IQ(LGCAH1+2)
        NR = IQ(LGCAH1+3)
        IF (NR.EQ.2) THEN
C
C ****  PRINT WITHOUT HADRONIC PART
C
          WRITE(PRUNIT,1101) 
     &      IQ(LGCAH1-5),
     &      (IQ(LGCAH1+J),J=2,3),
     &      (Q(LGCAH1+J),J=4,10),(IQ(LGCAH1+J),J=11,17),
     &      (Q(LGCAH1+J),J=18,21), (Q(LGCAH1+J),J=22,25),
     &      (Q(LGCAH1+J),J=26,28),(Q(LGCAH1+J),J=29,31),
     &      IQ(LGCAH1+32),IQ(LGCAH1+33),
     &      (IQ(LGCAH1+J),Q(LGCAH1+J+1),J=34,IQ(LGCAH1-1)-1,2)
        ELSE
C
C ****  PRINT WITH HADRONIC CELL ENERGY PART
C
          WRITE(PRUNIT,1102) 
     &      IQ(LGCAH1-5),
     &      (IQ(LGCAH1+J),J=2,3),
     &      (Q(LGCAH1+J),J=4,10),(IQ(LGCAH1+J),J=11,17),
     &      (Q(LGCAH1+J),J=18,21), (Q(LGCAH1+J),J=22,25),
     &      (Q(LGCAH1+J),J=26,28),(Q(LGCAH1+J),J=29,31),
     &      IQ(LGCAH1+32),IQ(LGCAH1+33), (IQ(LGCAH1+J),
     &      Q(LGCAH1+J+1),Q(LGCAH1+J+2),J=34,IQ(LGCAH1-1)-1,3)
        END IF
      ELSE 
C
C ****  PRINT FOR OLD VERSIONS
C
        WRITE(PRUNIT,1100) 
     &  (Q(LGCAH1+J),J=1,7),(IQ(LGCAH1+J),J=8,14),(Q(LGCAH1+J),J=15,18),
     &  IQ(LGCAH1+19),(IQ(LGCAH1+J),Q(LGCAH1+J+1),J=20,IQ(LGCAH1-1)-1,2)
      END IF
 1101 FORMAT(/
     &  ' TRACK NUMBER : ',I6/
     &  ' HEADER WORDS : ',I5, ' REPEATED WORDS IN CELL DATA ',I5/
     &  ' TRACK VERTEX: ',3(F10.3,1X)/
     &  ' MOMENTUM:     ',4(F10.3,1X)/
     &  ' TRACK TYPE/ORIGIN VERTEX #/PARENT VERTEX #/PARENT TRACK: '/
     &                                  4(I5,1X)/
     &  ' REASON FOR TRACK CREATION: ',I5/
     &  ' ISAJET PARENT VERTEX/ISAJET TRACK #: ',2(I5,1X)/
     &  ' CAL LIVE ENERGY/MASSLESS GAPS/ICD HITS/DEAD ENERGY: '/
     &                          4(F10.3,1X)/
     &  ' TRACK 4 MOMENTA AT CAL ENTRY POINT: ',4(F10.3,1X)/
     &  ' TRACK POSITION AT CAL ENTRY POINT:  ',3(F10.3,1X)/
     &  ' TRACK POSITION AT INTERACTION    :  ',3(F10.3,1X)/
     &  ' INTERACTION TYPE                 :  ', I5/
     &  ' NUMBER OF CELLS: ',I5/
     &  '   IADDR       ENERGY    IADDR       ENERGY   ',
     &  ' IADDR       ENERGY'/(3(Z10,2X,F10.3)))
 1102 FORMAT(/
     &  ' TRACK NUMBER : ',I6/
     &  ' HEADER WORDS : ',I5, ' REPEATED WORD IN CELL DATA ',I5/
     &  ' TRACK VERTEX: ',3(F10.3,1X)/
     &  ' MOMENTUM:     ',4(F10.3,1X)/
     &  ' TRACK TYPE/ORIGIN VERTEX #/PARENT VERTEX #/PARENT TRACK: '/
     &                                  4(I5,1X)/
     &  ' REASON FOR TRACK CREATION: ',I5/
     &  ' ISAJET PARENT VERTEX/ISAJET TRACK #: ',2(I5,1X)/
     &  ' CAL LIVE ENERGY/MASSLESS GAPS/ICD HITS/DEAD ENERGY: '/
     &                          4(F10.3,1X)/
     &  ' TRACK 4 MOMENTA AT CAL ENTRY POINT: ',4(F10.3,1X)/
     &  ' TRACK POSITION AT CAL ENTRY POINT:  ',3(F10.3,1X)/
     &  ' TRACK POSITION AT INTERACTION    :  ',3(F10.3,1X)/
     &  ' INTERACTION TYPE                 :  ', I5/
     &  ' NUMBER OF CELLS: ',I5/
     &  ' IADDR    ENERGY   HAD E    IADDR   ENERGY   HAD E',
     &  '    IADDR    ENERGY   HAD E '/(3(Z8,1X,2F8.2,1X)))
 1100 FORMAT(/
     &  ' TRACK VERTEX: ',3(F10.3,1X)/
     &  ' MOMENTUM:     ',4(F10.3,1X)/
     &  ' TRACK TYPE/ORIGIN VERTEX #/PARENT VERTEX #/PARENT TRACK: '/
     &                                  4(I5,1X)/
     &  ' REASON FOR TRACK CREATION: ',I5/
     &  ' ISAJET PARENT VERTEX/ISAJET TRACK #: ',2(I5,1X)/
     &  ' CAL LIVE ENERGY/MASSLESS GAPS/ICD HITS/DEAD ENERGY: '/
     &                          4(F10.3,1X)/
     &  ' NUMBER OF CELLS: ',I5/
     &  '   IADDR       ENERGY    IADDR       ENERGY   ',
     &  ' IADDR       ENERGY'/(3(Z10,2X,F10.3)))
C
C
C

C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
      IF( CFL .EQ. 'LINEAR' ) THEN
        LGCAH1 = LQ( LGCAH1 )
        IF( LGCAH1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command.
C
        LGCAH1 = LQ( LGCAH1 )
        IF ( LGCAH1.NE.0 ) GOTO 1
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LGCAH
 2000 FORMAT(/' ** PRGCAH ** CALLED FOR LINEAR WITHOUT VALID BANK '
     &        ,'POINTER, LGCAH =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LGCAH, NGCAH
 2100 FORMAT(/'  ** PRGCAH ** CALLED FOR ONE WITHOUT BANK POINTER AND '
     &        ,'BANK NUMBER, LGCAH =',I10,' NGCAH =', I10/)
      GOTO 999
      END
