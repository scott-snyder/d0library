      SUBROUTINE cjet_muprjets ( prunit, ljets, njets, cfl, ifl )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'JETS'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LJETS  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'DEF' or 'ALL'.
C-             NJETS  [I] : Bank number, used only if CFL='ONE' and LJETS = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LJETS point to a bank, or if <0, NJETS is
C-                                  the bank number.
C-                          'LINEAR' : LJETS points to the first bank of the
C-                                  Linear structure
C-                          'DEFAULT' : Prints JETS banks for DEFAULT algorithm
C-                          'ALL' : Prints all ALGORITMS' JETS banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created   19-OCT-1988  Z. Wolf
C-   Modified  26-FEB-1989  Z. Wolf   Make conform to template.
C-   Updated  28-JAN-1991   Boaz Klima  -
C-      Add SET_CAPH to use DEFault algorithm or ALL algorithms,NEW format
C-      and include some information from JTSH in the minimum printout
C-   Updated  19-NOV-1991   Nick Hadley, Boaz Klima
C-      Move stuff from JTSH to JETS
C-   Updated  17-JAN-1993 : Alex Smith : added MUON algorithm
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZJETS.LINK'
      INCLUDE 'D0$LINKS:IZJPTS.LINK'
      INCLUDE 'D0$LINKS:IZJTSH.LINK'
      INCLUDE 'D0$LINKS:IZCAPH.LINK'
C
      INTEGER PRUNIT, LJETS, NJETS, IFL, K
      CHARACTER*(*) CFL
      INTEGER LJETS1, GZJETS, LZLOC, J, LJPTS, NJPTS
      INTEGER LJTSH, NJTSH
      REAL    TEMPLATE(20),EMFRAC,SIGMA
      INTEGER LCAPH, GZCAPH, IER, IFLAG
      CHARACTER*10 HEADER(12)
      DATA HEADER / '  N','    PX','    PY','    PZ','     E'
     &  ,'    ET',' THETA ','  PHI','  ETA ','  EM ',' SIGMA',' FLAG'/
C----------------------------------------------------------------------
      LJETS1 = LJETS
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LJETS1 .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LJETS1 .LE. 0 ) THEN
          IF( NJETS .EQ. 0 ) GOTO 980          ! ERROR EXIT
          LJETS1 = LZLOC( IXMAIN, 'JETS', NJETS )
        ENDIF
      ELSEIF( CFL .EQ. 'DEFAULT' ) THEN
C
C ****  SET PATH TO THE DEFAULT JET FINDING ALGORITHM
C ****       ( CONE WITH R=0.7 )
C
        TEMPLATE(1)=1
        TEMPLATE(2)=6
        TEMPLATE(3)=0.7
        CALL SET_CAPH('CONE_JET',TEMPLATE,IER)
        IF ( IER.NE.0 ) GOTO 999
        LCAPH = GZCAPH( )
C  ***  PRINT THE MAIN CAPH PARAMETERS
        LJETS1 = GZJETS( )
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  LOOP OVER ALL JETS BANKS : FIRST SET PATH TO THE DEFAULT
C ****  JET FINDING ALGORITHM THEN TO OTHERS
C
        TEMPLATE(1)=0
        CALL SET_CAPH('MUON_JET',TEMPLATE,IER)
        IF ( IER.NE.0 ) GOTO 999
        LCAPH = GZCAPH( )
        IF( LCAPH .EQ. 0 ) GOTO 998
        LJETS1 = GZJETS( )
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
 1000   FORMAT(/' ** CJET_MUPRJETS ** ILLEGAL VALUE OF CFL = ',A/)
        GOTO 999
      ENDIF
    2 CONTINUE
      IF (LJETS1.LE.0) GOTO 997
      IF ( CFL.NE.'ONE'.AND.CFL.NE.'LINEAR' ) THEN
        IF ( IQ(LCAPH+4).EQ.2 ) WRITE(PRUNIT,7) IQ(LJETS1+1),
     &    Q(LCAPH+6),Q(LCAPH+7)
    7   FORMAT(///' JET CONE ALGORITHM ( VERSION NO.',I3,' ) :',/,
     &            ' --------------------------------------',/,
     &            ' ( R = ',F5.2,' , ET > ',F5.2,' GEV )',/)
        IF ( IQ(LCAPH+4).EQ.4 ) WRITE(PRUNIT,9) IQ(LJETS1+1),
     &    Q(LCAPH+6),Q(LCAPH+7)
    9   FORMAT(///' JET MUON ALGORITHM ( VERSION NO.',I3,' ) :',/,
     &            ' --------------------------------------',/,
     &            ' ( R = ',F5.2,' , ET > ',F5.2,' GEV )',/)
        IF ( IQ(LCAPH+4).EQ.3 ) WRITE(PRUNIT,8) IQ(LJETS1+1),
     &    Q(LCAPH+7),Q(LCAPH+13)
    8   FORMAT(///' JET NEAREST NEIGHBOR ALGORITHM ( VERSION NO.',
     &    I3,' ) :',/,
     &    ' --------------------------------------------------',/,
     &    ' ( N = ',F5.2,' , ET > ',F5.2,' GEV )',/)
      ENDIF
      WRITE(PRUNIT,33) (HEADER(K),K=1,12)
    1 CONTINUE
C--   WRITE BANK CONTENTS ( JETS AND A LITTLE BIT FROM JTSH )
      IF ( IQ(LJETS1+1).EQ.1 ) THEN
        LJTSH = LQ(LJETS1-IZJTSH)
        EMFRAC = Q(LJTSH+5)
        SIGMA = SQRT ( Q(LJTSH+3)**2 + Q(LJTSH+4)**2 )
        IFLAG = IQ(LJTSH+10)
C           OUTPUT JTSH BANK IF IFL = 0 OR 2
        IF (IFL.EQ.0 .OR. IFL.GE.2 ) THEN
          LJTSH = LQ(LJETS1-IZJTSH)
          IF(LJTSH.NE.0) CALL PRJTSH(PRUNIT,LJTSH,N JTSH,CFL,IFL)
        ENDIF
      ELSE
        EMFRAC = Q(LJETS1+14)
        SIGMA = SQRT ( Q(LJETS1+12)**2 + Q(LJETS1+13)**2 )
        IFLAG = IQ(LJETS1+15)
      ENDIF
      WRITE(PRUNIT,32)
     &  IQ(LJETS1-5)+1,(Q(LJETS1+K),K=2,9),EMFRAC,SIGMA,IFLAG
   32 FORMAT(I3,5F8.2, 5F6.2, I5)
   33 FORMAT(A3,5A8,   5A6  , A5)
C           OUTPUT JPTS BANK IF IFL = 0
      IF (IFL.EQ.0  ) THEN
        LJPTS = LQ(LJETS1-IZJPTS)
        CALL PRJPTS(PRUNIT,LJPTS,NJPTS,CFL,IFL)
      ENDIF
C
C  ***  LOOK IF ANOTHER BANK IS NEEDED
C
      IF( CFL .EQ. 'ONE' ) THEN
        GOTO 999
      ELSE
        LJETS1 = LQ( LJETS1 )
        IF( LJETS1 .NE. 0 ) GOTO 1
      ENDIF
  997 CONTINUE
      IF( CFL .EQ. 'ALL' ) THEN
        LCAPH = LQ( LCAPH )
        IF( LCAPH .NE. 0 ) THEN
          LJETS1 = LQ(LCAPH-IZJETS)
          GOTO 2
        END IF
      ENDIF
  998 CALL RESET_CAPH
  999 RETURN
C
C  *** ERROR : LINEAR WITHOUT BANK POINTER
C
  990 WRITE( PRUNIT, 2000 ) LJETS
 2000 FORMAT(/
     &  ' ** CJET_MUPRJETS ** CALLED FOR LINEAR WITHOUT VALID BANK '
     &        ,'POINTER, LJETS =',I10/)
      GOTO 999
C
C  *** ERROR : ONE BANK, BUT NEITHER POINTER NOR NUMBER
C
  980 WRITE( PRUNIT, 2100 ) LJETS, NJETS
 2100 FORMAT(/'  ** CJET_MUPRJETS ** CALLED FOR
     &  ONE WITHOUT BANK POINTER AND '
     &  ,'BANK NUMBER, LJETS =',I10,' NJETS =', I10/)
      GOTO 999
      END
