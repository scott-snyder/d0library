      PROGRAM michsugra

c     Interactive form
c
c     This routine is an example of how to create one model at a time using
c     the accompanying routines by calling the subroutine masseve. Use this
c     to model your own model-building needs.
c
c     This version not for distribution - a distribution version will be
C     available in weeks to come. This version does not contain all the
c     most recent code and upgrades, nor is it commented as it should be
c     for use outside this group. Changes should be made only to the routines
c     batch.f and interactive.f; all the rest should be left as is. If you
c     find any errors (or simply suspect some) please contact me at
c     kolda@umich.edu.
c
c                                  Christopher Kolda
c                                  Univ. of Michigan
c                                  23 May 1994
c Modified: R. J. Genik II, Mich. State.
c hacked out Chris's code to add random generatrion of points.
C-   Updated   1-OCT-1994   R. J. Genik II  Added Cascade of returns
C-   when gtop in diffeqs gets above the limit accepted at the beginning. Write
C-   dbase now is called in this case, and write out whatever is in the
C-   parameters blocks at this point NOTE: this could be old info depending on
C-   the initialization used by Kolda, et. al. The 5 Gut params are the ones
C-   generating the error, though. In addtion, the flag is set true in the
C-   Cernlib routine DDEQMR, which will cause the same cascade of returns ( I
C-   assume.)
C-   Updated   1-OCT-1994   R. J. Genik II  To turn off the mods,  just
C-   eliminate the line in DIFFEQS and DDEQRM which sets the gtop_too_big flag
C-   to .TRUE.
C-   Updated   9-OCT-1994   R. J. Genik II  Added ABS(GTOP) to avoid
C-   condition where GTOP diverges negatively, and added check on abs(gbot)
C-   and abs(gtau) to the same value.
C-   Updated  21-NOV-1994   R. J. Genik II  added cuts for ewsb failing
C-   (abs(mmu).lt.0.25), and mh0.lt.0 (tachionic stop)
C-   Updated  22-NOV-1994   R. J. Genik II  Added constrain_cut logical
C-   function instead of just specifically doing it here
C-   Updated  29-NOV-1994   R. J. Genik II  Added RTSEC cascade hook, seed
C-   printing every 10% of job. commenting out line in RTSEC will kill this
C-   mod.
C-   Updated  17-FEB-1995   R. J. Genik II  adding timing
C-   Updated  24-JUN-1995   R. J. Genik II  adding more cuts, and charge symmetry condition
C-   Modified  3-NOV-1995   A Lyon          Now just evolves sugra into mssm in a nice way
C-                                          to interface with spythia.

      IMPLICIT NONE
      INCLUDE 'D0$SPYTHIA$INC:VARS.INC'
      INCLUDE 'D0$SPYTHIA$INC:MASSMAT.INC'
      INCLUDE 'D0$SPYTHIA$INC:TUNING.INC'
      REAL*8 pass(94),topmass,tanb,mzero,monehalf,azero,smu
      Real rnot, ran_init_seq,
     +  ran_point, Ran_Get_Seed, Ran_Set_Seed, int_10x
      INTEGER isoln,ierror,masseve, solution_count,
     +  loop_count , bad_try, temp_unit, seq_num, i, isq, igl, unit,
     &  ierr
      LOGICAL Constrain_cut, ok
      Character*8 cur_time
      Character*80 Rlux_File
      Real Loop_Escape
      Parameter (temp_unit = 21)
      Parameter (Loop_Escape = 5.0) ! Ratio of bad tries to requested tries
                                    ! intiating loop escape
      bad_try = 0
      isoln = 0
      gtop_limit = 300.
C

      PRINT*, 'Michigan SUGRA -> MSSM Conversion'
      PRINT*, ' '

      PRINT*, 'Mtop? '
      READ*, topmass

      PRINT*, 'Tan(beta)? '
      READ*, tanb

      PRINT*, 'M0? '
      READ*, mzero

      PRINT*, 'M1/2? '
      READ*, monehalf

      PRINT*, 'A0? '
      READ*, azero

      PRINT*, 'Sign(mu)? '
      PRINT*, '[Note: If -1 for Baer, use +1 here and vice-versa]'
      READ*, smu


C*****Convert from SUGRA to MSSM
      ierror=masseve(topmass,tanb,mzero,monehalf,azero,smu, pass)
      if ( ierror .NE. 1 ) stop 'Something bad happened to masseve'

C*****Get masses for observable squarks and gluinos
      CALL transmich_msbar_pass(isq, igl)


C*****Make gluino3 mass THE gluino mass for spythia
      pass(53) = DBLE(igl)


      PRINT*, ' '
      PRINT*, 'SUGRA -> MSSM parameters (G. Kane)'
      PRINT*, ' '
      PRINT 100, pass(9), pass(11), azero, pass(2), pass(3),  smu
 100  FORMAT(1X,'Mtop = ',F5.1,'   tan(beta) = ',F4.1,9X,'A0 = ',F4.1,/,
     &       1X,'M0   = ',F5.1,7X,'M1/2 = ',F5.1,3X,'sign(mu) = ',F4.1)

      print*, ' '
      PRINT*, '                     Left              Right '
      PRINT*, ' '
      PRINT 110, pass(21), pass(27)
      PRINT 120, pass(22), pass(28)
      PRINT 130, pass(23), pass(29)
      PRINT 140, pass(24), pass(30)
      PRINT 150, pass(26), pass(32)
      PRINT 160, pass(25), pass(31)
      PRINT 170, pass(33), pass(39)
      PRINT 180, pass(34), pass(40)
      PRINT 190, pass(35), pass(41)
      PRINT 200, pass(36), pass(42)
      PRINT 210, pass(37), pass(43)
      PRINT 220, pass(38), pass(44)
 110  FORMAT(' Up~',17X,F6.1,12X,F6.1)
 120  FORMAT(' Down~',15X,F6.1,12X,F6.1)
 130  FORMAT(' Charm~',14X,F6.1,12X,F6.1)
 140  FORMAT(' Strange~',12X,F6.1,12X,F6.1)
 150  FORMAT(' Bottom~',13X,F6.1,12X,F6.1)
 160  FORMAT(' Top~',16X,F6.1,12X,F6.1)
 170  FORMAT(' Nu e~',15X,F6.1,12X,F6.1)
 180  FORMAT(' Electron~',11X,F6.1,12X,F6.1)
 190  FORMAT(' Nu mu~',14X,F6.1,12X,F6.1)
 200  FORMAT(' Muon~',15X,F6.1,12X,F6.1)
 210  FORMAT(' Nu tau~',13X,F6.1,12X,F6.1)
 220  FORMAT(' Tau~',16X,F6.1,12X,F6.1)

      PRINT *, ' '

      PRINT*, 'Gluino = ',pass(53)

      PRINT*, 'MZ1 = ',pass(55),'   MZ2 = ',pass(56)
      PRINT*, 'MZ3 = ',pass(57),'   MZ4 = ',pass(58)
      PRINT*, 'MW1 = ',pass(59),'   MW2 = ',pass(60)
      PRINT*, 'Mt~1= ',pass(61),'   Mt~2= ',pass(62)

      PRINT*, ' '

      PRINT*, 'Observable gluino = ',igl
      PRINT*, 'Observable squark = ',isq

      CALL GTUNIT(8888,unit,ierr)
      CALL D0OPEN(unit,'susyparams.out','OF',ok)

      IF ( .NOT. ok .OR. ierr .NE. 0 ) STOP 'Cannot open file'

      DO i=1,94

        IF ( i .NE. 7 ) THEN
          WRITE(unit,500) pass(i)
 500      FORMAT(F15.3)
        ELSE
          WRITE(unit,501) pass(i)
 501      FORMAT(G10.5)
        ENDIF

      ENDDO

      CALL D0CLOSE(unit,' ',ok)


      STOP
      END
