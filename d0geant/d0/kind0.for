      LOGICAL FUNCTION KIND0()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-                         Fill JVERT and JKINE Geant banks.
C-                         Also form and optionally fill D0'S /ZEBCOM/
C-
C-
C-   Returned value  : TRUE
C-   Inputs  : See below
C-   Outputs : JVERT, JKINE and optionally /ZEBCOM/ zebra banks
C-
C-   Notes   :
C-                         Data for JVERT and JKINE may come from one of
C-                         four places:
C-                         1) /ZEBCOM/ if ZBIO n m present and n > 0
C-                         2) GSAVE.DAT if GET 'INIT' 'KINE' ... present
C-                               GSAVE.DAT must be written in previous run
C-                               with SAVE 'INIT' 'KINE' ... present)
C-                         3) KINDAT.DAT if RKIN n with n > 0 present
C-                                ( ONLY if 1) and 2) are not .TRUE. )
C-                              Free Format Read:
C-                                NPTCL,(VERTEX(k),k=1,3)
C-                                  = # of particles, and vertex(x,y,z)
C-                                (IDENT,(PPTCL(K),K=1,3)) NPTCL times
C-                                  = ISAJET ID, and px,py,pz for each particle
C-                         4) KINE data card if 1), 2) and 3) .FALSE
C-                                (IDENT,PZ,PT,PHI,VX,VY,VZ)
C-
C-  NOTE:   THERE IS NO QUARANTEE that the two files used by:
C-             GET 'INIT' 'KINE'...  and by ZBIO n m are the same events!!
C-          BE CAREFUL with your logical assignments!!!!!!!
C-
C-   To use both GET 'INIT' 'KINE' ... and ZBIO n m  you must have:
C-      SAVE 'INIT' 'KINE' ...
C-      ZBIO n m
C-   in one job. Then:
C-      GET 'INIT' 'KINE' ...
C-      ZBIO m k
C-   in the second job.  BE CAREFUL ABOUT YOUR LOGICAL ASSIGNMENTS!!!!!!
C-
C-  GET ...  reads from FOR001.DAT by default
C-  SAVE ... writes to  FOR002.DAT by default
C-  ZBIO n m reads from FORxxn.DAT and writes to FORxxm.DAT
C-
C-   Created  11-JAN-1986  DH,SL,SK,AJ
C-   Updated     MAY-1986  KUNORI            - PUT IN ISAJET IO
C-   Updated   9-OCT-1987   A.M.Jonckheere   - Allow both /ZEBCOM/ and
C-                                              GET 'KINE'
C-   Updated  28-NOV-1988   Alan M. Jonckheere  : Add Parent Parton # as 6th
C-                                                 and Jet # as 7th UBUF
C-                                                 parameters
C-   Updated   7-JUL-1989   Harrison B. Prosper
C-   Made into logical function. This is now part of the D0 package.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:GCONST.INC/LIST'
      INCLUDE 'D0$INC:GCKINE.INC/LIST'
      INCLUDE 'D0$INC:GCLIST.INC/LIST'
      INCLUDE 'D0$INC:ZEBIO.INC/LIST'
C
      LOGICAL FIRST
      INTEGER NUBUF,KINPUT,NPTCL,NTT,I,J,NVERT,IER
      PARAMETER ( NUBUF=7 )
      INTEGER IDENT
      REAL PPTCL(3),VERTEX(3),UBUF(NUBUF)
      INTEGER KINE,HITS,DIGI,JXYZ
C
      INTEGER IUCOMP
      character*4 lgetc(20), lsavec(20)
C
      DATA KINPUT,FIRST/0,.TRUE./
      DATA KINE/4hKINE/,HITS/4hHITS/,DIGI/4hDIGI/,JXYZ/4hJXYZ/
C----------------------------------------------------------------------
      KIND0 = .TRUE.

      if( first )then
       do i=1,20
         call uhtoc(lget(i),4,lgetc(i),4)
         call uhtoc(lsave(i),4,lsavec(i),4)
       enddo
       first=.false.
      endif
C
C ****  Readin events in standard Dzero-Zebra format
C ****  ALWAYS SETUP BANKS
C ****  Read in Banks only of IRDUNI > 0 (from ZBIO card)
C
      CALL ISKINE
C
C ****  May ALSO readin JVERT and JKINE banks
C
      IF ( (IUCOMP(KINE,LGET,NGET).NE.0) .OR.
     &     (IUCOMP(HITS,LGET,NGET).NE.0) .OR.
     &     (IUCOMP(DIGI,LGET,NGET).NE.0) .OR.
     &     (IUCOMP(JXYZ,LGET,NGET).NE.0) ) THEN
        CALL GGET(1,lgetc,NGET,IDENT,IER)  ! Read Banks
      ENDIF
C
C ****  If neither GET 'KINE' nor ZBIO set then read from either
C ****  RKIN stream or from the KINE card, chosen by RKIN n ( n.gt.0 )
C
      IF ( IRDUNI.LE.0 .AND. IUCOMP(KINE,LGET,NGET).LE.0 ) THEN
        CALL UZERO(UBUF,1,NUBUF)
        UBUF(3) = 12                    ! Call it an hadronic interaction
        IF ( RKIN.GT.0 ) THEN  !read in kinematics from stream rkin
          KINPUT = RKIN
          IF ( FIRST ) THEN         ! Open file
            OPEN(UNIT=KINPUT,FILE='KINDATA',STATUS='OLD',
     +          FORM='FORMATTED')
            FIRST = .FALSE.
          ENDIF                  ! Read file
          READ(KINPUT,*) NPTCL,(VERTEX(I),I = 1,3)
C
          CALL GSVERT(VERTEX(1),0,0,0,0,NVERT) ! Load JVERTX
          DO I = 1,NPTCL
            READ(KINPUT,*) IDENT,(PPTCL(J),J = 1,3)
            CALL ISAGEA(IDENT,IPART)    ! Convert from ISA->GEANT ID
            CALL GSKINE(PPTCL,IPART,NVERT,UBUF,NUBUF,NTT)!Load JKINE
            IF ( DTRK.EQ.2 ) CALL PCOLOR(IPART)
          ENDDO
C
        ELSE  ! USE KINE DATA CARD TO GET PARTID,PZ,PT,PHI,VX,VY,VZ
C
          NPTCL = 1
          PPTCL(1) = PKINE(2)*COS(PKINE(3)*DEGRAD)
          PPTCL(2) = PKINE(2)*SIN(PKINE(3)*DEGRAD)
          PPTCL(3) = PKINE(1)
          VERTEX(1) = PKINE(4)
          VERTEX(2) = PKINE(5)
          VERTEX(3) = PKINE(6)
          IF ( DTRK.EQ.2 ) CALL PCOLOR(IKINE)
          CALL GSVERT(VERTEX(1),0,0,0,0,NVERT)
          CALL GSKINE(PPTCL,IKINE,NVERT,UBUF,NUBUF,NTT)
        ENDIF
      ENDIF
C
  999 RETURN
      END
