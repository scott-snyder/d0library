      SUBROUTINE PRMBAD(PRUNIT,LMBAD,MODULE,CFL,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To print out the contents of MBAD
C-
C-   Inputs  : PRUNIT   Unit number for printing
C-           *  LMBAD -  Address of MBAD bank
C-   Outputs : 
C-   Controls: MODULE - module number, if = 0, then all modules
C-             CFL    - flag for what to print - old/new/current/all
C-           *  IFL    - how much to print
C-
C-      * - dummy variables included to make this look like PRMPED etc.
C-
C-   Created   3-AUG-1990   Carol Francis
C-      Modified Jan-91     J.Green extensive rewrite
C-               Feb-91     J.Green interpret flag words
C-  DH 3/92 get rid of hexadecimal constnts for 'd0 standards' (yuch)
C-               Nov-93     J.Green Unix compatablity
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
C    Variable Declarations
C    =====================
      INTEGER PRUNIT                    ! where to print info.
      INTEGER LMBAD                     ! address
      INTEGER MODULE                    ! input module number
      CHARACTER CFL*(*)                 ! which bank
      INTEGER IFL                       ! other options
      INTEGER MODSEQ,MODNUM             ! Loop over all the MBAD banks
      INTEGER MODCOUNT                  ! number of modules
      INTEGER MUNMOD                    ! get module number
      INTEGER KMBAD(3)                  ! pointer to new MBAD
      INTEGER GZMBAD_R,GZMBAD,GZMBAD_N  ! get KMBAD
      INTEGER ITREE                     ! loop over ref,cur,new
      CHARACTER*8 CHTREE(3)             ! tree name
      INTEGER COBANK                    ! # bad channels in bank
      INTEGER NUMADD,NCHAN              ! counters for loop
      INTEGER ADDRESS                   ! address of bad channel
      INTEGER MODUL,PLANE,WIRE
      INTEGER FLAG                      ! describes bad channel
      INTEGER HARD,CHAN                 ! pieces of bad channel
      INTEGER CHMASK(11)                ! mask for pieces
      INTEGER OFFSET(11)                ! offsets to pieces
      INTEGER IERR                      ! err from MUADD
      INTEGER I                         ! format loop index
      CHARACTER*5 CHCHAN(10),BLANKS     ! character part of bad chan
      DATA    BLANKS /'     '/
      DATA    CHMASK / 8*31, 2*7, 65535 / 
      DATA    OFFSET / 0,-5, -10, -15, -20, -25, 0, -5, -10, -13, -16 /
      DATA    CHTREE /'     OLD',' CURRENT','     NEW'/
C----------------------------------------------------------------------
C    Executable Code
C    ===============
      IF (MODULE.EQ.0) THEN
        MODCOUNT = MUNMOD(0,0)
      ELSE
        MODCOUNT = 1
      ENDIF
C
      DO 500 MODSEQ = 1, MODCOUNT
        IF (MODULE.EQ.0) THEN
          MODNUM = MUNMOD(1,MODSEQ)
        ELSE
          MODNUM = MODULE
        ENDIF
        KMBAD(1) = 0
        KMBAD(2) = 0
        KMBAD(3) = 0
        IF (CFL(1:3).EQ.'ALL' .OR. CFL(1:3).EQ.'OLD')
     1                           KMBAD(1) = GZMBAD_R(MODNUM)
        IF (CFL(1:3).EQ.'ALL' .OR. CFL(1:3).EQ.'CUR')
     1                           KMBAD(2) = GZMBAD(MODNUM)
        IF (CFL(1:3).EQ.'ALL' .OR. CFL(1:3).EQ.'OLD')
     1                           KMBAD(3) = GZMBAD_N(MODNUM)
        DO ITREE = 1,3
        IF (KMBAD(ITREE).NE.0) THEN
          WRITE(PRUNIT,150) CHTREE(ITREE),MODNUM
  150     FORMAT(A,' BAD CHANNEL LISTING FOR MUON MODULE ',I4)
            WRITE(PRUNIT,155) (IC(KMBAD(ITREE)+I),I=1,14)
  155       FORMAT(' TYPE     ',I8,' STATUS   ',I8,' QUALITY       ',I8/
     A             ' LOW RUN  ',I8,' HIGH RUN ',I8,' RUN GENERATED ',I8/
     A             ' DATE GENERATED ',I10,' TIME GENERATED ',I10/
     A             ' MODULE NUMBER  ',I4,
     A             ' FLAGS ',4(2X,Z8)/
     A             ' NUMBER BAD ',I4 )
            WRITE(PRUNIT,152)
  152       FORMAT('   MOD  PLN  WIR PdAE PdAO PdBE PdBO Tim1',
     &             ' Tim2 DlT1 DlT2 PdlE PdlO Hard' )
            COBANK = IC(KMBAD(ITREE)+14)         ! number of entries in bank
            DO 300 NUMADD = 1,COBANK
              ADDRESS = IC(KMBAD(ITREE)+12+3*NUMADD)
              CALL MUADD(ADDRESS,MODUL,PLANE,WIRE,IERR)
              FLAG = IC(KMBAD(ITREE)+12+3*NUMADD+1)
              DO NCHAN=1,6
                CHAN = ISHFT(FLAG,OFFSET(NCHAN))
                CHAN = IAND(CHAN,CHMASK(NCHAN))
                IF (CHAN.EQ.0) THEN
                  CHCHAN(NCHAN) = BLANKS
                ELSE
                  WRITE(CHCHAN(NCHAN),157) CHAN
  157             FORMAT(3X,Z2)
                ENDIF
              ENDDO
              FLAG = IC(KMBAD(ITREE)+12+3*NUMADD+2)
              DO NCHAN=7,10
                CHAN = ISHFT(FLAG,OFFSET(NCHAN))
                CHAN = IAND(CHAN,CHMASK(NCHAN))
                IF (CHAN.EQ.0) THEN
                  CHCHAN(NCHAN) = BLANKS
                ELSE
                  WRITE(CHCHAN(NCHAN),157) CHAN
                ENDIF
              ENDDO
              HARD = ISHFT(FLAG,OFFSET(11))
              HARD = IAND(HARD,CHMASK(11))
              WRITE(PRUNIT,153)MODUL,PLANE,WIRE,(CHCHAN(I),I=1,10),
     &                    HARD 
  153         FORMAT(3X,I3,3X,I1,2X,I2,10A,2X,Z4)
  300       CONTINUE                        ! NUMADD
          ENDIF                         ! if kmbad.ne.0
        ENDDO                           ! tree loop
  500 CONTINUE                          ! MODSEQ

C
  999 RETURN
      END
