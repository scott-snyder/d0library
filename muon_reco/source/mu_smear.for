      FUNCTION MU_SMEAR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   1-MAR-1993   Tom Diehl
C-   Modified 16-APR-1993   Paul Quintas. Subtract MC DT0RES
C-   Modified  9-JUN-1993   Tom Diehl. Fixed bug which caused Joey's problem
C-   Modified 18-MAY-1994   Paul Quintas. Change names of called subroutines
C-			     to MGEH_FLG and MU_SMUO_SAVE
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMUD1.LINK'
      INCLUDE 'D0$LINKS:IZUSER.LINK'

      INTEGER IERR,I,J,K,ISEED             ! Various integers.
      INTEGER IFLG                         ! Geometry smeared flag.
      INTEGER IVERS                        ! Data version. 
      INTEGER LMUD1_NEW,LCRATE,PRVMACWDS   ! Old data pointers.
      INTEGER LMUD1,GZMUD1                 ! Muon raw data bank.
      INTEGER LMUHT,GZMUHT,LMTRH,GZMTRH    ! Previous related links. 
      INTEGER NMOD,WORDCOUNT,HEADERLOC     ! New MUD1 pointers.
      INTEGER NHEADW,NDATAW,NHITS,NWORDS   ! Things about old crate MUD1.
      INTEGER ICRID,NMACS                  ! More things about crates.
      INTEGER NEWCRAWDS,MACWORDSUM         ! Things about new crates.
      INTEGER PED,MUNMOD2,LFREE
      INTEGER NLATCHES,KILLLATCH,IADDRESS

      LOGICAL MU_SMEAR,FIRST,MGEH_FLG,LDUMMY,DROP_MTRH
      REAL TEMPDAT(8),RAN,SMEAR_TIME,SMEAR_DTIME,DT_MC
      REAL SEED1,SEED2,RANDOM

      DATA PED /350/
      DATA FIRST /.TRUE./
      DATA ISEED /323443/, SEED1 /0.32/, SEED2 /999./
      DATA DT_MC /12.7/
C----------------------------------------------------------------------
      MU_SMEAR = .FALSE.
      IF(IQ(LHEAD+1).LT.1000) THEN
        CALL ERRMSG('Not allowed to smear data in ','MUSMEAR'
     +            ,'today or any other day.','W')
        MU_SMEAR = .TRUE.
        GOTO 999
      ENDIF
      IF(FIRST) THEN
        FIRST = .FALSE.
        CALL INZCOM(2)                     ! Initialize Zebra data banks
        CALL INZSTP                        ! Initialize Zebra STP banks
        CALL INRCP ('MU_SMEAR_RCP',IERR)
        IF(IERR.NE.0) THEN
          CALL ERRMSG('Error return from S/R INRCP','MUSMEAR'
     +            ,'stop processing.','F')
          STOP
        ENDIF
        CALL EZPICK('MU_SMEAR_RCP')
        CALL EZGET('DROP_MTRH',DROP_MTRH,IERR)
        IF(IERR.NE.0) CALL ERRMSG('Error return in S/R MUSMEAR',
     +                'MUSMEAR' ,'stop processing.','F')
        NMOD = 0                           ! First call to set it all up.
        CALL MU_SMEAR_GETDATA(NMOD,TEMPDAT,IERR)
        CALL EZRSET
        CALL NORRUT(SEED1,SEED2)           ! Set NORRAN seeds.
      ENDIF

      IF(.NOT.MGEH_FLG(0)) THEN            ! Not smeared geometry.
        DO I = 1, 164                      ! Offset the geometry by the value
          NMOD = MUNMOD2(1,I)              !     of TEMPDAT(8) (Drift Dir).
          CALL MU_SMEAR_GETDATA(NMOD,TEMPDAT,IERR)
          CALL MGEOSMEAR(NMOD,TEMPDAT(8),IERR)
        ENDDO
        LDUMMY = MGEH_FLG(1)               ! Set flag which says smeared.
	CALL MU_SMUO_SAVE                  ! Write out the smeared geometry
      ENDIF

      LMUD1 = GZMUD1(0)
      IF(LMUD1.EQ.0) THEN
        CALL ERRMSG('Mud1 missing in origonal file','MUSMEAR'
     +            ,'not much use.','W')
        GOTO 999
      ENDIF
C                                          ! Punt previous related links.
      LMUHT = GZMUHT(0)
      IF(LMUHT.NE.0) CALL MZDROP(IXMAIN,LMUHT,' ')
      LMTRH = GZMTRH(0)
      IF(LMTRH.NE.0.AND.DROP_MTRH) CALL MZDROP(IXMAIN,LMTRH,' ')
      LFREE = LQ(LHEAD-IZUSER)             ! Screw anybody who used USER link.
      IF(LFREE.NE.0) CALL MZDROP(IXMAIN,LFREE,' ')

      LMUD1 = GZMUD1(0)                    ! Just being careful.
      NWORDS = IQ(LMUD1-1)                 ! Total words in origonal MUD1.
C                                          ! Copy MUD1 to free-for-all link.
      CALL MZCOPY(IXMAIN,LMUD1,IXMAIN,LHEAD,-IZUSER,' ') 
      CALL MZDROP(IXMAIN,LMUD1,' ')        ! Drop old MUD1
C
C                                          ! Create MUD1 space.
      CALL MZBOOK(IXMAIN,LMUD1,LHEAD,-IZMUD1,'MUD1',0,0,
     +  NWORDS,2,-1)

      LMUD1_NEW = LQ(LHEAD-IZUSER)         ! Location of MUD1 copy.
      LMUD1 = LQ(LHEAD-IZMUD1)             
       
      WORDCOUNT = 0                        ! Running new MUD1 wordcount.

      LCRATE = LMUD1_NEW                   ! Try to function a la MUSRT1.
      IVERS = IAND(IQ(LCRATE+4),65535)  

      IF(IVERS.EQ.1) THEN                  ! Laziness factor in action.
        CALL ERRMSG('This MC is pre 8-91. Too old for: ','MUSMEAR.'
     +             ,' Sorry.','W')
        CALL MZCOPY(IXMAIN,LMUD1_NEW,IXMAIN,LHEAD,-IZMUD1,' ') 
        CALL MZDROP(IXMAIN,LMUD1_NEW,' ')  ! Drop copy MUD1
        MU_SMEAR = .TRUE.
        GOTO 999
      ELSEIF(IVERS.EQ.2823) THEN
        CALL ERRMSG('Unusual MUD1 version # 2823.','MUSMEAR ','punts.',
     +              'W')
        CALL MZCOPY(IXMAIN,LMUD1_NEW,IXMAIN,LHEAD,-IZMUD1,' ') 
        CALL MZDROP(IXMAIN,LMUD1_NEW,' ')  ! Drop copy MUD1
        MU_SMEAR = .TRUE.
        GOTO 999
      ENDIF

      IQ(LMUD1 + WORDCOUNT) = 0            ! STA IQ   0 = 0
      WORDCOUNT = WORDCOUNT + 1            ! This was a fudge.

  756 IF(NWORDS-16.LE.LCRATE-LMUD1_NEW) THEN
        IF(IQ(LHEAD+1).GT.1000) GOTO 621   ! Kludge because MC MUD1 
C                   doesn't have the MUD1 trailer as of March 1993. 
        DO I = 1,16                        ! All done except for:
          IQ(LMUD1+WORDCOUNT) = IQ(LCRATE+I)
          WORDCOUNT = WORDCOUNT + 1        ! MUD1 trailer.
        ENDDO                      
        GOTO 621
      ENDIF
          
      ICRID = ISHFT(IQ(LCRATE+3),-24)
      IF(ICRID.GT.180) THEN                !Samus Crate.
        NHEADW = IQ(LCRATE+1) + 1
        NDATAW = IQ(LCRATE+NHEADW)
        DO I = 1, NHEADW + NDATAW + 5
          IQ(LMUD1+WORDCOUNT)=IQ(LCRATE+I) !SAMUS is so shiny it never 
          WORDCOUNT = WORDCOUNT + 1        !  gets trashed.
        ENDDO
        LCRATE = LCRATE + NHEADW + NDATAW + 5 
        GOTO 756
      ELSE                                 !Wamus Crate.
        NHEADW = IQ(LCRATE+1) + 1
        NDATAW = IQ(LCRATE+NHEADW)
        NMACS = IAND(ISHFT(IQ(LCRATE+3),-16),255)
        HEADERLOC =  WORDCOUNT 
        DO I = 1,NHEADW                    !We'll rewrite these later.
          IQ(LMUD1 + WORDCOUNT) = IQ(LCRATE +I)
          WORDCOUNT = WORDCOUNT + 1
        ENDDO           
        NEWCRAWDS = 0                      !Running count new data words 
        PRVMACWDS = 0                      !For old data pointer.
        DO I = 1, NMACS                     
          MACWORDSUM = 0             
          NHITS = IAND(IQ(LCRATE +6 +I),4095)/9  
          IF(NHITS.EQ.0) GOTO 807
          NMOD = ISHFT(IQ(LCRATE +6 +I),-16)         
          CALL MU_SMEAR_GETDATA(NMOD,TEMPDAT,IERR)
          IF(RAN(ISEED).LT.TEMPDAT(1)) THEN
            DO J = 1,NHITS                 !PDT is turned on for today.
              K = J - 1                    !Cell (pair) we are testing.
              NLATCHES = 0                 !Count the latches.
              KILLLATCH = 0                !How many latches we killed.
              IADDRESS = IQ(LCRATE+NHEADW+PRVMACWDS+K*9+1)
              IF(BTEST(IADDRESS,31)) THEN
                NLATCHES = NLATCHES + 1
                IF(RAN(ISEED).GT.TEMPDAT(2)) THEN 
                  KILLLATCH = KILLLATCH + 1
                  IADDRESS = IBCLR(IADDRESS,31)
                ENDIF
              ENDIF
              IF(BTEST(IADDRESS,30)) THEN
                NLATCHES = NLATCHES + 1
                IF(RAN(ISEED).GT.TEMPDAT(2)) THEN 
                  KILLLATCH = KILLLATCH + 1
                  IADDRESS = IBCLR(IADDRESS,30)
                ENDIF
              ENDIF
              IF(NLATCHES-KILLLATCH.GT.0) THEN  
                IQ(LMUD1+WORDCOUNT) = IADDRESS
                WORDCOUNT = WORDCOUNT + 1  !Address word.
C
                SMEAR_TIME = TEMPDAT(4) * 410.

                IF(IQ(LCRATE+NHEADW+PRVMACWDS+K*9+2).GT.PED+50) THEN
                  IF(RAN(ISEED).GT.TEMPDAT(3)) THEN 
                    IQ(LMUD1+WORDCOUNT) = 4095
                  ELSE                     !T2 word.
                    CALL NORRAN(RANDOM)
                    IQ(LMUD1+WORDCOUNT) = 
     $             IQ(LCRATE+NHEADW+PRVMACWDS+K*9+2) +RANDOM*SMEAR_TIME
                  ENDIF
                ELSE
                  IQ(LMUD1+WORDCOUNT)= IQ(LCRATE+NHEADW+PRVMACWDS+K*9+2)
                ENDIF                      
                WORDCOUNT = WORDCOUNT + 1

                IF(IQ(LCRATE+NHEADW+PRVMACWDS+K*9+3).GT.PED+50) THEN
                  IF(RAN(ISEED).GT.TEMPDAT(3)) THEN
                    IQ(LMUD1+WORDCOUNT) = 4095
                  ELSE                     !T1 word.
                    CALL NORRAN(RANDOM)
                    IQ(LMUD1+WORDCOUNT) = 
     $             IQ(LCRATE+NHEADW+PRVMACWDS+K*9+3) +RANDOM*SMEAR_TIME
                  ENDIF
                ELSE
                  IQ(LMUD1+WORDCOUNT)= IQ(LCRATE+NHEADW+PRVMACWDS+K*9+3)
                ENDIF                      
                WORDCOUNT = WORDCOUNT + 1
C
                IQ(LMUD1+WORDCOUNT) = IQ(LCRATE+NHEADW+PRVMACWDS+K*9+4)
                WORDCOUNT = WORDCOUNT + 1  !P1 word.
                IQ(LMUD1+WORDCOUNT) = IQ(LCRATE+NHEADW+PRVMACWDS+K*9+5)
                WORDCOUNT = WORDCOUNT + 1  !P2 word.
C
C                SMEAR_DTIME = 0.8333 * TEMPDAT(6)
		IF (TEMPDAT(6).GT.DT_MC) THEN
		  SMEAR_DTIME = 1.6667 * SQRT(TEMPDAT(6)**2-DT_MC**2)
		ELSE
		  SMEAR_DTIME = 0.
		ENDIF

                IF(IQ(LCRATE+NHEADW+PRVMACWDS+K*9+6).GT.PED+50) THEN
                  IF(RAN(ISEED).GT.TEMPDAT(5)) THEN
                    IQ(LMUD1+WORDCOUNT) = 4095
                  ELSE                     !DT1 word.
                    CALL NORRAN(RANDOM)
                    IQ(LMUD1+WORDCOUNT) = 
     $             IQ(LCRATE+NHEADW+PRVMACWDS+K*9+6) +RANDOM*SMEAR_DTIME
                  ENDIF
                ELSE
                  IQ(LMUD1+WORDCOUNT)= IQ(LCRATE+NHEADW+PRVMACWDS+K*9+6)
                ENDIF                      
                WORDCOUNT = WORDCOUNT + 1
C
                IF(IQ(LCRATE+NHEADW+PRVMACWDS+K*9+7).GT.PED+50) THEN
                  IF(RAN(ISEED).GT.TEMPDAT(5)) THEN
                    IQ(LMUD1+WORDCOUNT) = 4095
                  ELSE                     !DT2 word.
                    CALL NORRAN(RANDOM)
                    IQ(LMUD1+WORDCOUNT) = 
     $             IQ(LCRATE+NHEADW+PRVMACWDS+K*9+7) +RANDOM*SMEAR_DTIME
                  ENDIF
                ELSE
                  IQ(LMUD1+WORDCOUNT)= IQ(LCRATE+NHEADW+PRVMACWDS+K*9+7)
                ENDIF                      
                WORDCOUNT = WORDCOUNT + 1
C 
                IQ(LMUD1+WORDCOUNT) = IQ(LCRATE+NHEADW+PRVMACWDS+K*9+8)
                WORDCOUNT = WORDCOUNT + 1  !P3 word.
                IQ(LMUD1+WORDCOUNT) = IQ(LCRATE+NHEADW+PRVMACWDS+K*9+9)
                WORDCOUNT = WORDCOUNT + 1  !P4 word.
                MACWORDSUM = MACWORDSUM + 9!Keep track of new MAC words.
C
              ENDIF
            ENDDO
          ENDIF                            !Keep track of datawords in crate.
 807      CONTINUE
          NEWCRAWDS = NEWCRAWDS + MACWORDSUM  
          IQ(LMUD1 +HEADERLOC +6 +I-1) = IQ(LMUD1+HEADERLOC+6+I-1) 
     $      - NHITS*9 + MACWORDSUM         !Fix the MAC header bits.
          PRVMACWDS = PRVMACWDS + NHITS*9
        ENDDO                              !Ith MAC is done.
        IQ(LMUD1 +HEADERLOC +NHEADW  -1) = NEWCRAWDS
        DO I = 1,5                         !Crate Trailer. 
          IQ(LMUD1+WORDCOUNT) = IQ(LCRATE+NHEADW+NDATAW+I)
          WORDCOUNT = WORDCOUNT + 1
        ENDDO
        LCRATE = LCRATE + NHEADW + NDATAW + 5 
        GOTO 756
      ENDIF

  621 I = NWORDS + 1 - WORDCOUNT         ! The + 1 is a fudge needed 
C                                          because of STA 0 = 0 fudge above.
      CALL MZDROP(IXMAIN,LMUD1_NEW,' ')  ! Drop copy MUD1
      LMUD1 = LQ(LHEAD-IZMUD1)           ! Just being careful.
      CALL MZPUSH(IXMAIN,LMUD1,0,-I,' ')
      MU_SMEAR = .TRUE.  

  999 RETURN
      END
