      SUBROUTINE DODIGI(LSET,ETOT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : retrieves stored digi info and makes sums
C-
C-   Inputs  : LSET = SRCP array of detector set info
C-   Outputs : ETOT = total energy deposited in this detector set
C-   Controls: None
C-
C-   Created  22-JAN-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$INC:GCSETS.INC'
C
      INTEGER LSET(*)
      INTEGER IS
      INTEGER NSET,NV,NH,ND,IOFF
      INTEGER IARR(5000)
      REAL ARR(5000)
      EQUIVALENCE ( IARR,ARR)   !Change this to a Zebra bank later
      INTEGER NTRA,NUMVS,ITRS,NHMAX,ITRA,INUMBV,IHITS,NHITS,I,
     &  LTRA,IDIG
      REAL ETOT
      CHARACTER*4 ICSET,ICDET 
C ARGUMENTS TO GFDET
      CHARACTER*4 NAMESV(5)
      INTEGER NVF,NBITSV(5),IDTYPEF,NWHI,NWDI
C
      DATA NTRA/1/     !only one track per hit
      DATA NUMVS/0/    !use all valid volume numbers
C----------------------------------------------------------------------
      IHSET = LSET(1)
      ITRS  = LSET(4)    !Track number to get hits for=0 means all tracks
      NHMAX = LSET(5)   !Maximum number of hits to get.
      NSET = LSET(6)
      ETOT = 0.
      IOFF = 6
      DO 40 IS = 1 , NSET
        IHDET = LSET(IOFF+1)
        NV = LSET(IOFF+3)
C
        IOFF = IOFF+4+2*NV
        NH = LSET(IOFF)
C
        ITRA = 1             !pointer to ITRA storage
        INUMBV = ITRA+NHMAX  !pointer to NUMBV storage
        IHITS = INUMBV + NV*NHMAX    !pointer to storage of hits
C
        CALL UHTOC(IHSET,4,ICSET,4)
        CALL UHTOC(IHDET,4,ICDET,4)
        CALL GFHITS(ICSET,ICDET,NV,NH,NHMAX,ITRS,NUMVS,IARR(ITRA),
     &    IARR(INUMBV),ARR(IHITS),NHITS)
C
C ****  PRINT HITS IF ON SWITCH
C
        IF(NHITS.GT.0.AND.DHIT.EQ.1)CALL GPHITS(ICSET,ICDET)
C
C ****  NEED to Look up ISET and IDET that go with IHSET and IHDET
C
        CALL GFDET(ICSET,ICDET,NVF,NAMESV,NBITSV,IDTYPEF
     +                ,NWHI,NWDI,ISET,IDET)
        DO 50 I = 1,NHITS
          LTRA = IARR(ITRA+I-1)
C
C ****  BUMP HITS INDEX SO ENERGY IS LAST HIT
C
          ETOT = ETOT + ARR(IHITS+I*NH -1)    !Assume Energy = last Hit
C                                        ! if more than one hit stored

C          CALL GSDIGI(ISET,IDET,LTRA,NTRA,IARR(INUMBV),
C     &      ARR(IHITS+(I-1)*NH),IDIG)
C
C ****  ARR SHOULD BE CHANGED TO AN INTEGER SCALED VALUE FOR
C ****  DIGITIZATION TO WORK. PERHAPS, IF AN WHEN WE PUT CELLS
C ****  INTO THE GEOMETRY, WE CAN ADD ADC VALUES.
C
   50   CONTINUE
C        IF(DDIG.EQ.1)CALL GPDIGI(ICSET,ICDET)
C
        IOFF = IOFF+1+4*NH
        ND = LSET(IOFF)
        IOFF = IOFF+2*ND
   40 CONTINUE
  999 RETURN
      END
