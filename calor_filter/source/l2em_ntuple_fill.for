      SUBROUTINE l2em_ntuple_fill()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills ntuple with L2_EM filter variables
C-                         from L2EM ZEBRA bank
C-                         ntuple = USR$OUT:L2EM_'RUN_NUM'.NTUP
C-                         'RUN_NUM' = 1st run seen
C-   Inputs  : none
C-   Outputs : NTUPLE words
C-   Controls: none
C-
C-   Created  10-SEP-1992   James T. McKinley
C-   Updated   8-FEB-1993   James T. McKinley - Add selection by filter name.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'd0$inc:zebcom.inc'
      INCLUDE 'd0$inc:zlinka.inc'
      INCLUDE 'd0$params:l2em_ntuple.def'
C
      LOGICAL newparset,all,first,ok
      LOGICAL by_name,pass_trig,pass_filt,filt_only,newevt
      LOGICAL trigpass,filtpass,parpass,l1name_passed,l2name_passed
      DATA first/.TRUE./,newparset/.FALSE./,all/.TRUE./,ok/.TRUE./
      DATA by_name/.FALSE./,pass_trig/.FALSE./,pass_filt/.FALSE./
      DATA filt_only/.FALSE./,newevt/.TRUE./,trigpass/.FALSE./
      DATA trigpass/.FALSE./,filtpass/.FALSE./,parpass/.FALSE./
      INTEGER gzl2em,gzisae,ll2em,lisae,ier,ntrig,nfilt
      INTEGER num_of_events,runno,runnum/0/,runranges(2,NMAX),versions
      DATA runranges/NMAX*0,NMAX*0/,versions/0/,ntrig/0/,nfilt/0/
      COMMON/nevt/num_of_events
      INTEGER trulen,length,vi,sal/32/
      INTEGER i,j,k,l,lindx,nps_found,parm_set_found(NMAX)
      INTEGER ngot,ncand,parm_set(NMAX),ncandps(NMAX),nsets,psindx
      DATA nps_found/0/,ngot/0/,ncand/0/,parm_set/NMAX*0/
      DATA parm_set_found/NMAX*0/,ncandps/NMAX*0/,nsets/0/,psindx/0/
      REAL ntuple(NTUPLE_SIZE)
      REAL l1_em_et,l1_tot_et,event_weight/1.0/ ! Assume data
      REAL fast_z,fast_flag,slow_z,slow_flag
      CHARACTER*8 userid/'l2emntup'/,mapnames(NMAX)
      CHARACTER*32 trig(NMAX),filt(NMAX),filt_table(NMAX,NMAX),name
C
      SAVE trigpass,filtpass,newevt
C----------------------------------------------------------------------
C                                     ! this is necessary to make sure
      IF( runnum .NE. runno() )THEN   ! that first event's weight is
        first = .TRUE.                ! not set to 1 for each run if
        runnum = runno()              ! weight from ISAE is zero, and still
      ENDIF                           ! allow setting single track weights
C                                     ! to 1, since always 0 from ISAJET
C the first single track event will have the last weight from the previous
C run.  if no previous run the weight will be 1.0 since data is assumed
      lisae=gzisae()
      IF( lisae .GT. 0 )THEN           ! get MC event weight for this event
        IF( q(lisae + 12) .EQ. 0.0 .AND. .NOT.first)THEN 
            event_weight = 1.0         ! handle single tracks
        ELSE                           ! or last event's weight if ISAE is FUBAR
          event_weight = q(lisae+12)   ! event weight from ISAJET
        ENDIF                          
      ELSE                             ! use 1.0 if ISAE bank not present
        event_weight = 1.0
      ENDIF
      first = .false.
C                                   
      ll2em=gzl2em()                   ! get link to L2EM bank
      IF(ll2em.LE.0) GOTO 999          ! bug out if no bank
      CALL ztopsy(ixcom,ll2em)         ! invert linear structure so L2EM bank
C                                      ! is ordered in Et for each script
      ncand = iq(ll2em+3)
C
      DOWHILE( ll2em .GT. 0 )                   ! loop over linear chain
C
        l = 0                                   ! zero repetition index for
C                                               ! new bank in linear chain
        DO i=1,ncand                            ! Loop over all candidates in
C                                               ! this bank (repetition bank)
C
          IF( all ) GOTO 10                     ! fill with all parameter sets
C
          IF( by_name )THEN
C
            IF( newevt )THEN ! only need to check once/event

              newevt   = .FALSE.
              trigpass = .FALSE.
              filtpass = .FALSE.
C
              IF(pass_trig)THEN     ! check if event passed requested triggers
                DO j=1,ntrig
                  length = MIN(trulen(trig(j)),32)
                  IF(trig(j)(length:length).EQ.'*')THEN ! remove wildcards
                    name=trig(j)(1:(length-1))
                  ELSE
                    name=trig(j)
                  ENDIF
                  IF(l1name_passed(name)) trigpass = .TRUE.
                ENDDO
              ELSE
                trigpass = .TRUE.
              ENDIF
C
              IF(pass_filt)THEN       ! check if event passed requested filters
                DO j=1,nfilt
                  length = MIN(trulen(filt(j)),32)
                  IF(filt(j)(length:length).EQ.'*')THEN ! remove wildcards
                    name=filt(j)(1:(length-1))
                  ELSE
                    name=filt(j)
                  ENDIF
                  IF(l2name_passed(name)) filtpass = .TRUE.
                ENDDO
              ELSE
                filtpass = .TRUE.
              ENDIF
C
            ENDIF
C
            parpass  = .FALSE.
            IF(filt_only)THEN           ! must always check if requested
              DO j = 1,versions         ! use proper mapping table for version
                IF( runnum.GE.runranges(1,j) .AND.
     &              runnum.LE.runranges(2,j) )THEN
                  vi = j
                ENDIF
              ENDDO
              DO j=1,nfilt        ! check if parameter set number corresponds
                DO k = 1,NMAX     ! to a requested filter name
                  length = MIN(trulen(filt(j)),32)
                  IF(filt(j)(length:length).EQ.'*')THEN  ! allow use of partial
                    length = length-1             ! names if user wants, 
                  ELSE                            ! but exact match otherwise
                    length = MIN(MAX(trulen(filt_table(k,vi)),
     &                trulen(filt(j))),32)
                  ENDIF
                  IF(filt(j)(1:length).EQ.filt_table(k,vi)(1:length))
     &              THEN
                    IF(iq(ll2em+29+l).eq.k)THEN
                      parpass = .TRUE.
                      GOTO 40
                    ENDIF
                  ENDIF
                ENDDO
              ENDDO
            ELSE
              parpass = .TRUE.
            ENDIF
C
   40       IF(trigpass.AND.filtpass.AND.parpass) GOTO 10  ! fill w/ this cand.
            GOTO 30                           ! bug out if candidate not
C                                             ! selected by user's criteria
C
          ELSE                                ! allow direct parameter set #'s
            DO j=1,nsets                      ! only fill with requested sets
              IF( iq(ll2em+29) .EQ. parm_set(j) )THEN
                GOTO 10
              ENDIF
            ENDDO
          ENDIF
C
          GOTO 30                                  ! set not requested
C
   10     CALL cl1phet(iq(ll2em+5+l),iq(ll2em+4+l),l1_em_et,l1_tot_et)
C
          ntuple(1) = runno()                      ! run number
          ntuple(2) = l1_tot_et
          ntuple(3) = l1_em_et                     ! L1 info
C
          DO j = 4,8
            ntuple(j) = FLOAT( iq(ll2em + j + l) ) ! TT eta,phi and 
          ENDDO                                    ! L2 eta,phi,lyr
          DO j = 9,26
            ntuple(j) = q(ll2em + j + l)           ! L2_EM filter cut variables
          ENDDO                                    ! shower shape and isolation
          DO j = 27,29
            ntuple(j) = FLOAT( iq(ll2em + j + l) ) ! NTRAK, IFAILED, PAR_SET
          ENDDO
C
          newparset = .TRUE.                       ! deal w/ multiple parameter
          DO j = 1,nps_found                       ! sets
            IF( iq(ll2em + 29) .EQ. parm_set_found(j) )THEN
              ncandps(j) = ncandps(j) + 1        ! number of candidates per set
              newparset = .FALSE.                ! have seen this set this event
              psindx = j                         ! array index of set
              GOTO 20
            ENDIF
          ENDDO
   20     CONTINUE
          IF( newparset )THEN                          ! new set this event
            nps_found = nps_found + 1                  ! number sets this event
            parm_set_found(nps_found) = iq(ll2em+29+l) ! parameter set number
            psindx = nps_found                         ! array index of set
            ncandps(psindx) = ncandps(psindx) + 1    ! number candidates per set
          ENDIF
C
          IF(iq(ll2em+1).gt.1)THEN                ! check version number
            DO j = 30,35                          ! AETA,APHI,X,Y,Z,ET_ZCORR
              ntuple(j) = q(ll2em + j + l)
            ENDDO
          ENDIF
C
          IF(iq(ll2em+1).gt.2)THEN                ! check version number
            ntuple(36) = FLOAT(iq(ll2em+36+l))    ! CUTBITS
          ENDIF
C
          IF(iq(ll2em+1).gt.3)THEN
            ntuple(37) = q(ll2em + 37 + l)
          ENDIF
C
          CALL gtl0vt(fast_z,fast_flag,slow_z,slow_flag,ok)
C
C slow_flag=1 --> single interaction, slow_flag=2 --> 60% single interaction, 
C slow_flag=3 --> 60% multiple interaction, slow_flag=4 --> multiple interaction
C
          IF(ok)THEN                      ! L0 vertex info
            IF(slow_flag.NE.0)THEN        
              ntuple(38) = slow_z
              ntuple(39) = slow_flag      ! multiple interaction flag
              ntuple(40) = 2.0            ! VTX_TYPE = 2 --> slow Z
            ELSEIF(fast_flag.NE.2)THEN
              ntuple(38) = fast_z
              ntuple(39) = fast_flag      ! fast Z good, redundant w/ ZTYPE
              ntuple(40) = 1.0            ! VTX_TYPE = 1 --> fast Z
            ENDIF                         ! VTX_TYPE = 0 --> no vertex info
          ENDIF
C
          ntuple(41) = ncandps(psindx)    ! candidate number for this set
          ntuple(42) = iq(lhead + 9)      ! event number
          ntuple(43) = event_weight       ! MC event weight, 1.0 for data
C
          CALL grlink(userid,lindx)       ! reserve zebra link so PAW
          lrlink(lindx)=ll2em             ! doesn't trash it if a garbage
C                                         ! collection is triggered.
          CALL hcdir('//PAWC',' ')
          CALL hcdir('//NTUPLE',' ')
          CALL hfn(4410,ntuple)           ! fill
          CALL hcdir('//PAWC',' ')
C
          ll2em = lrlink(lindx)           ! get link back from ZLINKA common
          CALL rrlink(userid,lindx)       ! release link space so others can use
C
          CALL vzero(ntuple(1),NTUPLE_SIZE) !zero ntuple array for next cand.
   30     CONTINUE 
          l = l + iq(ll2em + 2)           ! increment repetition index
        ENDDO                             ! ncand=1 --> linear chain --> 1 loop 
C
        ll2em = lq(ll2em)                 ! get link to next bank
      ENDDO
  999 RETURN
C
C#######################################################################
      ENTRY fill_reset()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : set up ntuple filling variables for next event
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  10-SEP-1992   James T. McKinley
C-   Updated  11-FEB-1992   James T. McKinley - Add filter name selection
C-
C----------------------------------------------------------------------
      CALL vzero(ntuple(1),NTUPLE_SIZE)
      CALL vzero_i(parm_set_found,NMAX)
      CALL vzero_i(ncandps,NMAX)
      nps_found = 0
      newevt = .TRUE.
      RETURN
C
C#######################################################################
      ENTRY l2em_ntuple_init
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read in L2EM_NTUPLE_RCP parameters
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  10-SEP-1991   James T. McKinley
C-   Updated   8-FEB-1991   James T. McKinley - Add trigger/filter name
C-                          selection parameters to RCP file.
C-
C----------------------------------------------------------------------
      CALL ezpick('L2EM_NTUPLE_RCP')              
      CALL ezget_l('ALL',all,ier)                           ! fill w/ all
      CALL ezget_l('BY_NAME',by_name,ier)                   ! name switch
      CALL ezget_iarr('PARM_SET',parm_set,ier)                 ! P.S. # switch
      CALL ezget_i('NSETS',nsets,ier)                       ! # of P.S.'s
      CALL ezget_l('PASS_TRIG',pass_trig,ier)               ! pass trigger
      CALL ezget_l('PASS_FILT',pass_filt,ier)               ! pass filter
      CALL ezget_l('FILT_ONLY',filt_only,ier)               ! name only switch
      CALL ezget_i('NTRIG',ntrig,ier)                       ! # trigger names
      CALL ezget_i('NFILT',nfilt,ier)                       ! # filter names
      CALL ez_get_chars('TRIG',sal,trig(1),ier)           ! trigger names
      CALL ez_get_chars('FILT',sal,filt(1),ier)           ! filter names
      CALL ezget('VERSIONS',versions,ier)                 ! # trigger menus
      CALL ezget('RUN_RANGES',runranges(1,1),ier)         ! runs for menus
      CALL ez_get_chars('MAP_NAMES',sal,mapnames(1),ier)  ! map table names
      DO i=1,versions                                     ! get mapping tables
        length = min(trulen(mapnames(i)),8)               ! for each menu
        CALL ez_get_chars(mapnames(i)(1:length),sal,filt_table(1,i),ier)
      ENDDO
      CALL ezrset
      RETURN
      END
