      FUNCTION TB90L2_ICD_HIST()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Processes events
C-   Called from EXM_DO_ANALYSIS hook of d0$examine2
C-
C-   Returned value  : true
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   5-AUG-1991   Kaushik De
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      real icdmax, hade, icde, mhmax

      LOGICAL tb90l2_icd_hist
      CHARACTER*80 msg                  ! holds error messages
      INTEGER nv                        ! bank version number
      INTEGER nr                        ! repeat count
      INTEGER nch                       ! number of chanels from caep bank
      LOGICAL first                     ! true if first time in
      LOGICAL firstcall                 ! true at start of each event
      INTEGER ieta,iphi,layer           ! hit coord froom caep bank
      INTEGER bits                      ! bit mask from caep bank
      real energy, total_energy
      real eta, phi
      real inteta, intphi
      integer gang_eta(9:14, 31:35)
      integer gang_phi(9:14, 31:35)
      INTEGER ichan                     ! channel number
      INTEGER num_of_events             ! total number of events
      INTEGER ier                       ! error flag
      INTEGER i
      DATA num_of_events/0/
      DATA first/.true./
      data gang_eta /13, 14, 14, 12,  9, 11,
     +                9, 10, 11, 12, 13, 10,
     +                9, 10, 11, 12, 13, 14,
     +                9, 10, 11, 12, 13, 14,
     +                9, 10, 11, 12, 13, 14/
      data gang_phi /35, 34, 35, 31, 35, 35,
     +               32, 32, 32, 32, 32, 35,
     +               33, 33, 33, 33, 33, 33,
     +               34, 34, 34, 34, 34, 34,
     +               35, 35, 35, 35, 35, 35/
C----------------------------------------------------------------------
C
C ****  init stuff
C
      IF (first) THEN
        first = .false.

         CALL HCDIR ('\\\\\\\\\\\\\\\\\',' ')
         call hbook1(100,'Total ICD Energy$',200,0.,40000.,0.)
         call hbook2(101,'Total ICD Ener vs. Total MH+FH+CH+OH Ener$',
     +            100,0.,40000.,100,0.,40000.,0.)
         call hbook1(102,'Total MH+FH+CH+OH Energy$',200,0.,40000.,0.)
         call hbook1(103,'Total ICD+MH+FH+CH+OH Ener$',200,0.,40000.,0.)
         call hbook1(104,'ICD Central ADC Dist.$',300,0.,30000.,0.)
         call hbook1(105,'ICD Central ADC Dist.$',200,0.,200.,0.)

         call hbook1(932,'ICD Energy 9, 32$',300,0.,30000.,0.)
         call hbook1(933,'ICD Energy 9, 33$',300,0.,30000.,0.)
         call hbook1(934,'ICD Energy 9, 34$',300,0.,30000.,0.)
         call hbook1(935,'ICD Energy 9,35/13,31$',300,0.,30000.,0.)
         call hbook1(1032,'ICD Energy 10, 32$',300,0.,30000.,0.)
         call hbook1(1033,'ICD Energy 10, 33$',300,0.,30000.,0.)
         call hbook1(1034,'ICD Energy 10, 34$',300,0.,30000.,0.)
         call hbook1(1035,'ICD Energy 10,35/14,32$',300,0.,30000.,0.)
         call hbook1(1132,'ICD Energy 11, 32$',300,0.,30000.,0.)
         call hbook1(1133,'ICD Energy 11, 33$',300,0.,30000.,0.)
         call hbook1(1134,'ICD Energy 11, 34$',300,0.,30000.,0.)
         call hbook1(1135,'ICD Energy 11,35/14,31$',300,0.,30000.,0.)
         call hbook1(1231,'ICD Energy 12, 31$',300,0.,30000.,0.)
         call hbook1(1232,'ICD Energy 12, 32$',300,0.,30000.,0.)
         call hbook1(1233,'ICD Energy 12, 33$',300,0.,30000.,0.)
         call hbook1(1234,'ICD Energy 12, 34$',300,0.,30000.,0.)
         call hbook1(1235,'ICD Energy 12, 35$',300,0.,30000.,0.)
         call hbook1(1332,'ICD Energy 13, 32$',300,0.,30000.,0.)
         call hbook1(1333,'ICD Energy 13, 33$',300,0.,30000.,0.)
         call hbook1(1334,'ICD Energy 13, 34$',300,0.,30000.,0.)
         call hbook1(1335,'ICD Energy 13,35/9,31$',300,0.,30000.,0.)
         call hbook1(1433,'ICD Energy 14, 33$',300,0.,30000.,0.)
         call hbook1(1434,'ICD Energy 14,34/10,31$',300,0.,30000.,0.)
         call hbook1(1435,'ICD Energy 14,35/11,31$',300,0.,30000.,0.)
        
         call hbook1(1932,'ICD Energy 9, 32$',200,0.,200.,0.)
         call hbook1(1933,'ICD Energy 9, 33$',200,0.,200.,0.)
         call hbook1(1934,'ICD Energy 9, 34$',200,0.,200.,0.)
         call hbook1(1935,'ICD Energy 9,35/13,31$',200,0.,200.,0.)
         call hbook1(2032,'ICD Energy 10, 32$',200,0.,200.,0.)
         call hbook1(2033,'ICD Energy 10, 33$',200,0.,200.,0.)
         call hbook1(2034,'ICD Energy 10, 34$',200,0.,200.,0.)
         call hbook1(2035,'ICD Energy 10,35/14,32$',200,0.,200.,0.)
         call hbook1(2132,'ICD Energy 11, 32$',200,0.,200.,0.)
         call hbook1(2133,'ICD Energy 11, 33$',200,0.,200.,0.)
         call hbook1(2134,'ICD Energy 11, 34$',200,0.,200.,0.)
         call hbook1(2135,'ICD Energy 11,35/14,31$',200,0.,200.,0.)
         call hbook1(2231,'ICD Energy 12, 31$',200,0.,200.,0.)
         call hbook1(2232,'ICD Energy 12, 32$',200,0.,200.,0.)
         call hbook1(2233,'ICD Energy 12, 33$',200,0.,200.,0.)
         call hbook1(2234,'ICD Energy 12, 34$',200,0.,200.,0.)
         call hbook1(2235,'ICD Energy 12, 35$',200,0.,200.,0.)
         call hbook1(2332,'ICD Energy 13, 32$',200,0.,200.,0.)
         call hbook1(2333,'ICD Energy 13, 33$',200,0.,200.,0.)
         call hbook1(2334,'ICD Energy 13, 34$',200,0.,200.,0.)
         call hbook1(2335,'ICD Energy 13,35/9,31$',200,0.,200.,0.)
         call hbook1(2433,'ICD Energy 14, 33$',200,0.,200.,0.)
         call hbook1(2434,'ICD Energy 14,34/10,31$',200,0.,200.,0.)
         call hbook1(2435,'ICD Energy 14,35/11,31$',200,0.,200.,0.)
         
         CALL tb90l2_get_eta_phi('TB90L2_CALOR_HIST_RCP',eta,phi)
         inteta = int(eta*10.+1.)
         intphi = int(phi+1.)
      ENDIF
C
C ****  set flag to true, init counters and get event header
C
      tb90l2_icd_hist = .true.
      firstcall = .true.
      total_energy = 0.
      icdmax = 0.
      hade = 0.
      icde = 0.
      CALL gtcaep_header(nv,nr,nch,ier)
      IF (ier.NE.0)THEN
        WRITE(msg,1002)ier
        CALL errmsg('BAD CAEP HEAD','TB90L2_ICD_HIST',msg,'W')
        RETURN
      ENDIF
C
C ****  loop over channels
C
      DO i=1,nch
        CALL gtcaep(firstcall,ieta,iphi,layer,bits,energy,ichan,ier)
        IF (ier .NE. 0) THEN
          WRITE (msg,1005)ier
          CALL errmsg('GTCAEP','TB90L2_ICD_HIST',msg,'W')
          return
        ENDIF
        if (layer .eq. 9) then
           CALL HCDIR ('\\\\\\\\\\\\\\\\\',' ')
           call hf1 (ieta*100+iphi,energy,1.)
           call hf1 (ieta*100+iphi+1000,energy,1.)
           if (inteta .ge. 9  .and. inteta .le. 14 .and.
     +         intphi .ge. 31 .and. intphi .le. 35) then
              if (ieta .eq. gang_eta(inteta,intphi)
     +         .and. iphi .eq. gang_phi(inteta,intphi)) then
                 call hf1 (104,energy,1.)
                 call hf1 (105,energy,1.)
              end if
           end if
           if (energy .gt. icdmax) then
              icdmax = energy
              call gtcaep_addr (ieta, iphi, 11, mhmax, ier)
           end if
           icde = icde + energy
        end if
        IF(firstcall)firstcall=.false.
        if (layer .ne. 9) total_energy = total_energy + energy
        If (layer .ge. 11 .and. layer .le. 17) then
           hade = hade + energy
        Endif
      ENDDO
      num_of_events = num_of_events + 1
      CALL HCDIR ('\\\\\\\\\\\\\\\\\',' ')
      call hf1 (100, icde, 1.)
      call hf2 (101, hade, icde, 1.)
      call hf1 (102, hade, 1.)
      call hf1 (103, hade+.4*icde, 1.)
      IF(mod(num_of_events,100).eq.0) then
        WRITE(msg,1301)num_of_events,total_energy,.4*icde
        CALL intmsg(msg)
      END IF
C
C ****  Format statements
C
 1002 FORMAT(' IER in GTCAEP_HEADER=',i3)
 1005 FORMAT(' IER in GTCAEP = ',i3)
 1301 FORMAT(' TB90L2_ICD_HIST >>> ',i5,' Events, ',
     &          'Tot Raw E =',f10.2,', ICD E =',f10.2)
      RETURN
      END
