      SUBROUTINE MTC_FILL_MPVCOM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : /MTC_MPVCOM/ contains most prob value
C-      and width of mip energy dist in adc counts from test beam
C-      data.  Read them in from an RCP file.
C-      MTC_FILL_MPV converts them to GeV.
C-
C-   Inputs  : MTC_MPV_RCP
C-   Outputs : /MTC_CALSECTS/ - cal sublayers and their eta ranges
C-             /MTC_ISSUBCAL/ - number of cal sub-sub-layer
C-             /MTC_MPVCOM/ - Most Probable Value constants
C-                              filled with initial values
C-
C-   Created  11-FEB-1994   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- blocks filled with RCP input data
      INCLUDE 'D0$INC:MTC_CALSECTS.INC'
      INCLUDE 'D0$INC:MTC_ISSUBCAL.INC'
      INCLUDE 'D0$INC:MTC_MPVCOM.INC'
C- local
      INTEGER ier, izz, index
      INTEGER ivect3(3),ivect5(5)
      REAL    vect22(22)
C----------------------------------------------------------------------
C----------------------------------------------------------------------
      CALL INRCP('MTC_MPV_RCP',IER)
      IF(IER.EQ.0) THEN
        CALL EZPICK('MTC_MPV_RCP')

C----------------------------------------------------------------------
C----------------------------------------------------------------------
C- MTC_CALSECTS section
C----------------------------------------------------------------------
C- get the number of cells in eta in each cal module sublayer
        CALL ezget('ISLNUM1',ivect5,ier)
        DO index=1,5
          ISLNUM(1,index) = ivect5(index)        ! 12,12,12,11,0
        END DO

C- check for errors now ...
        IF(ISLNUM(1,1).ne.12) then
          WRITE(6,*) ' MTC_FILL_MPVCOM: error reading RCP ? ',
     &      'subeta ranges in CCEM1 = ',ivect5(index)
        END IF

        CALL ezget('ISLNUM2',ivect5,ier)
        DO index=1,5
          ISLNUM(2,index) = ivect5(index)        !  21,21,22,22,0
        END DO
        CALL ezget('ISLNUM3',ivect5,ier)
        DO index=1,5
          ISLNUM(3,index) = ivect5(index)        !  5,0,0,0,0
        END DO
        CALL ezget('ISLNUM4',ivect5,ier)
        DO index=1,5
          ISLNUM(4,index) = ivect5(index)        !  6,0,0,0,0
        END DO
        CALL ezget('ISLNUM5',ivect5,ier)
        DO index=1,5
          ISLNUM(5,index) = ivect5(index)        !  6,0,0,0,0
        END DO
        CALL ezget('ISLNUM6',ivect5,ier)
        DO index=1,5
          ISLNUM(6,index) = ivect5(index)        !  10,9,8,0,0
        END DO
        CALL ezget('ISLNUM7',ivect5,ier)
        DO index=1,5
          ISLNUM(7,index) = ivect5(index)        !  20,19,19,18,17
        END DO
        CALL ezget('ISLNUM8',ivect5,ier)
        DO index=1,5
          ISLNUM(8,index) = ivect5(index)        ! 6,6,5,6,7
        END DO
        CALL ezget('ISLNUM9',ivect5,ier)
        DO index=1,5
          ISLNUM(9,index) = ivect5(index)        ! 6,0,0,0,0
        END DO
        CALL ezget('ISLNUM10',ivect5,ier)
        DO index=1,5
          ISLNUM(10,index) = ivect5(index)     ! 5,5,4,0,0
        END DO
C- fill the starting ieta for each calorimeter sublayer
        CALL ezget('ISLETA1',ivect5,ier)
        DO index=1,5
          ISLETA(1,index) = ivect5(index)        ! 1,1,1,1,999
        END DO
        CALL ezget('ISLETA2',ivect5,ier)
        DO index=1,5
          ISLETA(2,index) = ivect5(index)        ! 15,15,14,14,999
        END DO
        CALL ezget('ISLETA3',ivect5,ier)
        DO index=1,5
          ISLETA(3,index) = ivect5(index)        ! 8,999,999,999,999
        END DO
        CALL ezget('ISLETA4',ivect5,ier)
        DO index=1,5
          ISLETA(4,index) = ivect5(index)        ! 9,999,999,999,999
        END DO
        CALL ezget('ISLETA5',ivect5,ier)
        DO index=1,5
          ISLETA(5,index) = ivect5(index)        ! 8,999,999,999,999
        END DO
        CALL ezget('ISLETA6',ivect5,ier)
        DO index=1,5
          ISLETA(6,index) = ivect5(index)        ! 1,1,1,999,999
        END DO
        CALL ezget('ISLETA7',ivect5,ier)
        DO index=1,5
          ISLETA(7,index) = ivect5(index)        ! 17,18,19,20,21
        END DO
        CALL ezget('ISLETA8',ivect5,ier)
        DO index=1,5
          ISLETA(8,index) = ivect5(index)        ! 11,12,13,13,14
        END DO
        CALL ezget('ISLETA9',ivect5,ier)
        DO index=1,5
          ISLETA(9,index) = ivect5(index)        ! 1,999,999,999,999
        END DO
        CALL ezget('ISLETA10',ivect5,ier)
        DO index=1,5
          ISLETA(10,index) = ivect5(index)       ! 8,9,11,999,999
        END DO
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C- MTC_ISSUBCAL section
C----------------------------------------------------------------------
! COMMON /MTC_ISSUBCAL/ ISSLNUM,ISSLNUM_7,ISSLNUM_8,ISSLNUM_10
!  ISSLNUM are cal sub-sub-layer layers numbers by module type, sublayer #.
!  Then in some (ical,isub,ieta), the number of sub-sub-layers is not the norm
!  The # of sub-sub-layers in these special cal cells are entered in the
!  other arrays
!  by 1st column=sublayer #, 2nd column = ieta, 3rd col = # of sub-sub-layers
        CALL ezget('ISSLNUM1',ivect5,ier)
        DO index=1,5
          ISSLNUM(1,index) = ivect5(index)       ! 1,1,1,1,0
        END DO
        CALL ezget('ISSLNUM2',ivect5,ier)
        DO index=1,5
          ISSLNUM(2,index) = ivect5(index)       ! 1,1,3,2,0
        END DO
        CALL ezget('ISSLNUM3',ivect5,ier)
        DO index=1,5
          ISSLNUM(3,index) = ivect5(index)       ! 1,0,0,0,0
        END DO
        CALL ezget('ISSLNUM4',ivect5,ier)
        DO index=1,5
          ISSLNUM(4,index) = ivect5(index)       ! 1,0,0,0,0
        END DO
        CALL ezget('ISSLNUM5',ivect5,ier)
        DO index=1,5
          ISSLNUM(5,index) = ivect5(index)       ! 1,0,0,0,0
        END DO
        CALL ezget('ISSLNUM6',ivect5,ier)
        DO index=1,5
          ISSLNUM(6,index) = ivect5(index)     !   1,1,1,0,0
        END DO
        CALL ezget('ISSLNUM7',ivect5,ier)
        DO index=1,5
          ISSLNUM(7,index) = ivect5(index)       ! 2,2,2,2,3
        END DO
        CALL ezget('ISSLNUM8',ivect5,ier)
        DO index=1,5
          ISSLNUM(8,index) = ivect5(index)       ! 2,2,2,2,3
        END DO
        CALL ezget('ISSLNUM9',ivect5,ier)
        DO index=1,5
          ISSLNUM(9,index) = ivect5(index)       ! 1,0,0,0,0
        END DO
        CALL ezget('ISSLNUM10',ivect5,ier)
        DO index=1,5
          ISSLNUM(10,index) = ivect5(index)      ! 4,4,4,0,0
        END DO
! In some (ical,isub,ieta), the number of sub-sub-layers is not the norm
! The # of sub-sub-layers in these special cal cells are entered in the
! following arrays:
!   1st column=sublayer #, 2nd column = ieta, 3rd col = # of sub-sub-layers
! in ECIH (ical=7)
        CALL ezget('ISSLNUM_71',ivect3,ier)
        DO index=1,3
          ISSLNUM_7(1,index) = ivect3(index)       ! 1,17,1
        END DO
        CALL ezget('ISSLNUM_72',ivect3,ier)
        DO index=1,3
          ISSLNUM_7(2,index) = ivect3(index)       ! 2,18,1
        END DO
        CALL ezget('ISSLNUM_73',ivect3,ier)
        DO index=1,3
          ISSLNUM_7(3,index) = ivect3(index)       ! 3,19,1
        END DO
        CALL ezget('ISSLNUM_74',ivect3,ier)
        DO index=1,3
          ISSLNUM_7(4,index) = ivect3(index)       ! 5,21,1
        END DO
! in the ECMH (ical=8)
        CALL ezget('ISSLNUM_81',ivect3,ier)
        DO index=1,3
          ISSLNUM_8(1,index) = ivect3(index)       ! 1,11,1
        END DO
        CALL ezget('ISSLNUM_82',ivect3,ier)
        DO index=1,3
          ISSLNUM_8(2,index) = ivect3(index)       ! 4,13,1
        END DO
        CALL ezget('ISSLNUM_83',ivect3,ier)
        DO index=1,3
          ISSLNUM_8(3,index) = ivect3(index)       ! 5,14,1
        END DO
        CALL ezget('ISSLNUM_84',ivect3,ier)
        DO index=1,3
          ISSLNUM_8(4,index) = ivect3(index)       ! 5,15,2
        END DO
        CALL ezget('ISSLNUM_85',ivect3,ier)
        DO index=1,3
          ISSLNUM_8(5,index) = ivect3(index)       ! 5,20,2
        END DO
! in the ECOH (ical=10)
        CALL ezget('ISSLNUM_101',ivect3,ier)
        DO index=1,3
          ISSLNUM_10(1,index) = ivect3(index)      ! 1, 8,3
        END DO
        CALL ezget('ISSLNUM_102',ivect3,ier)
        DO index=1,3
          ISSLNUM_10(2,index) = ivect3(index)      ! 1,12,2
        END DO
        CALL ezget('ISSLNUM_103',ivect3,ier)
        DO index=1,3
          ISSLNUM_10(3,index) = ivect3(index)      ! 2, 9,1
        END DO
        CALL ezget('ISSLNUM_104',ivect3,ier)
        DO index=1,3
          ISSLNUM_10(4,index) = ivect3(index)      ! 3,11,3
        END DO
        CALL ezget('ISSLNUM_105',ivect3,ier)
        DO index=1,3
          ISSLNUM_10(5,index) = ivect3(index)      ! 3,15,2
        END DO
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C- MPVCOM section
C----------------------------------------------------------------------
C- Mip response varies from tile to tile for ICD ...
ccc        CALL EZGET('GEV_MPV_ICD',GEV_MPV_ICD,IER)         ! 768*-999.
        DO index=1,768
          GEV_MPV_ICD(index) = -999.
        END DO
ccc        CALL EZGET('GEV_MPV_ICD_W',GEV_MPV_ICD_W,IER)     ! 768*-999.
        DO index=1,768
          GEV_MPV_ICD_W(index) = -999.
        END DO
C......................................................................
C- 1x1 MIP MPV in adc counts ...
C......................................................................
C- CCEM ...............................................................
        izz = 1
        CALL EZGET('ADC_CCEM1',VECT22,IER)        ! 12*6.4   10*-999.
        DO index=1,22
          ADCMPV(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_CCEM2',VECT22,IER)        ! 12*6.4   10*-999.
        DO index=1,22
          ADCMPV(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_CCEM3',VECT22,IER)        ! 12*26.9  10*-999.
        DO index=1,22
          ADCMPV(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_CCEM4',VECT22,IER)        ! 11*41.5  11*-999.
        DO index=1,22
          ADCMPV(izz,4,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_CCEM5',VECT22,IER)        ! 22*-999.
        DO index=1,22
          ADCMPV(izz,5,index) = -999.
        END DO
C- ECEM ...............................................................
        izz = 2
        CALL EZGET('ADC_ECEM1',VECT22,IER)        ! 21*6.5   1*-999.
        DO index=1,22
          ADCMPV(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECEM2',VECT22,IER)        ! 21*7.5   1*-999.
        DO index=1,22
          ADCMPV(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECEM3',VECT22,IER)        ! 22*24.7
        DO index=1,22
          ADCMPV(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECEM4',VECT22,IER)        ! 22*32.3
        DO index=1,22
          ADCMPV(izz,4,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_ECEM5',VECT22,IER)        ! 22*-999.
        DO index=1,22
          ADCMPV(izz,5,index) = -999.
        END DO
C- CCMG ...............................................................
        izz = 3
        CALL EZGET('ADC_CCMG1',VECT22,IER)        ! 9.8  9.8  9.6  7.8 7.4  17*-999.
        DO index=1,22
          ADCMPV(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_CCMG2',VECT22,IER)        ! 22*-999.
        DO index=1,22
          ADCMPV(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCMG3',VECT22,IER)        ! 22*-999.
        DO index=1,22
          ADCMPV(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCMG4',VECT22,IER)        ! 22*-999.
        DO index=1,22
          ADCMPV(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCMG5',VECT22,IER)        ! 22*-999.
        DO index=1,22
          ADCMPV(izz,5,index) = -999.
        END DO
C- ICD ...............................................................
        izz = 4
        CALL EZGET('ADC_ICD1',VECT22,IER) ! 6*.03891  16*-999.
        DO index=1,22
          ADCMPV(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_ICD2',VECT22,IER) ! 22*-999.
        DO index=1,22
          ADCMPV(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ICD3',VECT22,IER) ! 22*-999.
        DO index=1,22
          ADCMPV(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ICD4',VECT22,IER) ! 22*-999.
        DO index=1,22
          ADCMPV(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ICD5',VECT22,IER) ! 22*-999.
        DO index=1,22
          ADCMPV(izz,5,index) = -999.
        END DO
C- ECMG ...............................................................
        izz = 5
        CALL EZGET('ADC_ECMG1',VECT22,IER)        ! 7.2  7.2  7.4  9.2  8.2  9.4  16*-999.
        DO index=1,22
          ADCMPV(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_ECMG2',VECT22,IER)        ! 22*-999.
        DO index=1,22
          ADCMPV(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ECMG3',VECT22,IER)        ! 22*-999.
        DO index=1,22
          ADCMPV(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ECMG4',VECT22,IER)        ! 22*-999.
        DO index=1,22
          ADCMPV(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ECMG5',VECT22,IER)        ! 22*-999.
        DO index=1,22
          ADCMPV(izz,5,index) = -999.
        END DO
C- CCFH ...............................................................
        izz = 6
        CALL EZGET('ADC_CCFH1',VECT22,IER)        ! 10*89.3  12*-999.
        DO index=1,22
          ADCMPV(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_CCFH2',VECT22,IER)        ! 9*66.5  13*-999.
        DO index=1,22
          ADCMPV(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_CCFH3',VECT22,IER)        ! 8*53.4  14*-999.
        DO index=1,22
          ADCMPV(izz,3,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_CCFH4',VECT22,IER)        ! 22*-999.
        DO index=1,22
          ADCMPV(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCFH5',VECT22,IER)        ! 22*-999.
        DO index=1,22
          ADCMPV(izz,5,index) = -999.
        END DO
C- ECIH ...............................................................
        izz = 7
        CALL EZGET('ADC_ECIH1',VECT22,IER)        ! 30.85  19*61.7  2*-999.
        DO index=1,22
          ADCMPV(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECIH2',VECT22,IER)        ! 30.85  18*61.7  3*-999.
        DO index=1,22
          ADCMPV(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECIH3',VECT22,IER)        ! 30.85  18*61.7  3*-999.
        DO index=1,22
          ADCMPV(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECIH4',VECT22,IER)        ! 30.85  17*61.7  4*-999.
        DO index=1,22
          ADCMPV(izz,4,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECIH5',VECT22,IER)        ! 16.33  32.66  15*49.0  5*-999.
        DO index=1,22
          ADCMPV(izz,5,index) = VECT22(index)
        END DO
C- ECMH ...............................................................
        izz = 8
        CALL EZGET('ADC_ECMH1',VECT22,IER)        ! 0.0  85.  78.  3*75.  16*-999.
        DO index=1,22
          ADCMPV(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECMH2',VECT22,IER)        ! 42.  78.  3*76.  38.  16*-999.
        DO index=1,22
          ADCMPV(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECMH3',VECT22,IER)        ! 74.  4*78.  17*-999.
        DO index=1,22
          ADCMPV(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECMH4',VECT22,IER)        ! 0.0  5*78.  16*-999.
        DO index=1,22
          ADCMPV(izz,4,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECMH5',VECT22,IER)        ! 10.3  20.6  4*30.9  20.6  15*-999.
        DO index=1,22
          ADCMPV(izz,5,index) = VECT22(index)
        END DO
C- CCCH ...............................................................
        izz = 9
        CALL EZGET('ADC_CCCH1',VECT22,IER)        ! 6*41.0  16*-999.
        DO index=1,22
          ADCMPV(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_CCCH2',VECT22,IER)        ! 22*-999.
        DO index=1,22
          ADCMPV(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCCH3',VECT22,IER)        ! 22*-999.
        DO index=1,22
          ADCMPV(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCCH4',VECT22,IER)        ! 22*-999.
        DO index=1,22
          ADCMPV(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCCH5',VECT22,IER)        ! 22*-999.
        DO index=1,22
          ADCMPV(izz,5,index) = -999.
        END DO
C- ECOH ...............................................................
        izz = 10
        CALL EZGET('ADC_ECOH1',VECT22,IER)        ! 18.5  37.  35.  32.  10.  17*-999.
        DO index=1,22
          ADCMPV(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECOH2',VECT22,IER)        ! 0.0  24.  35.  37.  27.  17*-999.
        DO index=1,22
          ADCMPV(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECOH3',VECT22,IER)        ! 14.  35.  35.  34.       18*-999.
        DO index=1,22
          ADCMPV(izz,3,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_ECOH4',VECT22,IER)        ! 22*-999.
        DO index=1,22
          ADCMPV(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ECOH5',VECT22,IER)        ! 22*-999.
        DO index=1,22
          ADCMPV(izz,5,index) = -999.
        END DO
C......................................................................
C- 1x1 MIP MPV in GeV ...
C......................................................................
C- CCEM ...............................................................
        izz = 1
        CALL EZGET('GEV_CCEM1',VECT22,IER)        ! 12*-9.  10*-999.
        DO index=1,22
          GEVMPV(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_CCEM2',VECT22,IER)        ! 12*-9.  10*-999.
        DO index=1,22
          GEVMPV(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_CCEM3',VECT22,IER)        ! 12*-9.  10*-999.
        DO index=1,22
          GEVMPV(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_CCEM4',VECT22,IER)        ! 11*-9.  11*-999.
        DO index=1,22
          GEVMPV(izz,4,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_CCEM5',VECT22,IER)        ! 22*-999.
        DO index=1,22
          GEVMPV(izz,5,index) = -999.
        END DO
C- ECEM ...............................................................
        izz = 2
        CALL EZGET('GEV_ECEM1',VECT22,IER)        ! 21*-9.   1*-999.
        DO index=1,22
          GEVMPV(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECEM2',VECT22,IER)        ! 21*-9.   1*-999.
        DO index=1,22
          GEVMPV(izz,2,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_ECEM3',VECT22,IER)        ! 22*-9.
        DO index=1,22
          GEVMPV(izz,3,index) = -9.
        END DO
ccc        CALL EZGET('GEV_ECEM4',VECT22,IER)        ! 22*-9.
        DO index=1,22
          GEVMPV(izz,4,index) = -9.
        END DO
ccc        CALL EZGET('GEV_ECEM5',VECT22,IER)        ! 22*-999.
        DO index=1,22
          GEVMPV(izz,5,index) = -999.
        END DO
C- CCMG ...............................................................
        izz = 3
        CALL EZGET('GEV_CCMG1',VECT22,IER)        ! 5*-9.  17*-999.
        DO index=1,22
          GEVMPV(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_CCMG2',VECT22,IER)        ! 22*-999.
        DO index=1,22
          GEVMPV(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCMG3',VECT22,IER)        ! 22*-999.
        DO index=1,22
          GEVMPV(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCMG4',VECT22,IER)        ! 22*-999.
        DO index=1,22
          GEVMPV(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCMG5',VECT22,IER)        ! 22*-999.
        DO index=1,22
          GEVMPV(izz,5,index) = -999.
        END DO
C- ICD ...............................................................
        izz = 4
        CALL EZGET('GEV_ICD1',VECT22,IER) ! 6*-9.  16*-999.
        DO index=1,22
          GEVMPV(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_ICD2',VECT22,IER) ! 22*-999.
        DO index=1,22
          GEVMPV(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ICD3',VECT22,IER) ! 22*-999.
        DO index=1,22
          GEVMPV(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ICD4',VECT22,IER) ! 22*-999.
        DO index=1,22
          GEVMPV(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ICD5',VECT22,IER) ! 22*-999.
        DO index=1,22
          GEVMPV(izz,5,index) = -999.
        END DO
C- ECMG ...............................................................
        izz = 5
        CALL EZGET('GEV_ECMG1',VECT22,IER)        ! 6*-9.  16*-999.
        DO index=1,22
          GEVMPV(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_ECMG2',VECT22,IER)        ! 22*-999.
        DO index=1,22
          GEVMPV(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ECMG3',VECT22,IER)        ! 22*-999.
        DO index=1,22
          GEVMPV(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ECMG4',VECT22,IER)        ! 22*-999.
        DO index=1,22
          GEVMPV(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ECMG5',VECT22,IER)        ! 22*-999.
        DO index=1,22
          GEVMPV(izz,5,index) = -999.
        END DO
C- CCFH ...............................................................
        izz = 6
        CALL EZGET('GEV_CCFH1',VECT22,IER)        ! 10*-9.  12*-999.
        DO index=1,22
          GEVMPV(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_CCFH2',VECT22,IER)        ! 9*-9.  13*-999.
        DO index=1,22
          GEVMPV(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_CCFH3',VECT22,IER)        ! 8*-9.  14*-999.
        DO index=1,22
          GEVMPV(izz,3,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_CCFH4',VECT22,IER)        ! 22*-999.
        DO index=1,22
          GEVMPV(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCFH5',VECT22,IER)        ! 22*-999.
        DO index=1,22
          GEVMPV(izz,5,index) = -999.
        END DO
C- ECIH ...............................................................
        izz = 7
        CALL EZGET('GEV_ECIH1',VECT22,IER)        ! 20*-9.   2*-999.
        DO index=1,22
          GEVMPV(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECIH2',VECT22,IER)        ! 19*-9.   3*-999.
        DO index=1,22
          GEVMPV(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECIH3',VECT22,IER)        ! 19*-9.   3*-999.
        DO index=1,22
          GEVMPV(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECIH4',VECT22,IER)        ! 18*-9.   4*-999.
        DO index=1,22
          GEVMPV(izz,4,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECIH5',VECT22,IER)        ! 17*-9.   5*-999.
        DO index=1,22
          GEVMPV(izz,5,index) = VECT22(index)
        END DO
C- ECMH ...............................................................
        izz = 8
        CALL EZGET('GEV_ECMH1',VECT22,IER)        ! 6*-9.  16*-999.
        DO index=1,22
          GEVMPV(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECMH2',VECT22,IER)        ! 6*-9.  16*-999.
        DO index=1,22
          GEVMPV(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECMH3',VECT22,IER)        ! 5*-9.  17*-999.
        DO index=1,22
          GEVMPV(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECMH4',VECT22,IER)        ! 6*-9.  16*-999.
        DO index=1,22
          GEVMPV(izz,4,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECMH5',VECT22,IER)        ! 7*-9.  15*-999.
        DO index=1,22
          GEVMPV(izz,5,index) = VECT22(index)
        END DO
C- CCCH ...............................................................
        izz = 9
        CALL EZGET('GEV_CCCH1',VECT22,IER)        ! 6*-9.  16*-999.
        DO index=1,22
          GEVMPV(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_CCCH2',VECT22,IER)        ! 22*-999.
        DO index=1,22
          GEVMPV(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCCH3',VECT22,IER)        ! 22*-999.
        DO index=1,22
          GEVMPV(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCCH4',VECT22,IER)        ! 22*-999.
        DO index=1,22
          GEVMPV(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCCH5',VECT22,IER)        ! 22*-999.
        DO index=1,22
          GEVMPV(izz,5,index) = -999.
        END DO
C- ECOH ...............................................................
        izz = 10
        CALL EZGET('GEV_ECOH1',VECT22,IER)        ! 5*-9.  17*-999.
        DO index=1,22
          GEVMPV(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECOH2',VECT22,IER)        ! 5*-9.  17*-999.
        DO index=1,22
          GEVMPV(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECOH3',VECT22,IER)        ! 4*-9.  18*-999.
        DO index=1,22
          GEVMPV(izz,3,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_ECOH4',VECT22,IER)        ! 22*-999.
        DO index=1,22
          GEVMPV(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ECOH5',VECT22,IER)        ! 22*-999.
        DO index=1,22
          GEVMPV(izz,5,index) = -999.
        END DO
C......................................................................
C- 1x1 MIP MPV RMS in adc counts ...
C......................................................................
C- CCEM ...............................................................
        izz = 1
        CALL EZGET('ADC_CCEM_W1',VECT22,IER)      ! 12*8.0   10*-999.
        DO index=1,22
          ADCMPV_W(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_CCEM_W2',VECT22,IER)      ! 12*8.0   10*-999.
        DO index=1,22
          ADCMPV_W(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_CCEM_W3',VECT22,IER)      ! 12*20.0  10*-999.
        DO index=1,22
          ADCMPV_W(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_CCEM_W4',VECT22,IER)      ! 11*30.0  11*-999.
        DO index=1,22
          ADCMPV_W(izz,4,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_CCEM_W5',VECT22,IER)      ! 22*-999.
        DO index=1,22
          ADCMPV_W(izz,5,index) = -999.
        END DO
C- ECEM ...............................................................
        izz = 2
        CALL EZGET('ADC_ECEM_W1',VECT22,IER)      ! 21*5.    1*-999.
        DO index=1,22
          ADCMPV_W(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECEM_W2',VECT22,IER)      ! 21*5.    1*-999.
        DO index=1,22
          ADCMPV_W(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECEM_W3',VECT22,IER)      ! 22*17.
        DO index=1,22
          ADCMPV_W(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECEM_W4',VECT22,IER)      ! 22*17.
        DO index=1,22
          ADCMPV_W(izz,4,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_ECEM_W5',VECT22,IER)      ! 22*-999.
        DO index=1,22
          ADCMPV_W(izz,5,index) = -999.
        END DO
C- CCMG ...............................................................
        izz = 3
        CALL EZGET('ADC_CCMG_W1',VECT22,IER)      ! 23.  23.  13.  12.  10.  17*-999.
        DO index=1,22
          ADCMPV_W(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_CCMG_W2',VECT22,IER)      ! 22*-999.
        DO index=1,22
          ADCMPV_W(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCMG_W3',VECT22,IER)      ! 22*-999.
        DO index=1,22
          ADCMPV_W(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCMG_W4',VECT22,IER)      ! 22*-999.
        DO index=1,22
          ADCMPV_W(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCMG_W5',VECT22,IER)      ! 22*-999.
        DO index=1,22
          ADCMPV_W(izz,5,index) = -999.
        END DO
C- ICD ...............................................................
        izz = 4
        CALL EZGET('ADC_ICD_W1',VECT22,IER)       ! 6*-9.  16*-999.
        DO index=1,22
          ADCMPV_W(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_ICD_W2',VECT22,IER)       ! 22*-999.
        DO index=1,22
          ADCMPV_W(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ICD_W3',VECT22,IER)       ! 22*-999.
        DO index=1,22
          ADCMPV_W(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ICD_W4',VECT22,IER)       ! 22*-999.
        DO index=1,22
          ADCMPV_W(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ICD_W5',VECT22,IER)       ! 22*-999.
        DO index=1,22
          ADCMPV_W(izz,5,index) = -999.
        END DO
C- ECMG ...............................................................
        izz = 5
        CALL EZGET('ADC_ECMG_W1',VECT22,IER)      ! 18.  18.  12.  12.  13.  14.  16*-999.
        DO index=1,22
          ADCMPV_W(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_ECMG_W2',VECT22,IER)      ! 22*-999.
        DO index=1,22
          ADCMPV_W(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ECMG_W3',VECT22,IER)      ! 22*-999.
        DO index=1,22
          ADCMPV_W(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ECMG_W4',VECT22,IER)      ! 22*-999.
        DO index=1,22
          ADCMPV_W(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ECMG_W5',VECT22,IER)      ! 22*-999.
        DO index=1,22
          ADCMPV_W(izz,5,index) = -999.
        END DO
C- CCFH ...............................................................
        izz = 6
        CALL EZGET('ADC_CCFH_W1',VECT22,IER)      ! 10*45.0  12*-999.
        DO index=1,22
          ADCMPV_W(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_CCFH_W2',VECT22,IER)      ! 9*45.0  13*-999.
        DO index=1,22
          ADCMPV_W(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_CCFH_W3',VECT22,IER)      ! 8*40.0  14*-999.
        DO index=1,22
          ADCMPV_W(izz,3,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_CCFH_W4',VECT22,IER)      ! 22*-999.
        DO index=1,22
          ADCMPV_W(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCFH_W5',VECT22,IER)      ! 22*-999.
        DO index=1,22
          ADCMPV_W(izz,5,index) = -999.
        END DO
C- ECIH ...............................................................
        izz = 7
        CALL EZGET('ADC_ECIH_W1',VECT22,IER)      ! 30.  19*30.  2*-999.
        DO index=1,22
          ADCMPV_W(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECIH_W2',VECT22,IER)      ! 30.  18*30.  3*-999.
        DO index=1,22
          ADCMPV_W(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECIH_W3',VECT22,IER)      ! 30.  18*30.  3*-999.
        DO index=1,22
          ADCMPV_W(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECIH_W4',VECT22,IER)      ! 30.  17*30.  4*-999.
        DO index=1,22
          ADCMPV_W(izz,4,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECIH_W5',VECT22,IER)      ! 22.  22.  15*22.  5*-999.
        DO index=1,22
          ADCMPV_W(izz,5,index) = VECT22(index)
        END DO
C- ECMH ...............................................................
        izz = 8
        CALL EZGET('ADC_ECMH_W1',VECT22,IER)      ! 22.  32.  32.  3*31.  16*-999.
        DO index=1,22
          ADCMPV_W(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECMH_W2',VECT22,IER)      ! 34.  32.  3*30.  30.  16*-999.
        DO index=1,22
          ADCMPV_W(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECMH_W3',VECT22,IER)      ! 32.  4*31.  17*-999.
        DO index=1,22
          ADCMPV_W(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECMH_W4',VECT22,IER)      ! 23.  5*31.  16*-999.
        DO index=1,22
          ADCMPV_W(izz,4,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECMH_W5',VECT22,IER)      ! 14.  14.  4*14.  14.  15*-999.
        DO index=1,22
          ADCMPV_W(izz,5,index) = VECT22(index)
        END DO
C- CCCH ...............................................................
        izz = 9
        CALL EZGET('ADC_CCCH_W1',VECT22,IER)      ! 6*19.0  16*-999.
        DO index=1,22
          ADCMPV_W(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_CCCH_W2',VECT22,IER)      ! 22*-999.
        DO index=1,22
          ADCMPV_W(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCCH_W3',VECT22,IER)      ! 22*-999.
        DO index=1,22
          ADCMPV_W(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCCH_W4',VECT22,IER)      ! 22*-999.
        DO index=1,22
          ADCMPV_W(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCCH_W5',VECT22,IER)      ! 22*-999.
        DO index=1,22
          ADCMPV_W(izz,5,index) = -999.
        END DO
C- ECOH ...............................................................
        izz = 10
        CALL EZGET('ADC_ECOH_W1',VECT22,IER)      ! 27.  27.  21.  21.  14.  17*-999.
        DO index=1,22
          ADCMPV_W(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECOH_W2',VECT22,IER)      ! 11.  19.  21.  21.  19.  17*-999.
        DO index=1,22
          ADCMPV_W(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECOH_W3',VECT22,IER)      ! 17.  22.  21.  20.       18*-999.
        DO index=1,22
          ADCMPV_W(izz,3,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_ECOH_W4',VECT22,IER)      ! 22*-999.
        DO index=1,22
          ADCMPV_W(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ECOH_W5',VECT22,IER)      ! 22*-999.
        DO index=1,22
          ADCMPV_W(izz,5,index) = -999.
        END DO
C......................................................................
C- 1x1 MIP RMS in GeV
C......................................................................
C- CCEM ...............................................................
        izz = 1
        CALL EZGET('GEV_CCEM_W1',VECT22,IER)      ! 12*-9.  10*-999.
        DO index=1,22
          GEVMPV_W(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_CCEM_W2',VECT22,IER)      ! 12*-9.  10*-999.
        DO index=1,22
          GEVMPV_W(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_CCEM_W3',VECT22,IER)      ! 12*-9.  10*-999.
        DO index=1,22
          GEVMPV_W(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_CCEM_W4',VECT22,IER)      ! 11*-9.  11*-999.
        DO index=1,22
          GEVMPV_W(izz,4,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_CCEM_W5',VECT22,IER)      ! 22*-999.
        DO index=1,22
          GEVMPV_W(izz,5,index) = -999.
        END DO
C- ECEM ...............................................................
        izz = 2
        CALL EZGET('GEV_ECEM_W1',VECT22,IER)      ! 21*-9.   1*-999.
        DO index=1,22
          GEVMPV_W(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECEM_W2',VECT22,IER)      ! 21*-9.   1*-999.
        DO index=1,22
          GEVMPV_W(izz,2,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_ECEM_W3',VECT22,IER)      ! 22*-9.
        DO index=1,22
          GEVMPV_W(izz,3,index) = -9.
        END DO
ccc        CALL EZGET('GEV_ECEM_W4',VECT22,IER)      ! 22*-9.
        DO index=1,22
          GEVMPV_W(izz,4,index) = -9.
        END DO
ccc        CALL EZGET('GEV_ECEM_W5',VECT22,IER)      ! 22*-999.
        DO index=1,22
          GEVMPV_W(izz,5,index) = -999.
        END DO
C- CCMG ...............................................................
        izz = 3
        CALL EZGET('GEV_CCMG_W1',VECT22,IER)      ! 5*-9.  17*-999.
        DO index=1,22
          GEVMPV_W(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_CCMG_W2',VECT22,IER)      ! 22*-999.
        DO index=1,22
          GEVMPV_W(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCMG_W3',VECT22,IER)      ! 22*-999.
        DO index=1,22
          GEVMPV_W(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCMG_W4',VECT22,IER)      ! 22*-999.
        DO index=1,22
          GEVMPV_W(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCMG_W5',VECT22,IER)      ! 22*-999.
        DO index=1,22
          GEVMPV_W(izz,5,index) = -999.
        END DO
C- ICD ...............................................................
        izz = 4
        CALL EZGET('GEV_ICD_W1',VECT22,IER)       ! 6*-9.  16*-999.
        DO index=1,22
          GEVMPV_W(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_ICD_W2',VECT22,IER)       ! 22*-999.
        DO index=1,22
          GEVMPV_W(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ICD_W3',VECT22,IER)       ! 22*-999.
        DO index=1,22
          GEVMPV_W(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ICD_W4',VECT22,IER)       ! 22*-999.
        DO index=1,22
          GEVMPV_W(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ICD_W5',VECT22,IER)       ! 22*-999.
        DO index=1,22
          GEVMPV_W(izz,5,index) = -999.
        END DO
C- ECMG ...............................................................
        izz = 5
        CALL EZGET('GEV_ECMG_W1',VECT22,IER)      ! 6*-9.  16*-999.
        DO index=1,22
          GEVMPV_W(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_ECMG_W2',VECT22,IER)      ! 22*-999.
        DO index=1,22
          GEVMPV_W(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ECMG_W3',VECT22,IER)      ! 22*-999.
        DO index=1,22
          GEVMPV_W(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ECMG_W4',VECT22,IER)      ! 22*-999.
        DO index=1,22
          GEVMPV_W(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ECMG_W5',VECT22,IER)      ! 22*-999.
        DO index=1,22
          GEVMPV_W(izz,5,index) = -999.
        END DO
C- CCFH ...............................................................
        izz = 6
        CALL EZGET('GEV_CCFH_W1',VECT22,IER)      ! 10*-9.  12*-999.
        DO index=1,22
          GEVMPV_W(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_CCFH_W2',VECT22,IER)      ! 9*-9.  13*-999.
        DO index=1,22
          GEVMPV_W(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_CCFH_W3',VECT22,IER)      ! 8*-9.  14*-999.
        DO index=1,22
          GEVMPV_W(izz,3,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_CCFH_W4',VECT22,IER)      ! 22*-999.
        DO index=1,22
          GEVMPV_W(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCFH_W5',VECT22,IER)      ! 22*-999.
        DO index=1,22
          GEVMPV_W(izz,5,index) = -999.
        END DO
C- ECIH ...............................................................
        izz = 7
        CALL EZGET('GEV_ECIH_W1',VECT22,IER)      ! 20*-9.   2*-999.
        DO index=1,22
          GEVMPV_W(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECIH_W2',VECT22,IER)      ! 19*-9.   3*-999.
        DO index=1,22
          GEVMPV_W(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECIH_W3',VECT22,IER)      ! 19*-9.   3*-999.
        DO index=1,22
          GEVMPV_W(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECIH_W4',VECT22,IER)      ! 18*-9.   4*-999.
        DO index=1,22
          GEVMPV_W(izz,4,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECIH_W5',VECT22,IER)      ! 17*-9.   5*-999.
        DO index=1,22
          GEVMPV_W(izz,5,index) = VECT22(index)
        END DO
C- ECMH ...............................................................
        izz = 8
        CALL EZGET('GEV_ECMH_W1',VECT22,IER)      ! 6*-9.  16*-999.
        DO index=1,22
          GEVMPV_W(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECMH_W2',VECT22,IER)      ! 6*-9.  16*-999.
        DO index=1,22
          GEVMPV_W(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECMH_W3',VECT22,IER)      ! 5*-9.  17*-999.
        DO index=1,22
          GEVMPV_W(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECMH_W4',VECT22,IER)      ! 6*-9.  16*-999.
        DO index=1,22
          GEVMPV_W(izz,4,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECMH_W5',VECT22,IER)      ! 7*-9.  15*-999.
        DO index=1,22
          GEVMPV_W(izz,5,index) = VECT22(index)
        END DO
C- CCCH ...............................................................
        izz = 9
        CALL EZGET('GEV_CCCH_W1',VECT22,IER)      ! 6*-9.  16*-999.
        DO index=1,22
          GEVMPV_W(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_CCCH_W2',VECT22,IER)      ! 22*-999.
        DO index=1,22
          GEVMPV_W(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCCH_W3',VECT22,IER)      ! 22*-999.
        DO index=1,22
          GEVMPV_W(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCCH_W4',VECT22,IER)      ! 22*-999.
        DO index=1,22
          GEVMPV_W(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCCH_W5',VECT22,IER)      ! 22*-999.
        DO index=1,22
          GEVMPV_W(izz,5,index) = -999.
        END DO
C- ECOH ...............................................................
        izz = 10
        CALL EZGET('GEV_ECOH_W1',VECT22,IER)      ! 5*-9.  17*-999.
        DO index=1,22
          GEVMPV_W(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECOH_W2',VECT22,IER)      ! 5*-9.  17*-999.
        DO index=1,22
          GEVMPV_W(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECOH_W3',VECT22,IER)      ! 5*-9.  17*-999.
        DO index=1,22
          GEVMPV_W(izz,3,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_ECOH_W4',VECT22,IER)      ! 22*-999.
        DO index=1,22
          GEVMPV_W(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ECOH_W5',VECT22,IER)      ! 22*-999.
        DO index=1,22
          GEVMPV_W(izz,5,index) = -999.
        END DO
C......................................................................
C......................................................................
C- MIP response most prob value and RMS in the 3x3 cells about a test beam muon
C- in each (cal section, cal sect sublayer, cal sect sublyr eta)
C- ADC*(10,5,22) -- in ADC counts
C- GeV*(10,5,22) -- in GeV
!---------------------------------------------------------------------
C- Mip response varies from tile to tile for ICD ...
ccc        CALL EZGET('GEV_MPV_ICD3',GEV_MPV_ICD3,IER)       ! 768*-999.
        DO index=1,768
          GEV_MPV_ICD3(index) = -999.
        END DO
ccc        CALL EZGET('GEV_MPV_ICD_W3',GEV_MPV_ICD_W3,IER)   ! 768*-999.
        DO index=1,768
          GEV_MPV_ICD_W3(index) = -999.
        END DO
C......................................................................
C- 3x3 MIP MPV in adc counts ...
C......................................................................
C- CCEM ...............................................................
        izz = 1
        CALL EZGET('ADC_CCEM31',VECT22,IER)       ! 12*6.4   10*-999.
        DO index=1,22
          ADCMPV3(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_CCEM32',VECT22,IER)       ! 12*6.4   10*-999.
        DO index=1,22
          ADCMPV3(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_CCEM33',VECT22,IER)       ! 12*26.9  10*-999.
        DO index=1,22
          ADCMPV3(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_CCEM34',VECT22,IER)       ! 11*41.5  11*-999.
        DO index=1,22
          ADCMPV3(izz,4,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_CCEM35',VECT22,IER)       ! 22*-999.
        DO index=1,22
          ADCMPV3(izz,5,index) = -999.
        END DO
C- ECEM ...............................................................
        izz = 2
        CALL EZGET('ADC_ECEM31',VECT22,IER)       ! 21*6.5   1*-999.
        DO index=1,22
          ADCMPV3(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECEM32',VECT22,IER)       ! 21*7.5   1*-999.
        DO index=1,22
          ADCMPV3(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECEM33',VECT22,IER)       ! 22*24.7
        DO index=1,22
          ADCMPV3(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECEM34',VECT22,IER)       ! 22*32.3
        DO index=1,22
          ADCMPV3(izz,4,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_ECEM35',VECT22,IER)       ! 22*-999.
        DO index=1,22
          ADCMPV3(izz,5,index) = -999.
        END DO
C- CCMG ...............................................................
        izz = 3
        CALL EZGET('ADC_CCMG31',VECT22,IER)       ! 9.8  9.8  9.9  9.9  7.8  17*-999.
        DO index=1,22
          ADCMPV3(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_CCMG32',VECT22,IER)       ! 22*-999.
        DO index=1,22
          ADCMPV3(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCMG33',VECT22,IER)       ! 22*-999.
        DO index=1,22
          ADCMPV3(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCMG34',VECT22,IER)       ! 22*-999.
        DO index=1,22
          ADCMPV3(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCMG35',VECT22,IER)       ! 22*-999.
        DO index=1,22
          ADCMPV3(izz,5,index) = -999.
        END DO
C- ICD ...............................................................
        izz = 4
        CALL EZGET('ADC_ICD31',VECT22,IER)        ! 6*.03891  16*-999.
        DO index=1,22
          ADCMPV3(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_ICD32',VECT22,IER)        ! 22*-999.
        DO index=1,22
          ADCMPV3(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ICD33',VECT22,IER)        ! 22*-999.
        DO index=1,22
          ADCMPV3(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ICD34',VECT22,IER)        ! 22*-999.
        DO index=1,22
          ADCMPV3(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ICD35',VECT22,IER)        ! 22*-999.
        DO index=1,22
          ADCMPV3(izz,5,index) = -999.
        END DO
C- ECMG ...............................................................
        izz = 5
        CALL EZGET('ADC_ECMG31',VECT22,IER)       ! 10.  10.  8.2  8.4  9.7  9.3  16*-999.
        DO index=1,22
          ADCMPV3(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_ECMG32',VECT22,IER)       ! 22*-999.
        DO index=1,22
          ADCMPV3(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ECMG33',VECT22,IER)       ! 22*-999.
        DO index=1,22
          ADCMPV3(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ECMG34',VECT22,IER)       ! 22*-999.
        DO index=1,22
          ADCMPV3(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ECMG35',VECT22,IER)       ! 22*-999.
        DO index=1,22
          ADCMPV3(izz,5,index) = -999.
        END DO
C- CCFH ...............................................................
        izz = 6
        CALL EZGET('ADC_CCFH31',VECT22,IER)       ! 10*89.3  12*-999.
        DO index=1,22
          ADCMPV3(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_CCFH32',VECT22,IER)       ! 9*66.5  13*-999.
        DO index=1,22
          ADCMPV3(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_CCFH33',VECT22,IER)       ! 8*53.4  14*-999.
        DO index=1,22
          ADCMPV3(izz,3,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_CCFH34',VECT22,IER)       ! 22*-999.
        DO index=1,22
          ADCMPV3(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCFH35',VECT22,IER)       ! 22*-999.
        DO index=1,22
          ADCMPV3(izz,5,index) = -999.
        END DO
C- ECIH ...............................................................
        izz = 7
        CALL EZGET('ADC_ECIH31',VECT22,IER)       ! 30.85  19*61.7  2*-999.
        DO index=1,22
          ADCMPV3(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECIH32',VECT22,IER)       ! 30.85  18*61.7  3*-999.
        DO index=1,22
          ADCMPV3(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECIH33',VECT22,IER)       ! 30.85  18*61.7  3*-999.
        DO index=1,22
          ADCMPV3(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECIH34',VECT22,IER)       ! 30.85  17*61.7  4*-999.
        DO index=1,22
          ADCMPV3(izz,4,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECIH35',VECT22,IER)       ! 16.33  32.66  15*49.0  5*-999.
        DO index=1,22
          ADCMPV3(izz,5,index) = VECT22(index)
        END DO
C- ECMH ...............................................................
        izz = 8
        CALL EZGET('ADC_ECMH31',VECT22,IER)       ! 0.0  92.  83.  3*82.  16*-999.
        DO index=1,22
          ADCMPV3(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECMH32',VECT22,IER)       ! 47.  84.  3*85.  43.  16*-999.
        DO index=1,22
          ADCMPV3(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECMH33',VECT22,IER)       ! 76.  4*82.  17*-999.
        DO index=1,22
          ADCMPV3(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECMH34',VECT22,IER)       ! 0.0  5*78.  16*-999.
        DO index=1,22
          ADCMPV3(izz,4,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECMH35',VECT22,IER)       ! 12.  24.  4*37.  24.  15*-999.
        DO index=1,22
          ADCMPV3(izz,5,index) = VECT22(index)
        END DO
C- CCCH ...............................................................
        izz = 9
        CALL EZGET('ADC_CCCH31',VECT22,IER)       ! 6*41.0  16*-999.
        DO index=1,22
          ADCMPV3(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_CCCH32',VECT22,IER)       ! 22*-999.
        DO index=1,22
          ADCMPV3(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCCH33',VECT22,IER)       ! 22*-999.
        DO index=1,22
          ADCMPV3(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCCH34',VECT22,IER)       ! 22*-999.
        DO index=1,22
          ADCMPV3(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCCH35',VECT22,IER)       ! 22*-999.
        DO index=1,22
          ADCMPV3(izz,5,index) = -999.
        END DO
C- ECOH ...............................................................
        izz = 10
        CALL EZGET('ADC_ECOH31',VECT22,IER)       ! 18.  35.  35.  33.  13.  17*-999.
        DO index=1,22
          ADCMPV3(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECOH32',VECT22,IER)       ! 5.2  23.  36.  39.  26.  17*-999.
        DO index=1,22
          ADCMPV3(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECOH33',VECT22,IER)       ! 15.  37.  36.  35.       18*-999.
        DO index=1,22
          ADCMPV3(izz,3,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_ECOH34',VECT22,IER)       ! 22*-999.
        DO index=1,22
          ADCMPV3(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ECOH35',VECT22,IER)       ! 22*-999.
        DO index=1,22
          ADCMPV3(izz,5,index) = -999.
        END DO
C......................................................................
C- 3x3 MIP MPV in GeV ...
C......................................................................
C- CCEM ...............................................................
        izz = 1
        CALL EZGET('GEV_CCEM31',VECT22,IER)       ! 12*-9.  10*-999.
        DO index=1,22
          GEVMPV3(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_CCEM32',VECT22,IER)       ! 12*-9.  10*-999.
        DO index=1,22
          GEVMPV3(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_CCEM33',VECT22,IER)       ! 12*-9.  10*-999.
        DO index=1,22
          GEVMPV3(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_CCEM34',VECT22,IER)       ! 11*-9.  11*-999.
        DO index=1,22
          GEVMPV3(izz,4,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_CCEM35',VECT22,IER)       ! 22*-999.
        DO index=1,22
          GEVMPV3(izz,5,index) = -999.
        END DO
C- ECEM ...............................................................
        izz = 2
        CALL EZGET('GEV_ECEM31',VECT22,IER)       ! 21*-9.   1*-999.
        DO index=1,22
          GEVMPV3(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECEM32',VECT22,IER)       ! 21*-9.   1*-999.
        DO index=1,22
          GEVMPV3(izz,2,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_ECEM33',VECT22,IER)       ! 22*-9.
        DO index=1,22
          GEVMPV3(izz,3,index) = -9.
        END DO
ccc        CALL EZGET('GEV_ECEM34',VECT22,IER)       ! 22*-9.
        DO index=1,22
          GEVMPV3(izz,4,index) = -9.
        END DO
ccc        CALL EZGET('GEV_ECEM35',VECT22,IER)       ! 22*-999.
        DO index=1,22
          GEVMPV3(izz,5,index) = -999.
        END DO
C- CCMG ...............................................................
        izz = 3
        CALL EZGET('GEV_CCMG31',VECT22,IER)       ! 5*-9.  17*-999.
        DO index=1,22
          GEVMPV3(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_CCMG32',VECT22,IER)       ! 22*-999.
        DO index=1,22
          GEVMPV3(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCMG33',VECT22,IER)       ! 22*-999.
        DO index=1,22
          GEVMPV3(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCMG34',VECT22,IER)       ! 22*-999.
        DO index=1,22
          GEVMPV3(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCMG35',VECT22,IER)       ! 22*-999.
        DO index=1,22
          GEVMPV3(izz,5,index) = -999.
        END DO
C- ICD ...............................................................
        izz = 4
        CALL EZGET('GEV_ICD31',VECT22,IER)        ! 6*-9.  16*-999.
        DO index=1,22
          GEVMPV3(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_ICD32',VECT22,IER)        ! 22*-999.
        DO index=1,22
          GEVMPV3(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ICD33',VECT22,IER)        ! 22*-999.
        DO index=1,22
          GEVMPV3(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ICD34',VECT22,IER)        ! 22*-999.
        DO index=1,22
          GEVMPV3(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ICD35',VECT22,IER)        ! 22*-999.
        DO index=1,22
          GEVMPV3(izz,5,index) = -999.
        END DO
C- ECMG ...............................................................
        izz = 5
        CALL EZGET('GEV_ECMG31',VECT22,IER)       ! 6*-9.  16*-999.
        DO index=1,22
          GEVMPV3(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_ECMG32',VECT22,IER)       ! 22*-999.
        DO index=1,22
          GEVMPV3(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ECMG33',VECT22,IER)       ! 22*-999.
        DO index=1,22
          GEVMPV3(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ECMG34',VECT22,IER)       ! 22*-999.
        DO index=1,22
          GEVMPV3(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ECMG35',VECT22,IER)       ! 22*-999.
        DO index=1,22
          GEVMPV3(izz,5,index) = -999.
        END DO
C- CCFH ...............................................................
        izz = 6
        CALL EZGET('GEV_CCFH31',VECT22,IER)       ! 10*-9.  12*-999.
        DO index=1,22
          GEVMPV3(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_CCFH32',VECT22,IER)       ! 9*-9.  13*-999.
        DO index=1,22
          GEVMPV3(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_CCFH33',VECT22,IER)       ! 8*-9.  14*-999.
        DO index=1,22
          GEVMPV3(izz,3,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_CCFH34',VECT22,IER)       ! 22*-999.
        DO index=1,22
          GEVMPV3(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCFH35',VECT22,IER)       ! 22*-999.
        DO index=1,22
          GEVMPV3(izz,5,index) = -999.
        END DO
C- ECIH ...............................................................
        izz = 7
        CALL EZGET('GEV_ECIH31',VECT22,IER)       ! 20*-9.   2*-999.
        DO index=1,22
          GEVMPV3(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECIH32',VECT22,IER)       ! 19*-9.   3*-999.
        DO index=1,22
          GEVMPV3(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECIH33',VECT22,IER)       ! 19*-9.   3*-999.
        DO index=1,22
          GEVMPV3(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECIH34',VECT22,IER)       ! 18*-9.   4*-999.
        DO index=1,22
          GEVMPV3(izz,4,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECIH35',VECT22,IER)       ! 17*-9.   5*-999.
        DO index=1,22
          GEVMPV3(izz,5,index) = VECT22(index)
        END DO
C- ECMH ...............................................................
        izz = 8
        CALL EZGET('GEV_ECMH31',VECT22,IER)       ! 6*-9.  16*-999.
        DO index=1,22
          GEVMPV3(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECMH32',VECT22,IER)       ! 6*-9.  16*-999.
        DO index=1,22
          GEVMPV3(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECMH33',VECT22,IER)       ! 5*-9.  17*-999.
        DO index=1,22
          GEVMPV3(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECMH34',VECT22,IER)       ! 6*-9.  16*-999.
        DO index=1,22
          GEVMPV3(izz,4,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECMH35',VECT22,IER)       ! 7*-9.  15*-999.
        DO index=1,22
          GEVMPV3(izz,5,index) = VECT22(index)
        END DO
C- CCCH ...............................................................
        izz = 9
        CALL EZGET('GEV_CCCH31',VECT22,IER)       ! 6*-9.  16*-999.
        DO index=1,22
          GEVMPV3(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_CCCH32',VECT22,IER)       ! 22*-999.
        DO index=1,22
          GEVMPV3(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCCH33',VECT22,IER)       ! 22*-999.
        DO index=1,22
          GEVMPV3(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCCH34',VECT22,IER)       ! 22*-999.
        DO index=1,22
          GEVMPV3(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCCH35',VECT22,IER)       ! 22*-999.
        DO index=1,22
          GEVMPV3(izz,5,index) = -999.
        END DO
C- ECOH ...............................................................
        izz = 10
        CALL EZGET('GEV_ECOH31',VECT22,IER)       ! 5*-9.  17*-999.
        DO index=1,22
          GEVMPV3(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECOH32',VECT22,IER)       ! 5*-9.  17*-999.
        DO index=1,22
          GEVMPV3(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECOH33',VECT22,IER)       ! 4*-9.  18*-999.
        DO index=1,22
          GEVMPV3(izz,3,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_ECOH34',VECT22,IER)       ! 22*-999.
        DO index=1,22
          GEVMPV3(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ECOH35',VECT22,IER)       ! 22*-999.
        DO index=1,22
          GEVMPV3(izz,5,index) = -999.
        END DO
C......................................................................
C- 3x3 MIP MPV RMS in adc counts ...
C......................................................................
C- CCEM ...............................................................
        izz = 1
        CALL EZGET('ADC_CCEM_W31',VECT22,IER)     ! 12*8.0   10*-999.
        DO index=1,22
          ADCMPV_W3(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_CCEM_W32',VECT22,IER)     ! 12*8.0   10*-999.
        DO index=1,22
          ADCMPV_W3(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_CCEM_W33',VECT22,IER)     ! 12*20.0  10*-999.
        DO index=1,22
          ADCMPV_W3(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_CCEM_W34',VECT22,IER)     ! 11*30.0  11*-999.
        DO index=1,22
          ADCMPV_W3(izz,4,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_CCEM_W35',VECT22,IER)     ! 22*-999.
        DO index=1,22
          ADCMPV_W3(izz,5,index) = -999.
        END DO
C- ECEM ...............................................................
        izz = 2
        CALL EZGET('ADC_ECEM_W31',VECT22,IER)     ! 21*5.    1*-999.
        DO index=1,22
          ADCMPV_W3(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECEM_W32',VECT22,IER)     ! 21*5.    1*-999.
        DO index=1,22
          ADCMPV_W3(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECEM_W33',VECT22,IER)     ! 22*17.
        DO index=1,22
          ADCMPV_W3(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECEM_W34',VECT22,IER)     ! 22*17.
        DO index=1,22
          ADCMPV_W3(izz,4,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_ECEM_W35',VECT22,IER)     ! 22*-999.
        DO index=1,22
          ADCMPV_W3(izz,5,index) = -999.
        END DO
C- CCMG ...............................................................
        izz = 3
        CALL EZGET('ADC_CCMG_W31',VECT22,IER)     ! 31.  31.  16.  14.  13.  17*-999.
        DO index=1,22
          ADCMPV_W3(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_CCMG_W32',VECT22,IER)     ! 22*-999.
        DO index=1,22
          ADCMPV_W3(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCMG_W33',VECT22,IER)     ! 22*-999.
        DO index=1,22
          ADCMPV_W3(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCMG_W34',VECT22,IER)     ! 22*-999.
        DO index=1,22
          ADCMPV_W3(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCMG_W35',VECT22,IER)     ! 22*-999.
        DO index=1,22
          ADCMPV_W3(izz,5,index) = -999.
        END DO
C- ICD ...............................................................
        izz = 4
        CALL EZGET('ADC_ICD_W31',VECT22,IER)      ! 6*-9.  16*-999.
        DO index=1,22
          ADCMPV_W3(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_ICD_W32',VECT22,IER)      ! 22*-999.
        DO index=1,22
          ADCMPV_W3(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ICD_W33',VECT22,IER)      ! 22*-999.
        DO index=1,22
          ADCMPV_W3(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ICD_W34',VECT22,IER)      ! 22*-999.
        DO index=1,22
          ADCMPV_W3(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ICD_W35',VECT22,IER)      ! 22*-999.
        DO index=1,22
          ADCMPV_W3(izz,5,index) = -999.
        END DO
C- ECMG ...............................................................
        izz = 5
        CALL EZGET('ADC_ECMG_W31',VECT22,IER)     ! 25.  25.  14.  15.  15.  15.  16*-999.
        DO index=1,22
          ADCMPV_W3(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_ECMG_W32',VECT22,IER)     ! 22*-999.
        DO index=1,22
          ADCMPV_W3(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ECMG_W33',VECT22,IER)     ! 22*-999.
        DO index=1,22
          ADCMPV_W3(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ECMG_W34',VECT22,IER)     ! 22*-999.
        DO index=1,22
          ADCMPV_W3(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ECMG_W35',VECT22,IER)     ! 22*-999.
        DO index=1,22
          ADCMPV_W3(izz,5,index) = -999.
        END DO
C- CCFH ...............................................................
        izz = 6
        CALL EZGET('ADC_CCFH_W31',VECT22,IER)     ! 10*45.0  12*-999.
        DO index=1,22
          ADCMPV_W3(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_CCFH_W32',VECT22,IER)     ! 9*45.0  13*-999.
        DO index=1,22
          ADCMPV_W3(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_CCFH_W33',VECT22,IER)     ! 8*40.0  14*-999.
        DO index=1,22
          ADCMPV_W3(izz,3,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_CCFH_W34',VECT22,IER)     ! 22*-999.
        DO index=1,22
          ADCMPV_W3(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCFH_W35',VECT22,IER)     ! 22*-999.
        DO index=1,22
          ADCMPV_W3(izz,5,index) = -999.
        END DO
C- ECIH ...............................................................
        izz = 7
        CALL EZGET('ADC_ECIH_W31',VECT22,IER)     ! 30.  19*30.  2*-999.
        DO index=1,22
          ADCMPV_W3(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECIH_W32',VECT22,IER)     ! 30.  18*30.  3*-999.
        DO index=1,22
          ADCMPV_W3(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECIH_W33',VECT22,IER)     ! 30.  18*30.  3*-999.
        DO index=1,22
          ADCMPV_W3(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECIH_W34',VECT22,IER)     ! 30.  17*30.  4*-999.
        DO index=1,22
          ADCMPV_W3(izz,4,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECIH_W35',VECT22,IER)     ! 22.  22.  15*22.  5*-999.
        DO index=1,22
          ADCMPV_W3(izz,5,index) = VECT22(index)
        END DO
C- ECMH ...............................................................
        izz = 8
        CALL EZGET('ADC_ECMH_W31',VECT22,IER)     ! 31.  42.  45.  3*43.  16*-999.
        DO index=1,22
          ADCMPV_W3(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECMH_W32',VECT22,IER)     ! 41.  43.  3*43.  43.  16*-999.
        DO index=1,22
          ADCMPV_W3(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECMH_W33',VECT22,IER)     ! 43.  4*44.  17*-999.
        DO index=1,22
          ADCMPV_W3(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECMH_W34',VECT22,IER)     ! 33.  43.  4*43.  16*-999.
        DO index=1,22
          ADCMPV_W3(izz,4,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECMH_W35',VECT22,IER)     ! 17.  17.  4*17.  17.  15*-999.
        DO index=1,22
          ADCMPV_W3(izz,5,index) = VECT22(index)
        END DO
C- CCCH ...............................................................
        izz = 9
        CALL EZGET('ADC_CCCH_W31',VECT22,IER)     ! 6*19.0  16*-999.
        DO index=1,22
          ADCMPV_W3(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_CCCH_W32',VECT22,IER)     ! 22*-999.
        DO index=1,22
          ADCMPV_W3(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCCH_W33',VECT22,IER)     ! 22*-999.
        DO index=1,22
          ADCMPV_W3(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCCH_W34',VECT22,IER)     ! 22*-999.
        DO index=1,22
          ADCMPV_W3(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('ADC_CCCH_W35',VECT22,IER)     ! 22*-999.
        DO index=1,22
          ADCMPV_W3(izz,5,index) = -999.
        END DO
C- ECOH ...............................................................
        izz = 10
        CALL EZGET('ADC_ECOH_W31',VECT22,IER)     ! 29.  29.  23.  23.  17.  17*-999.
        DO index=1,22
          ADCMPV_W3(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECOH_W32',VECT22,IER)     ! 16.  21.  24.  24.  21.  17*-999.
        DO index=1,22
          ADCMPV_W3(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('ADC_ECOH_W33',VECT22,IER)     ! 19.  24.  24.  22.       18*-999.
        DO index=1,22
          ADCMPV_W3(izz,3,index) = VECT22(index)
        END DO
ccc        CALL EZGET('ADC_ECOH_W34',VECT22,IER)     ! 22*-999.
        DO index=1,22
          ADCMPV_W3(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('ADC_ECOH_W35',VECT22,IER)     ! 22*-999.
        DO index=1,22
          ADCMPV_W3(izz,5,index) = -999.
        END DO
C......................................................................
C- 3x3 MIP RMS in GeV ...
C......................................................................
C- CCEM ...............................................................
        izz = 1
        CALL EZGET('GEV_CCEM_W31',VECT22,IER)     ! 12*-9.  10*-999.
        DO index=1,22
          GEVMPV_W3(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_CCEM_W32',VECT22,IER)     ! 12*-9.  10*-999.
        DO index=1,22
          GEVMPV_W3(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_CCEM_W33',VECT22,IER)     ! 12*-9.  10*-999.
        DO index=1,22
          GEVMPV_W3(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_CCEM_W34',VECT22,IER)     ! 11*-9.  11*-999.
        DO index=1,22
          GEVMPV_W3(izz,4,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_CCEM_W35',VECT22,IER)     ! 22*-999.
        DO index=1,22
          GEVMPV_W3(izz,5,index) = -999.
        END DO
C- ECEM ...............................................................
        izz = 2
        CALL EZGET('GEV_ECEM_W31',VECT22,IER)     ! 21*-9.   1*-999.
        DO index=1,22
          GEVMPV_W3(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECEM_W32',VECT22,IER)     ! 21*-9.   1*-999.
        DO index=1,22
          GEVMPV_W3(izz,2,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_ECEM_W33',VECT22,IER)     ! 22*-9.
        DO index=1,22
          GEVMPV_W3(izz,3,index) = -9.
        END DO
ccc        CALL EZGET('GEV_ECEM_W34',VECT22,IER)     ! 22*-9.
        DO index=1,22
          GEVMPV_W3(izz,4,index) = -9.
        END DO
ccc        CALL EZGET('GEV_ECEM_W35',VECT22,IER)     ! 22*-999.
        DO index=1,22
          GEVMPV_W3(izz,5,index) = -999.
        END DO
C- CCMG ...............................................................
        izz = 3
        CALL EZGET('GEV_CCMG_W31',VECT22,IER)     ! 5*-9.  17*-999.
        DO index=1,22
          GEVMPV_W3(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_CCMG_W32',VECT22,IER)     ! 22*-999.
        DO index=1,22
          GEVMPV_W3(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCMG_W33',VECT22,IER)     ! 22*-999.
        DO index=1,22
          GEVMPV_W3(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCMG_W34',VECT22,IER)     ! 22*-999.
        DO index=1,22
          GEVMPV_W3(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCMG_W35',VECT22,IER)     ! 22*-999.
        DO index=1,22
          GEVMPV_W3(izz,5,index) = -999.
        END DO
C- ICD ...............................................................
        izz = 4
        CALL EZGET('GEV_ICD_W31',VECT22,IER)      ! 6*-9.  16*-999.
        DO index=1,22
          GEVMPV_W3(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_ICD_W32',VECT22,IER)      ! 22*-999.
        DO index=1,22
          GEVMPV_W3(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ICD_W33',VECT22,IER)      ! 22*-999.
        DO index=1,22
          GEVMPV_W3(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ICD_W34',VECT22,IER)      ! 22*-999.
        DO index=1,22
          GEVMPV_W3(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ICD_W35',VECT22,IER)      ! 22*-999.
        DO index=1,22
          GEVMPV_W3(izz,5,index) = -999.
        END DO
C- ECMG ...............................................................
        izz = 5
        CALL EZGET('GEV_ECMG_W31',VECT22,IER)     ! 6*-9.  16*-999.
        DO index=1,22
          GEVMPV_W3(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_ECMG_W32',VECT22,IER)     ! 22*-999.
        DO index=1,22
          GEVMPV_W3(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ECMG_W33',VECT22,IER)     ! 22*-999.
        DO index=1,22
          GEVMPV_W3(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ECMG_W34',VECT22,IER)     ! 22*-999.
        DO index=1,22
          GEVMPV_W3(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ECMG_W35',VECT22,IER)     ! 22*-999.
        DO index=1,22
          GEVMPV_W3(izz,5,index) = -999.
        END DO
C- CCFH ...............................................................
        izz = 6
        CALL EZGET('GEV_CCFH_W31',VECT22,IER)     ! 10*-9.  12*-999.
        DO index=1,22
          GEVMPV_W3(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_CCFH_W32',VECT22,IER)     ! 9*-9.  13*-999.
        DO index=1,22
          GEVMPV_W3(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_CCFH_W33',VECT22,IER)     ! 8*-9.  14*-999.
        DO index=1,22
          GEVMPV_W3(izz,3,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_CCFH_W34',VECT22,IER)     ! 22*-999.
        DO index=1,22
          GEVMPV_W3(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCFH_W35',VECT22,IER)     ! 22*-999.
        DO index=1,22
          GEVMPV_W3(izz,5,index) = -999.
        END DO
C- ECIH ...............................................................
        izz = 7
        CALL EZGET('GEV_ECIH_W31',VECT22,IER)     ! 20*-9.   2*-999.
        DO index=1,22
          GEVMPV_W3(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECIH_W32',VECT22,IER)     ! 19*-9.   3*-999.
        DO index=1,22
          GEVMPV_W3(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECIH_W33',VECT22,IER)     ! 19*-9.   3*-999.
        DO index=1,22
          GEVMPV_W3(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECIH_W34',VECT22,IER)     ! 18*-9.   4*-999.
        DO index=1,22
          GEVMPV_W3(izz,4,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECIH_W35',VECT22,IER)     ! 17*-9.   5*-999.
        DO index=1,22
          GEVMPV_W3(izz,5,index) = VECT22(index)
        END DO
C- ECMH ...............................................................
        izz = 8
        CALL EZGET('GEV_ECMH_W31',VECT22,IER)     ! 6*-9.  16*-999.
        DO index=1,22
          GEVMPV_W3(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECMH_W32',VECT22,IER)     ! 6*-9.  16*-999.
        DO index=1,22
          GEVMPV_W3(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECMH_W33',VECT22,IER)     ! 5*-9.  17*-999.
        DO index=1,22
          GEVMPV_W3(izz,3,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECMH_W34',VECT22,IER)     ! 6*-9.  16*-999.
        DO index=1,22
          GEVMPV_W3(izz,4,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECMH_W35',VECT22,IER)     ! 7*-9.  15*-999.
        DO index=1,22
          GEVMPV_W3(izz,5,index) = VECT22(index)
        END DO
C- CCCH ...............................................................
        izz = 9
        CALL EZGET('GEV_CCCH_W31',VECT22,IER)     ! 6*-9.  16*-999.
        DO index=1,22
          GEVMPV_W3(izz,1,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_CCCH_W32',VECT22,IER)     ! 22*-999.
        DO index=1,22
          GEVMPV_W3(izz,2,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCCH_W33',VECT22,IER)     ! 22*-999.
        DO index=1,22
          GEVMPV_W3(izz,3,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCCH_W34',VECT22,IER)     ! 22*-999.
        DO index=1,22
          GEVMPV_W3(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('GEV_CCCH_W35',VECT22,IER)     ! 22*-999.
        DO index=1,22
          GEVMPV_W3(izz,5,index) = -999.
        END DO
C- ECOH ...............................................................
        izz = 10
        CALL EZGET('GEV_ECOH_W31',VECT22,IER)     ! 5*-9.  17*-999.
        DO index=1,22
          GEVMPV_W3(izz,1,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECOH_W32',VECT22,IER)     ! 5*-9.  17*-999.
        DO index=1,22
          GEVMPV_W3(izz,2,index) = VECT22(index)
        END DO
        CALL EZGET('GEV_ECOH_W33',VECT22,IER)     ! 5*-9.  17*-999.
        DO index=1,22
          GEVMPV_W3(izz,3,index) = VECT22(index)
        END DO
ccc        CALL EZGET('GEV_ECOH_W34',VECT22,IER)     ! 22*-999.
        DO index=1,22
          GEVMPV_W3(izz,4,index) = -999.
        END DO
ccc        CALL EZGET('GEV_ECOH_W35',VECT22,IER)     ! 22*-999.
        DO index=1,22
          GEVMPV_W3(izz,5,index) = -999.
        END DO
C----------------------------------------------------------------------
        CALL EZRSET
      ELSE
        WRITE(6,*) ' MTC_FILL_MPVCOM:  MTC_MPV_RCP error IER = ',IER
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
