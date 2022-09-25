      subroutine Clean_Photon(LHMTR,Trust)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Analyzes packed hit information
C-                         in the central detector, stored in HMTR/HMTE
C-                         banks and returns .True. if the em-cluster
C-                         passes golden photon criteria.
C-
C-   Inputs  :  LHMTR     -- pointer to either HMTR or HMTE bank
C-   Controls:  None
C-
C-   Created  11-JUL-1994   Greg Landsberg
C-   Updated   2-NOV-1994   Meenakshi Narain  Added entry points
C-                            CLEAN_PHOTON_VAR
C-                            CLEAN_PHOTON_NAMES
C-   Updated   9-FEB-1995   Steven M. Glenn  Added TRD hits quantities
C-   Updated  27-FEB-1995   Meenakshi Narain  Add check on HMTE/P version # 
C-   Updated  27-APR-1995   Gregory L. Landsberg  -- added NCloud info 
C-   Updated  16-MAY-1995   Gregory L. Landsberg  -- fixed NHVTX3D bug 
C-   Updated   2-JUN-1995   Meenakshi Narain  -- Fix array legth for QUANS and
C-                                              QNAMES
C-
C----------------------------------------------------------------------
      implicit      none
      save          L_First, NH_CDC, NH_FDC, NH_VTX, N3D_CDC, NZS_CDC
      save          IH_CDC, IH_FDC, IH_VTX
      include      'D0$INC:ZEBCOM.INC'
      logical       L_First
      logical       L_VTX, L_CD, VTX_FAIL, CD_FAIL
      integer       LHMTR, NH_CDC, NH_FDC, NH_VTX, N3D_CDC, NZS_CDC
      integer       IH_CDC, IH_FDC, IH_VTX, IFLAG, IERR, Trust
      real          RH_CDC, RH_FDC, RH_VTX
      integer       RHVTXW  ,RHCDCW  , FLGCD
      integer       NHVTXXY ,NHVTX3D ,NHCDCXY ,NHCDC3D ,NHCDCZS,NTVAR
      integer       natrd1, natrd2, natrd3, nctrd1, nctrd2, nctrd3
      integer       trd_word, version, NCLOUD
      real          QUANS(15)
      character*8   QNAMES(15)
      data          L_First /.True./
C
      Trust = -1
      if (LHMTR .le. 0) then
         call ErrMsg('CLEANEM','CLEAN_PHOTON',
     &              'Zero pointer to HMTR/HMTE bank','W')
         return
      end if
      if (L_First) then
         L_First = .False.
         call EZPICK('CLEANEM_RCP')
         call EZERR(IErr)
         if (IErr .ne. 0) then
            call ErrMsg('CLEANEM','CLEAN_PHOTON',
     &                'Failed to pick CLEANEM_RCP','F')
            return
         end if
         if (IErr .eq. 0) call EZGET_i('NH_CDC',NH_CDC,IErr)
         if (IErr .eq. 0) call EZGET_i('NH_FDC',NH_FDC,IErr)
         if (IErr .eq. 0) call EZGET_i('NH_VTX',NH_VTX,IErr)
         if (IErr .eq. 0) call EZGET_i('N3D_CDC',N3D_CDC,IErr)
         if (IErr .eq. 0) call EZGET_i('NZS_CDC',NZS_CDC,IErr)
         if (IErr .eq. 0) call EZGET('RH_CDC',RH_CDC,IErr)
         if (IErr .eq. 0) call EZGET('RH_FDC',RH_FDC,IErr)
         if (IErr .eq. 0) call EZGET('RH_VTX',RH_VTX,IErr)
C
         if (IErr .ne. 0) call ErrMsg('CLEANEM','CLEAN_PHOTON',
     &                        'Failed to read CLEANEM_RCP bank','F')
         call EZRSET
         IH_CDC = RH_CDC*100.
         IH_FDC = RH_FDC*100.
         IH_VTX = RH_VTX*100.
      end if
C
      RHVTXW  = 0
      RHCDCW  = 0
      NHVTXXY = 0
      NHVTX3D = 0
      NHCDCXY = 0
      NHCDC3D = 0
      NHCDCZS = 0
      natrd1 = 0
      natrd2 = 0
      natrd3 = 0
      nctrd1 = 0
      nctrd2 = 0
      nctrd3 = 0
C
      IFlag = IQ(LHMTR+17)
      VERSION = IQ(LHMTR+1)
      if ( (IBits(IQ(LHMTR+16),3,1) .eq. 1) .and.
     &     (IBits(IFlag,8,23) .ne. 0) ) Then           ! Use VTX info
         L_VTX    = .True.
         VTX_FAIL = (IBits(IFlag,24,7) .gt. IH_VTX) .or.
     &             (IBits(IFlag,16,8) .gt. NH_VTX)
         RHVTXW   = IBITS(IFlag,24,7)
         NHVTXXY  = IBITS(IFlag,8,8)
         NHVTX3D  = IBITS(IFlag,16,8)
      else
         L_VTX    = .False.
         VTX_FAIL = .False.
      end if
C
      IFlag = IQ(LHMTR+16)
      if ((IBits(IFlag,8,8).ne.255).or.(IBits(IFlag,24,7).ne.100))
     &                                   then     ! Use CD info
         L_CD    = .True.
         FLGCD   = 0
         if (IBits(IFlag,0,1) .eq. 1) Then           ! CC/CDC
            CD_FAIL = (IBits(IFlag,24,7) .gt. IH_CDC)  .or.
     &              (IBits(IFlag, 8,8) .gt. NH_CDC)  .or.
     &              (IBits(IFlag,16,4) .gt. N3D_CDC) .or.
     &              (IBits(IFlag,20,4) .gt. NZS_CDC)
            FLGCD   = 1
            RHCDCW  = IBITS(IFlag,24,7)
            NHCDCXY = IBITS(IFlag, 8,8)
            NHCDC3D = IBITS(IFlag,16,4)
            NHCDCZS = IBITS(IFlag,20,4)
            if (version.ge.4) then
              trd_word = iq(lhmtr+18)
              natrd1 = ibits(trd_word, 3, 3)
              natrd2 = ibits(trd_word, 6, 3)
              natrd3 = ibits(trd_word, 9, 3)
              nctrd1 = ibits(trd_word, 12, 3)
              nctrd2 = ibits(trd_word, 15, 3)
              nctrd3 = ibits(trd_word, 18, 3)
              NCLOUD = IBITS(TRD_WORD,21,10)
            endif
         else                                        ! EC/FDC
            CD_FAIL = (IBits(IFlag,24,7) .gt. IH_FDC)  .or.
     &              (IBits(IFlag, 8,8) .gt. NH_FDC)
            RHCDCW  = IBITS(IFlag,24,7)
            NHCDCXY = IBITS(IFlag, 8,8)
         end if
      else
         L_CD    = .False.
         CD_FAIL = .False.
      end if
C
      if ( L_VTX .or. L_CD )  Trust = 0
      if ( VTX_FAIL ) Trust = Trust + 1
      if (  CD_FAIL ) Trust = Trust + 1
      return
C----------------------------------------------------------------------
      entry CLEAN_PHOTON_VAR(NTVAR,QUANS)
      NTVAR = 15
      QUANS(1) = FLGCD
      QUANS(2) = RHVTXW
      QUANS(3) = RHCDCW
      QUANS(4) = NHVTXXY
      QUANS(5) = NHVTX3D
      QUANS(6) = NHCDCXY
      QUANS(7) = NHCDC3D
      QUANS(8) = NHCDCZS
      quans(9) = natrd1
      quans(10) = natrd2
      quans(11) = natrd3
      quans(12) = nctrd1
      quans(13) = nctrd2
      quans(14) = nctrd3
      QUANS(15) = NCloud
      return
C----------------------------------------------------------------------
      entry CLEAN_PHOTON_NAMES(NTVAR,QNAMES)
      NTVAR = 15
      QNAMES(1) = 'FLGCD'
      QNAMES(2) = 'RHVTXW'
      QNAMES(3) = 'RHCDCW'
      QNAMES(4) = 'NHVTXXY'
      QNAMES(5) = 'NHVTX3D'
      QNAMES(6) = 'NHCDCXY'
      QNAMES(7) = 'NHCDC3D'
      QNAMES(8) = 'NHCDCZS'
      qnames(9) = 'NATRD1'
      qnames(10) = 'NATRD2'
      qnames(11) = 'NATRD3'
      qnames(12) = 'NCTRD1'
      qnames(13) = 'NCTRD2'
      qnames(14) = 'NCTRD3'
      QNAMES(15) = 'NCLOUD'
  999 return
      end
