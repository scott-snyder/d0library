      SUBROUTINE PICAEH(type,ADDR,NEWLIN,PRUNIT,
     &  emin,emax,pmin,pmax,laymin,laymax,etmin,etmax)
C-----------------------------------------------------------------------
C-
C-  grabs CAEH (calorimeter energy hits) 
C-
C-  INPUT:  TYPE      0 for direct CAEH, 1 for use pointer as provided
C-                    by the variable ADDR below.
C-          ADDR      pointer to CACH or JPTS bank
C-          NEWLINE   passed from c program, it's equiv of "   \n"
C-  PRUNIT=           character array
C-
C-   Created  Dec. 21,1988  Serban Protopopescu
C-   Updated  20-APR-1992   Chip Stewart   - no abort
C-   Updated  20-MAY-1993   Stan Krzywdzinski
C-      Add printout of error matrix
C-   Updated  24-JUN-1993   Stan Krzywdzinski
C-      Fixed format mismatch
C-   Stolen   1995  Baden
C-----------------------------------------------------------------------

      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:DCPARA.DEF/LIST'
      INTEGER LCAEH,GZCAEH
      INTEGER I,LDATA,NR
      INTEGER NCH,VRSION,i1,i2
      INTEGER NEWLIN,PRUNIT(*)
      CHARACTER*100 CTEMP
      integer eta,phi,lay,ip,addr,type
      real et
c
      integer ichar
      character*4 cchar
      equivalence (ichar,cchar)
c
      integer emin,emax,pmin,pmax,laymin,laymax,ib,j,iodd,jbyt,nw
      real etmin,etmax
      integer MCHAN
      common /xxcaeh/mchan
C--------------------------------------------------------------------
C
C--   check if bank does exist
C
      LCAEH=GZCAEH()
      IF(LCAEH.LE.0) THEN
        call FWARNING(%REF('CZCAEH fails to find pointer to CAEH'))
        return
      endif
C
      VRSION=IQ(LCAEH+1)
      NR=IQ(LCAEH+2)
      NCH=IQ(LCAEH+3)
      WRITE(CTEMP,100) vrsion,newlin
  100 format('The version number of the current bank (CAEH) is ',i4,
     &  '   ',A4)
      I1 = 60    !60 is number of characters in CTEMP (must be div by 4)
      I2 = 1
      CALL UCTOH(CTEMP,PRUNIT(I2),4,I1)
c
      WRITE(CTEMP,'(''Total Number of CAEH channels:     '',I5,A4)') 
     &  NCH,NEWLIN
      i2 = i2 + i1/4
      I1 = 44    !44 is number of characters is CTEMP (must be div by 4)
      CALL UCTOH(CTEMP,PRUNIT(I2),4,I1)
c
      WRITE(CTEMP,
     &  '(''Number of channels requested and/or passing:   '',I5,A4)') 
     &  MCHAN,NEWLIN
      i2 = i2 + i1/4
      I1 = 56    !56 is number of characters in CTEMP (must be div by 4)
      CALL UCTOH(CTEMP,PRUNIT(I2),4,I1)
c
      WRITE(CTEMP,
     &  '('' Hit Eta Phi Lyr'',9X,''E'',8X,''Et'',6X,''STATUS    '')')
      i2 = i2 + i1/4
      I1 = 52    !52 is number of characters in CTEMP
      CALL UCTOH(CTEMP,PRUNIT(I2),4,I1)
c
      WRITE(CTEMP,
     &  '('' Hit Eta Phi Lyr'',9X,''E'',8X,''Et'',6X,''STATUS'',A4)') 
     &  NEWLIN
      i2 = i2 + i1/4
      I1 = 52    !52 is number of characters in CTEMP
      CALL UCTOH(CTEMP,PRUNIT(I2),4,I1)
c
c     use pointer to cach/jpts/cjpt
c
      if (type.eq.1) then
        ichar = iq(addr-4)
        if (cchar.eq.'CACH'.or.cchar.eq.'JPTS') then
c
c         cach/jpts have same format
c
          nch = iq(addr+2)
          ip = 0
          do i=1,nch
            ib = iq(addr+2+i)
            ldata = lcaeh+nr*(ib-1)
            eta = iq(ldata+12)
            phi = iq(ldata+13)
            lay = iq(ldata+14)
            et = q(ldata+8)
            if (  (eta.ge.emin.and.eta.le.emax) .and.
     &        (phi.ge.pmin.and.phi.le.pmax) .and.
     &        (lay.ge.laymin.and.lay.le.laymax) .and.
     &        (et.ge.etmin.and.et.le.etmax) ) then
              ip = ip + 1
              i2 = i2 + i1/4
              I1 = 52
              if (mod(ip,2).eq.1) then
                WRITE(CTEMP,'(4I4,2F10.3,4X,Z8.8,''    '')')
     &          ib,eta,phi,lay,q(ldata+7),et,IQ(LDATA+15)
                CALL UCTOH(CTEMP,PRUNIT(I2),4,I1)
              else
                WRITE(CTEMP,'(4I4,2F10.3,4X,Z8.8,A4)')
     &          ib,eta,phi,lay,q(ldata+7),et,IQ(LDATA+15),NEWLIN
                CALL UCTOH(CTEMP,PRUNIT(I2),4,I1)
              endif
            endif
          enddo
          i2 = i2 + i1/4
          prunit(i2) = 0
        else if (cchar.eq.'CJPT') then
c
c         cjpt has packed format (2 pointers per word)
c
          iodd = 1
          if (jbyt(iq(addr+iq(addr-1)),1,16).gt.0) iodd = 0
          nw = iq(addr-1) - 2
          nch = nw*2 - iodd
          do i=0,nw-1
            do j=1,2
              ib = jbyt(iq(addr+3+i),((2-j)*16)+1,16)
              ldata = lcaeh + nr*(ib-1)
              eta = iq(ldata+12)
              phi = iq(ldata+13)
              lay = iq(ldata+14)
              et = q(ldata+8)
              if (  (eta.ge.emin.and.eta.le.emax) .and.
     &            (phi.ge.pmin.and.phi.le.pmax) .and.
     &            (lay.ge.laymin.and.lay.le.laymax) .and.
     &            (et.ge.etmin.and.et.le.etmax) ) then
                ip = ip + 1
                i2 = i2 + i1/4
                I1 = 52
                if (mod(ip,2).eq.1) then
                  WRITE(CTEMP,'(4I4,2F10.3,4X,Z8.8,''    '')')
     &              ib,eta,phi,lay,q(ldata+7),et,IQ(LDATA+15)
                  CALL UCTOH(CTEMP,PRUNIT(I2),4,I1)
                else
                  WRITE(CTEMP,'(4I4,2F10.3,4X,Z8.8,A4)')
     &            ib,eta,phi,lay,q(ldata+7),et,IQ(LDATA+15),NEWLIN
                  CALL UCTOH(CTEMP,PRUNIT(I2),4,I1)
                endif
              endif
            enddo
          enddo
        else
          call fwarning(%ref('Not valid pointer to CACH/JPTS bank'))
        endif
        return
      endif
C
C     use direct caeh
C
      ip = 0
      DO I=1,NCH
        LDATA=LCAEH+NR*(I-1)
        eta = iq(ldata+12)
        phi = iq(ldata+13)
        lay = iq(ldata+14)
        et = q(ldata+8)
        if (  (eta.ge.emin.and.eta.le.emax) .and.
     &        (phi.ge.pmin.and.phi.le.pmax) .and.
     &        (lay.ge.laymin.and.lay.le.laymax) .and.
     &        (et.ge.etmin.and.et.le.etmax) ) then
          ip = ip + 1
          i2 = i2 + i1/4
          I1 = 52
          if (mod(ip,2).eq.1) then
            WRITE(CTEMP,'(4I4,2F10.3,4X,Z8.8,''    '')')
     &      i,eta,phi,lay,q(ldata+7),et,IQ(LDATA+15)
            CALL UCTOH(CTEMP,PRUNIT(I2),4,I1)
          else
            WRITE(CTEMP,'(4I4,2F10.3,4X,Z8.8,A4)')
     &      i,eta,phi,lay,q(ldata+7),et,IQ(LDATA+15),NEWLIN
            CALL UCTOH(CTEMP,PRUNIT(I2),4,I1)
          endif
        endif
      ENDDO
      i2 = i2 + i1/4
      prunit(i2) = 0
C
      END
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine picaehn(type,addr,nchan,emin,emax,pmin,pmax,laymin,
     &  laymax,etmin,etmax)
c
c     gets number of channels in caeh bank
c
c     returns:  nchan
c     input:    emin/max eta
c               pmin/max phi
c               lmin/max layer
c               etmin/max et 
c               addr     address
c               type     0=all, 1=pointer
c
      implicit none
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:DCPARA.DEF/LIST'
      INTEGER LCAEH,GZCAEH
      integer MCHAN
      common /xxcaeh/mchan
      integer type,nr,eta,phi,lay,ldata,i
      real et
c
      integer nchan,emin,emax,pmin,pmax,laymin,laymax,addr,j,iodd
      integer jbyt,nw,nch
      real etmin,etmax
c
      integer ichar,ib
      character*4 cchar
      equivalence (ichar,cchar)
c
      nchan = 0
      LCAEH=GZCAEH()
      IF(LCAEH.LE.0) return
      nr = iq(lcaeh+2)
c
c     use pointer
c
      if (type.eq.1) then
        ichar = iq(addr-4)
        if (cchar.eq.'CACH'.or.cchar.eq.'JPTS') then
          mchan = iq(addr+2)
          do i=1,mchan
            ib = iq(addr+2+i)
            ldata = lcaeh+nr*(ib-1)
            eta = iq(ldata+12)
            phi = iq(ldata+13)
            lay = iq(ldata+14)
            et = q(ldata+8)
            if (  (eta.ge.emin.and.eta.le.emax) .and.
     &        (phi.ge.pmin.and.phi.le.pmax) .and.
     &        (lay.ge.laymin.and.lay.le.laymax) .and.
     &        (et.ge.etmin.and.et.le.etmax) ) nchan = nchan + 1
          enddo
          mchan = nchan
        else if (cchar.eq.'CJPT') then
c
c         cjpt has packed format (2 pointers per word)
c
          iodd = 1
          if (jbyt(iq(addr+iq(addr-1)),1,16).gt.0) iodd = 0
          nw = iq(addr-1) - 2
          nch = nw*2 - iodd
          do i=0,nw-1
            do j=1,2
              ib = jbyt(iq(addr+3+i),((2-j)*16)+1,16)
              ldata = lcaeh + nr*(ib-1)
              eta = iq(ldata+12)
              phi = iq(ldata+13)
              lay = iq(ldata+14)
              et = q(ldata+8)
              if (  (eta.ge.emin.and.eta.le.emax) .and.
     &            (phi.ge.pmin.and.phi.le.pmax) .and.
     &            (lay.ge.laymin.and.lay.le.laymax) .and.
     &            (et.ge.etmin.and.et.le.etmax) ) then
                nchan = nchan + 1
              endif
            enddo
          enddo
          mchan = nchan
        else
          call fwarning(%ref('Not valid pointer to CACH/JPTS bank'))
        endif
        return
      endif
c
c     use direct caeh
c
      mchan = iq(lcaeh+3)
      do i=1,mchan
        ldata = lcaeh + nr*(i-1)
        eta = iq(ldata+12)
        phi = iq(ldata+13)
        lay = iq(ldata+14)
        et = q(ldata+8)
        if (  (eta.ge.emin.and.eta.le.emax) .and.
     &        (phi.ge.pmin.and.phi.le.pmax) .and.
     &        (lay.ge.laymin.and.lay.le.laymax) .and.
     &        (et.ge.etmin.and.et.le.etmax) ) nchan = nchan + 1
      enddo
      mchan = nchan
c
      return
      end
