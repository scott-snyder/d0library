C  $$$$$$   HAVING TROUBLE WITH CMSUPDATE ON THIS ROUTINE!!!
      SUBROUTINE SETDET(NMSRCP,SCAL,ISET,IDET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sets up detectorS, HITS AND digitizations
C-                          read in from SRCP
C-
C-   Inputs  : NMSRCP = Character string SRCP ident
C-             SCAL(1)= IF.EQ.1 AMJ TOWERS. SO ONLY DO GSDET with minimal
C-                      Geant space reservations and no GSDETH and GSDETD
C-                      Reader should be familiar with GEANT - HITS and DIGI
C-   Outputs : ISET,IDET  - See Geant Manual
C-   Controls: None
C-
C-   Created   3-OCT-1988   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NMSRCP
      CHARACTER*32 NMSRC1
      REAL SCAL(*)
      INTEGER ISET,IDET, iii
      INTEGER LSET(1500),IS,LEN3
      INTEGER IUSET,IUDET,IDTYPE,NWHI,NWDI,NSET,NV,NH,ND,IOFF
      character*4 iusetc, iudetc, namesv(20), namesh(20), namesd(20)
C----------------------------------------------------------------------
C
C The format of the LSET array is as follows.
C
C LSET(1) = IUSET  = Detector set name
C LSET(2) = NWHI   = number of words for primary allocation of HITS banks
C LSET(3) = NWDI   = number of words for DIGI banks when first allocation
C                    not sufficient.
C LSET(4) = ITRS   = Number of selected track in GFHITS. 0= all tracks
C LSET(5) = NHMAX  = Maximum number of hits to be returned by GFHITS
C LSET(6) = NSET   = number of detectors IUDET belonging to the set IUSET.
C                    The rest will be repeated NSET times.
C--------------
C| IUDET      = Geometry Volume name being declared as belonging to IUSET
C| IDTYPE     = Flag set by user to easily tell the IUDET in GUSTEP
C| NV         = Number of Volume descriptors needed to identify IUSET
C|              uniquely.
C| NAMESV(NV) = vector of NV names to describe the geometry uniquely
C| NBITSV(NV) = vector of NV numbers to describe bits needed for packing
C|              volume numbers
C| NH         = Number of elements stored as HITS
C| NAMESH(NH) = vector of NH variable names for hit elements
C| NBITSH(NH) = vector of NH numbers describing bit numbers for packing Hit
C|              values
C| ORIG(NH)   = vector of NH ORIG values (see GEANT manual under GSDETH)
C| FACT(NH)   = vector of NH FACT values (see GEANT manual under GSDETH)
C| ND         = Number of elements per digitization (GSDETD)
C| NAMESD(ND) = vector of ND names for digitizations
C| NBITSD(ND) = vector of ND bit numbers for packing digitizations
C--------------
      CALL ADDSTR(NMSRCP,'(1)',NMSRC1,LEN3)   !Makes it into array format
      CALL GTSRCP(NMSRC1,LSET(1),1)
C
      IUSET = LSET(1)
      NWHI = LSET(2)
      NWDI = LSET(3)
C
      IF(SCAL(1).NE.0)THEN
        NWHI = 1  !Minimal storage for AMJ towers
        NWDI = 1
      ENDIF
      IOFF = 6
      NSET = LSET(IOFF)
      DO 40 IS = 1 , NSET
        IUDET = LSET(IOFF+1)
        IDTYPE = LSET(IOFF+2)
        NV = LSET(IOFF+3)
        call uhtoc(iuset,4,iusetc,4)
        call uhtoc(iudet,4,iudetc,4)
        do iii=1,nv
           call uhtoc(lset(ioff+3+nv),4,namesv(iii),4)
        enddo
        CALL GSDET(IUSETc,IUDETc,NV,namesv(1),LSET(IOFF+4+NV),IDTYPE,
     &            NWHI,NWDI,ISET,IDET)
C
        IOFF = IOFF+4+2*NV
        NH = LSET(IOFF)
        do iii=1,nh
             call uhtoc(lset(ioff+1),4,namesh(iii),4)
        enddo
        IF(SCAL(1).EQ.0)THEN !Only setup if Not AMJ towers
          CALL GSDETH(IUSETc,IUDETc,NH,namesh(1),LSET(IOFF+1+NH),
     &              LSET(IOFF+1+2*NH),LSET(IOFF+1+3*NH))
        ENDIF
C
        IOFF = IOFF+1+4*NH
        ND = LSET(IOFF)
        do iii=1,nd
             call uhtoc(lset(ioff+1),4,namesd(iii),4)
        enddo
        IF(SCAL(1).EQ.0)THEN !Only setup if not AMJ towers
          CALL GSDETD(IUSETc,IUDETc,ND,namesd(1),LSET(IOFF+1+ND))
        ENDIF
        IOFF = IOFF+2*ND
   40 CONTINUE
C
  999 RETURN
      END
