      SUBROUTINE UDST_GET_MUON(LPMUO,XDATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fill muon parameteres into array XDATA
C-
C-   Inputs  : LPMUO - pointer to muon bank
C-   Outputs : XDATA(N)
C-   Controls:
C-
C-   Created   6-MAY-1993   Cecilia E. Gerber
C-   Updated   7-DEC-1993   Ian Adam
C-    Remove tracking information (link to track object is kept instead)
C-   Updated  15-DEC-1993   Ian Adam
C-    Make tags and booking the same structure as other UDST code
C-    split trigger word bitmasks
C-    sign the momentum P to store the charge, drop particle ID (=+/-14)
C-   Updated  14-JAN-1994   Ian Adam  Check ZTRK link before calling
C-    UDST_GET_TRACK_LINK
C-   Updated  23-JAN-1994   Ulrich Heintz  add words 23,14,15 from MUOT
C-   Updated  20-FEB-1994   Ulrich Heintz  add words 11, 12, 17, 18 from MUOT 
C-   Updated  12-MAR-1994   Ulrich Heintz  split words 46 and 47 from PMUO 
C-   UPDATED 10-13-94 DAVID HEDIN Kluge for SAMUS tracks
C-   Updated  30-SEP-1995   Ian Adam  Change requested by Dmitri 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'

      INTEGER   LPMUO,NS,LMUOT,LMUON,LZTRK,UDST_TRACK_LINK,I
      REAL      CHARGE,OUT(2)

      INTEGER KMUON,KMUON1,ID_MUON
      PARAMETER(KMUON=118)
      CHARACTER*8 TAGS(KMUON)
      DATA TAGS/
     & 'DEDXM' ,'METHCM','FLAGM' ,'NUMCDM','QUADM' ,'METHFM',
     & 'IFW4M' ,'PM'    ,'PTM'   ,'THETAM',
     & 'ETAM'  ,'PHIM'  ,'SIGPM' ,
     & 'CHISQM','T0FLTM','XTRAM' ,'YTRAM' ,'ZTRAM' ,'ISOL0M','ISOL1M',
     & 'ISOL2M','ISOL4M','ISOL6M','ELSSCM','ENET2M','ENET4M','ENET6M',
     & 'DANGLM','DPHIM' ,'DTHEM' ,'CONEM' ,'IMPVM' ,'IMPVGM','ELSSFM',
     & 'IFW2M' ,'IFWUM' ,'HITTM_','HITTM ','HITFM_','HITFM ','TRG1M_',
     & 'TRG1M' ,'TRG2M_',
     & 'TRG2M' ,'TRG3M_','TRG3M' ,'TRG4M_','TRG4M' ,'VERT1M','VERT2M',
     & 'IMPMBM','IMPMNM','IMPCBM','IMPCNM','X1M'   ,'Y1M'   ,'Z1M'   ,
     & 'DX1M'  ,'DY1M'  ,'DZ1M'  ,'X2M'   ,'Y2M'   ,'Z2M'   ,'DX2M'  ,
     & 'DY2M'  ,'DZ2M'  ,'X3M'   ,'Y3M'   ,'Z3M'   ,'DX3M'  ,'DY3M'  ,
     & 'DZ3M'  ,'ENEM0M','ENEM1M','ENEM2M','ENEM4M','ENEM6M','ENET0M',
     & 'ENET1M','ENEB0M','ENEB1M','ENEB2M','ENEB3M','IFW1M' ,'IFW3M' ,
     & 'BVM'   ,'NBVM'  ,'BDLM'  ,'DX2MUT','DY2MUT','PMUOT' ,'DX3MUT',
     & 'DY3MUT','X3MUOT','Y3MUOT','E6CM'  ,'E7CM'  ,'E8CM'  ,'ET6CM' ,
     & 'ET7CM' ,'ET8CM' ,'TKNMU' ,'EEMM'  ,'EFHM'  ,'ECHM'  ,'ETRKM' ,
     & 'TRESM' ,'TRESVM','FRACTM','HFRCM' ,'GHFRCM','ECHIM' ,'EN3M'  ,
     & 'EFRCHM','LYRM'  ,'ECHI2M','SCTOFM','SCEXPM'/
      REAL
     & DEDXM ,METHCM,FLAGM ,NUMCDM,QUADM ,METHFM,
     & IFW4M ,PM    ,PTM   ,THETAM,
     & ETAM  ,PHIM  ,SIGPM ,
     & CHISQM,T0FLTM,XTRAM ,YTRAM ,ZTRAM ,ISOL0M,ISOL1M,
     & ISOL2M,ISOL4M,ISOL6M,ELSSCM,ENET2M,ENET4M,ENET6M,
     & DANGLM,DPHIM ,DTHEM ,CONEM ,IMPVM ,IMPVGM,ELSSFM,
     & IFW2M ,IFWUM ,HITTM_,HITTM ,HITFM_,HITFM ,TRG1M_,
     & TRG1M ,TRG2M_,
     & TRG2M ,TRG3M_,TRG3M ,TRG4M_,TRG4M ,VERT1M,VERT2M,
     & IMPMBM,IMPMNM,IMPCBM,IMPCNM,X1M   ,Y1M   ,Z1M   ,
     & DX1M  ,DY1M  ,DZ1M  ,X2M   ,Y2M   ,Z2M   ,DX2M  ,
     & DY2M  ,DZ2M  ,X3M   ,Y3M   ,Z3M   ,DX3M  ,DY3M  ,
     & DZ3M  ,ENEM0M,ENEM1M,ENEM2M,ENEM4M,ENEM6M,ENET0M,
     & ENET1M,ENEB0M,ENEB1M,ENEB2M,ENEB3M,IFW1M ,IFW3M ,
     & BVM   ,NBVM  ,BDLM  ,DX2MUT,DY2MUT,PMUOT ,DX3MUT,
     & DY3MUT,X3MUOT,Y3MUOT,E6CM  ,E7CM  ,E8CM  ,ET6CM ,
     & ET7CM ,ET8CM ,TKNMU ,EEMM  ,EFHM  ,ECHM  ,ETRKM ,
     & TRESM ,TRESVM,FRACTM,HFRCM ,GHFRCM,ECHIM ,EN3M  ,
     & EFRCHM,LYRM  ,ECHI2M,SCTOFM,SCEXPM
      COMMON/PMUO_OBJECT/
     & DEDXM ,METHCM,FLAGM ,NUMCDM,QUADM ,METHFM,
     & IFW4M ,PM    ,PTM   ,THETAM,
     & ETAM  ,PHIM  ,SIGPM ,
     & CHISQM,T0FLTM,XTRAM ,YTRAM ,ZTRAM ,ISOL0M,ISOL1M,
     & ISOL2M,ISOL4M,ISOL6M,ELSSCM,ENET2M,ENET4M,ENET6M,
     & DANGLM,DPHIM ,DTHEM ,CONEM ,IMPVM ,IMPVGM,ELSSFM,
     & IFW2M ,IFWUM ,HITTM_,HITTM ,HITFM_,HITFM ,TRG1M_,
     & TRG1M ,TRG2M_,
     & TRG2M ,TRG3M_,TRG3M ,TRG4M_,TRG4M ,VERT1M,VERT2M,
     & IMPMBM,IMPMNM,IMPCBM,IMPCNM,X1M   ,Y1M   ,Z1M   ,
     & DX1M  ,DY1M  ,DZ1M  ,X2M   ,Y2M   ,Z2M   ,DX2M  ,
     & DY2M  ,DZ2M  ,X3M   ,Y3M   ,Z3M   ,DX3M  ,DY3M  ,
     & DZ3M  ,ENEM0M,ENEM1M,ENEM2M,ENEM4M,ENEM6M,ENET0M,
     & ENET1M,ENEB0M,ENEB1M,ENEB2M,ENEB3M,IFW1M ,IFW3M ,
     & BVM   ,NBVM  ,BDLM  ,DX2MUT,DY2MUT,PMUOT ,DX3MUT,
     & DY3MUT,X3MUOT,Y3MUOT,E6CM  ,E7CM  ,E8CM  ,ET6CM ,
     & ET7CM ,ET8CM ,TKNMU ,EEMM  ,EFHM  ,ECHM  ,ETRKM ,
     & TRESM ,TRESVM,FRACTM,HFRCM ,GHFRCM,ECHIM ,EN3M  ,
     & EFRCHM,LYRM  ,ECHI2M,SCTOFM,SCEXPM
      REAL    XX(KMUON),XDATA(KMUON)
      EQUIVALENCE(XX,DEDXM)
C----------------------------------------------------------------------
      CALL VZERO(XX,KMUON)
C
C links to MUON and MUOT banks
C
      NS = IQ(LPMUO-2)
      LMUOT = LQ(LPMUO-NS-1)
      LMUON = LQ(LPMUO-NS-2)
C
C get PMUO information
C
      IF(IQ(LPMUO+1).GE.3.)THEN !  get muon parameters for new PMUO version
        DEDXM  = FLOAT(IQ(LPMUO+3)) ! flag for dedx
        METHCM = FLOAT(IQ(LPMUO+4)) ! method of calculation
        FLAGM  = FLOAT(IQ(LPMUO+5)) ! flag where track vector is defined
        NUMCDM = FLOAT(IQ(LPMUO+6)) ! number of CD tracks is cone
        QUADM  = FLOAT(IQ(LPMUO+7)) ! quadrant
        METHFM = FLOAT(IQ(LPMUO+8)) ! method of fit
        IFW4M  = FLOAT(IQ(LPMUO+9)) ! IFW4 quality flag
        IF (IQ(LPMUO+2).NE.0) THEN
          CHARGE=FLOAT(IQ(LPMUO+2))/ABS(FLOAT(IQ(LPMUO+2)))
        ELSE
          CALL ERRMSG('PMUO','UDST_GET_MUON','MU CHARGE ZERO','W')
          CHARGE=0.0
        ENDIF
        PM     = Q(LPMUO+13)*CHARGE ! P, sign(P) gives the charge
        PTM    = Q(LPMUO+14) ! Pt
        THETAM = Q(LPMUO+15) ! theta
        ETAM   = Q(LPMUO+16) ! eta
        PHIM   = Q(LPMUO+17) ! Phi
        SIGPM  = Q(LPMUO+21) ! (sigP)**2
        CHISQM = Q(LPMUO+23) ! chisq / deg of freedom
        T0FLTM = Q(LPMUO+24) ! floated t0 offset (ns)
        XTRAM  = Q(LPMUO+25) ! x coordinate where track vector defined
        YTRAM  = Q(LPMUO+26) ! y coordinate where track vector defiend
        ZTRAM  = Q(LPMUO+27) ! z coordinate where track vector defiend
        ISOL0M = Q(LPMUO+28) ! isolation (based on cells hit only)
        ISOL1M = Q(LPMUO+29) ! isolation (based on cells hit+neighbors)
        ISOL2M = Q(LPMUO+30) ! isolation (based on cells hit+ 2 n'bors)
        ISOL4M = Q(LPMUO+31) ! isolation (based on cone size 0.4)
        ISOL6M = Q(LPMUO+32) ! isolation (based on cone size 0.6)
        ELSSCM = Q(LPMUO+33) ! E loss expected in CAL
        ENET2M = Q(LPMUO+34) ! E observed in CAL (in cells hit+2 neighbors)
        ENET4M = Q(LPMUO+35) ! E observed in CAL (in cone 0.4)
        ENET6M = Q(LPMUO+36) ! E observed in CAL (in cone 0.6)
        DANGLM = Q(LPMUO+37) ! Angle between muon & nearest CD track (deg)
        DPHIM  = Q(LPMUO+38) ! D_phi (deg)
        DTHEM  = Q(LPMUO+39) ! D_theta (deg)
        CONEM  = Q(LPMUO+40) ! cone size considered for CD track finding
        IMPVM  = Q(LPMUO+41) ! impact parameter (measured from vertex)
        IMPVGM = Q(LPMUO+42) ! impact parameter after global fit
        ELSSFM = Q(LPMUO+43) ! E loss in muon system
        IFW2M  = FLOAT(IQ(LPMUO+44)) ! IFW2 quality flag
        IFWUM  = FLOAT(IQ(LPMUO+45)) ! user flag word
        CALL SPLIT_BITMASK(IQ(LPMUO+46),OUT)
        HITTM_ = OUT(1) ! hits on track, A,B,C
        HITTM  = OUT(2) ! hits on track, A,B,C
        CALL SPLIT_BITMASK(IQ(LPMUO+47),OUT)
        HITFM_ = OUT(1) ! hits in track fit, A,B,C
        HITFM  = OUT(2) ! hits in track fit, A,B,C
        CALL SPLIT_BITMASK(IQ(LPMUO+48),OUT)
        TRG1M_ = OUT(1) ! trigger I 16 LSB
        TRG1M  = OUT(2) ! trigger I 16 MSB
        CALL SPLIT_BITMASK(IQ(LPMUO+49),OUT)
        TRG2M_ = OUT(1) ! trigger I 16 LSB
        TRG2M  = OUT(2) ! trigger I 16 MSB
        CALL SPLIT_BITMASK(IQ(LPMUO+50),OUT)
        TRG3M_ = OUT(1) ! trigger I 16 LSB
        TRG3M  = OUT(2) ! trigger I 16 MSB
        CALL SPLIT_BITMASK(IQ(LPMUO+51),OUT)
        TRG4M_ = OUT(1) ! trigger I 16 LSB
        TRG4M  = OUT(2) ! trigger I 16 MSB
        VERT1M = FLOAT(IQ(LPMUO+54)) ! Vertex used
        VERT2M = FLOAT(IQ(LPMUO+55)) ! Vertex number (by calorimeter info)
        IMPMBM = Q(LPMUO+56) ! impact parameter, bend view, muon only
        IMPMNM = Q(LPMUO+57) ! impact parameter, nonbend view, muon only
        IMPCBM = Q(LPMUO+58) ! impact parameter, bend view, incl. CD
        IMPCNM = Q(LPMUO+59) ! impact parameter, nonbend view, incl. CD
        X1M    = Q(LPMUO+60) ! x            \
        Y1M    = Q(LPMUO+61) ! y             > at vertex
        Z1M    = Q(LPMUO+62) ! z            /
        DX1M   = Q(LPMUO+63) ! x dir cosine \
        DY1M   = Q(LPMUO+64) ! y dir cosine  > at vertex
        DZ1M   = Q(LPMUO+65) ! z dir cosine /
        X2M    = Q(LPMUO+66) ! x            \
        Y2M    = Q(LPMUO+67) ! y             > at calorimeter (backside)
        Z2M    = Q(LPMUO+68) ! z            /
        DX2M   = Q(LPMUO+69) ! x dir cosine \
        DY2M   = Q(LPMUO+70) ! y dir cosine  > at calorimeter (backside)
        DZ2M   = Q(LPMUO+71) ! z dir cosine /
        X3M    = Q(LPMUO+72) ! x            \
        Y3M    = Q(LPMUO+73) ! y             > outside torroid
        Z3M    = Q(LPMUO+74) ! z            /
        DX3M   = Q(LPMUO+75) ! x dir cosine \
        DY3M   = Q(LPMUO+76) ! y dir cosine  > outside torroid
        DZ3M   = Q(LPMUO+77) ! z dir cosine /
        ENEM0M = Q(LPMUO+78) ! EM energy in cells supposedly hit by muon
        ENEM1M = Q(LPMUO+79) ! hit cells and 1 nearest neighbors
        ENEM2M = Q(LPMUO+80) ! hit cells and 2 nearest neighbors
        ENEM4M = Q(LPMUO+81) ! hit cells and 4 nearest neighbors
        ENEM6M = Q(LPMUO+82) ! hit cells and 6 nearest neighbors
        ENET0M = Q(LPMUO+83) ! Total E observed in CAL (in cells hit)
        ENET1M = Q(LPMUO+84) ! Total E (in cells hit + 1 neighbor)
        ENEB0M = Q(LPMUO+86) ! Total energy in hit cells on opposit side
        ENEB1M = Q(LPMUO+87) ! hit cells and 1 nearest neighbors
        ENEB2M = Q(LPMUO+88) ! hit cells and 2 nearest neighbors
        ENEB3M = Q(LPMUO+89) ! hit cells and 3 nearest neighbors
        ETRKM  = Q(LPMUO+90)
        TRESM  = Q(LPMUO+91)
        TRESVM = Q(LPMUO+92)
        FRACTM = Q(LPMUO+93)
        HFRCM  = Q(LPMUO+94)
        GHFRCM = Q(LPMUO+95)
        ECHIM  = Q(LPMUO+96)
        EN3M   = Q(LPMUO+97)
        EFRCHM = Q(LPMUO+98) 
        LYRM   = Q(LPMUO+99)
        ECHI2M = Q(LPMUO+100)  
        SCTOFM = Q(LPMUO+52)
        SCEXPM = Q(LPMUO+53)
      ELSE                          ! old PMUO version
        CALL ERRMSG('OLD_PMUO_VERSION','UDST_GET_MUON','skip muon','W')
        GOTO 999
      ENDIF
C
C MUOT information
C
      IF(LMUOT.GT.0) THEN
        IFW1M  = FLOAT(IQ(LMUOT+4)) ! IFW1 flag word
        IFW3M  = FLOAT(IQ(LMUOT+6)) ! IFW3 flag word
        BVM    = Q(LMUOT+20)  ! bend view quality of fit
        NBVM   = Q(LMUOT+21)  ! nonbend view quality of fit
        BDLM   = Q(LMUOT+22)  ! BDL
        DX2MUT = Q(LMUOT+14)  ! x dir cosine        \
        DY2MUT = Q(LMUOT+15)  ! y dir cosine         > muon RECO only
        PMUOT  = Q(LMUOT+23)  ! signed muon momentum/
        X3MUOT = Q(LMUOT+11) ! x            \
        Y3MUOT = Q(LMUOT+12) ! y             > outside torroid
        DX3MUT = Q(LMUOT+17) ! x dir cosine  |
        DY3MUT = Q(LMUOT+18) ! y dir cosine /
      ENDIF
C
C MUCA stuff from Gene Alvarez:
C
      CALL GET_MUCA_ENERGY(LPMUO,E6CM,E7CM,E8CM,ET6CM,ET7CM,ET8CM)
c-
c-Wyatt's new routine 
c-     

      CALL GET_MUCA_ENERGY2(LPMUO,EEMM,EFHM,ECHM)
C
C central tracking information
C
      LZTRK=LQ(LPMUO-NS-4)
      IF (LZTRK.GT.0) THEN
        CALL UDST_GET_TRACK_LINK(LZTRK,UDST_TRACK_LINK)
      ELSE
        UDST_TRACK_LINK=0
      ENDIF
      TKNMU = FLOAT(UDST_TRACK_LINK)
C
      DO I=1,KMUON
        XDATA(I) = XX(I)
      ENDDO
C----------------------------------------------------------------------
  999 RETURN

      ENTRY UDST_MUON_TAGS(KMUON1,ID_MUON)

      KMUON1=KMUON
      ID_MUON=8
      CALL UDST_BOOK_GROUP(ID_MUON,'PMUO',TAGS,KMUON)

      RETURN
      END
