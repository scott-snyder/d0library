      SUBROUTINE get_pjet_vectors(iarray,pjarray,algo,limit,nparton,
     &  parton_et,parton_eta,parton_phi)

C----------------------------------------------------------------------
C-   Purpose and Methods :  Use PJETS to obtain energy vector information
C-      of partons in event.
C-
C-   Inputs   :
C-                iarray(10)    I   array of pjets parameters
C-                      (1) = nalgo
C-                      (2) = iter
C-                      (3) = irst
C-                      (4) = imuon
C-                pjarray(10)    R   array of pjets parameters
C-                      (1) = et cut used by pjets
C-                      (2) = cone size used by PJETS
C-                      (3) = spl_mrg flag
C-                algo            -- parton/particle level switch
C-                limit           -- maximum number of pjets
C-
C-   Outputs  :
C-                npartons        --  # of partons
C-                parton_et(20)   --  Et of partons
C-                parton_eta(20)  --  eta of partons
C-                parton_phi(20)  --  phi of partons
C-
C-   Created  Feb-11-1994   Bob Kehoe
C-   Updated  Mar-27-1994   Bob Kehoe --  modify way deal with PJETS algo.
C----------------------------------------------------------------------

      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPJET.LINK'
      INTEGER lpjet,gzpjhd,lpjhd,lqpjet
      INTEGER irst(20),imuon(20),iter(20),nparton,limit,iarray(10)
      REAL parton_et(limit),parton_eta(limit),parton_phi(limit)
      REAL etcut(20),spl_mrg(20),par_cone(20),pjarray(10)
      CHARACTER*8 algo,algorithm(20)

C----------------------------------------------------------------------
      iter(1) = iarray(2)
      irst(1) = iarray(3)
      imuon(1) = iarray(4)
      etcut(1) = pjarray(1)
      par_cone(1) = pjarray(2)
      spl_mrg(1) = pjarray(3)
      algorithm(1) = algo
      nparton = 0
      CALL vzero(parton_et,limit)
      CALL vzero(parton_eta,limit)
      CALL vzero(parton_phi,limit)

C-        GET PARTON DIRECTIONS AND TRANSVERSE ENERGIES USING PJETS.
      lpjhd = gzpjhd()
      CALL mzdrop(ixcom,lpjhd,'L')
      CALL pjpset(1,algorithm,etcut,par_cone,iter,irst,imuon,spl_mrg)
      CALL pjetfl
      lpjhd = gzpjhd()
      nparton = 0
      DO WHILE ((lpjhd.GT.0).AND.(int(10.*q(lpjhd+4)).ne.int(10.
     &        *par_cone(1))))
        lpjhd = lq(lpjhd)
      ENDDO
      lpjet = lpjhd - izpjet
      IF (lpjet.GT.0) THEN
        lqpjet = lq(lpjet)
        CALL zsort(ixcom,lqpjet,2)
        lpjet = lpjhd - izpjet
        lqpjet = lq(lpjet)
        CALL ztopsy(ixcom,lqpjet)
        lpjet = lpjhd - izpjet
        DO WHILE((lpjet.GT.0).AND.(nparton.LT.limit))
          lqpjet=lq(lpjet)
          nparton = nparton + 1
          parton_et(nparton) = q(lqpjet+2)
          parton_eta(nparton) = q(lqpjet+10)
          parton_phi(nparton) = q(lqpjet+8)
          lpjet=lqpjet
        ENDDO
      ELSE
        CALL errmsg('missing pjet algo','get_pjet_vectors',
     &        ' found no matching pjet algorithm','W')
      ENDIF

  999 RETURN
      END
