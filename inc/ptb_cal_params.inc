C----------------------------------------------------------------------
C-
C-   Created  18-JAN-1990   C.E.Cretsinger
C-
C-   Modified 15-FEB-1990 to include ecih section.--C.E.C.
C----------------------------------------------------------------------
      COMMON /PTB_CAL_PARAMS/ CELLS,PHI_VALUES,COS_VALUES,
     &  SIN_VALUES,ZLAYER,RADII,MHXOFF,XMHSID,YMHSID,
     &  NETA,NPHI,NLAYER,MAXNRG,MAXHITE,LOGSCL,ICOLR,NCOLS,
     &  PFILL,ALTGRAY,LBLS,XBEAM,YBEAM
      
      CHARACTER*4 LBLS(14)
      REAL CELLS(34,128,14),PHI_VALUES(128,34,14)
      REAL COS_VALUES(128,34,14),SIN_VALUES(128,34,14)
      REAL ZLAYER(14), RADII(35,14)
      REAL MHXOFF
      REAL XMHSID(5),YMHSID(5)          
      INTEGER NETA(14),NPHI(34,14),NLAYER,NCOLS,PFILL
      REAL MAXNRG,MAXHITE
      REAL XBEAM(14),YBEAM(14)
      LOGICAL LOGSCL,ICOLR,ALTGRAY

      COMMON /PTBOPT/ INOPIX,IDEMO,IGEOM,IGNEDG
      INTEGER INOPIX                    
      INTEGER IDEMO                     
      INTEGER IGEOM                     
                                        
      LOGICAL IGNEDG                    

      COMMON/PESUMS/ESUMLR(14),ESUM(4),EAVLR(14),EAVSUM(4),PAV,MEVTS
      REAL ESUMLR,ESUM,EAVLR,EAVSUM,PAV
      INTEGER MEVTS
C  ENERGY SUMS FOR EACH EVENT AND RUNNING AVERAGES

