C DEC/CMS REPLACEMENT HISTORY, Element SURVEY.INC
C *5     7-AUG-1992 11:23:58 KAHN "update"
C *4     6-AUG-1992 15:36:45 KAHN "INCLUDE DATE INFO"
C *3     5-AUG-1992 15:14:40 KAHN "SURVEY FOR CC, ECN, ECS"
C *2    30-JUL-1992 15:40:34 KAHN "UPDATE SURVEY ROUTINES"
C *1    14-JUN-1991 19:58:42 KAHN "survey related"
C DEC/CMS REPLACEMENT HISTORY, Element SURVEY.INC
C----------------------------------------------------------------------
C-
C-   Created   2-MAR-1991   Stephen Kahn
C-
C----------------------------------------------------------------------
      COMMON /CALOR_SURVEY/ IDVSURV, LSURV, LCSRV, LCSYL, LCTHE, LCMDL
      INTEGER IDVSURV, LSURV, LCSRV, LCSYL, LCMDL, LCTHE
      INTEGER NWSURV, NWSURV_MX
      INTEGER ISMODL, ISNUMB, ISNMSR, ISNILX, ISNIUX, ISNOLX, ISNOUX
      INTEGER ISSILX, ISSIUX, ISSOLX, ISSOUX, ISMCOD, JBCOLD, NMWDS
      INTEGER ISNIX, ISNOX, ISSIX, ISSOX, ISUCX
      INTEGER ISNTX, ISNBX, ISNEX, ISNWX, ISSTX, ISSBX, ISSEX, ISSWX
      INTEGER ISCYLN, ISREGN, ISDATE, ISDAT2
      INTEGER IRCTHE, IRCMDL, IRCLGA, IRCLGI

      PARAMETER( NWSURV =  25000)
      PARAMETER( NWSURV_MX =  50000)
      PARAMETER( ISREGN =  1)           
      PARAMETER( ISDATE =  9)           
      PARAMETER( ISDAT2 =  7)           
      PARAMETER( ISMODL =  1)           
      PARAMETER( ISNUMB =  2)           
      PARAMETER( ISNMSR =  3)           
                                        
      PARAMETER( ISMCOD =  4)           
                                        
      PARAMETER( ISNILX =  8)           
      PARAMETER( ISNIUX =  12)           
      PARAMETER( ISNOLX =  16)          
      PARAMETER( ISNOUX =  20)          
      PARAMETER( ISSILX =  24)          
      PARAMETER( ISSIUX =  28)          
      PARAMETER( ISSOLX =  32)          
      PARAMETER( ISSOUX =  36)          
      PARAMETER( ISNIX =  8 )           
      PARAMETER( ISNOX =  12)           
      PARAMETER( ISSIX =  16)           
      PARAMETER( ISSOX =  20)           
      PARAMETER( ISNTX =  8 )           
      PARAMETER( ISNWX =  12)           
      PARAMETER( ISNBX =  16)           
      PARAMETER( ISNEX =  20)           
      PARAMETER( ISSTX =  24)           
      PARAMETER( ISSWX =  28)           
      PARAMETER( ISSBX =  32)           
      PARAMETER( ISSEX =  36)           
      PARAMETER( ISUCX =  24)           
      PARAMETER( JBCOLD =  1)           
                                        
                                        
      PARAMETER( NMWDS =  4)            
                                        
      PARAMETER( ISCYLN = 3)            
      PARAMETER( IRCTHE = 2 )           
      PARAMETER( IRCMDL = 2 )           
      PARAMETER( IRCLGA = 1 )           
      PARAMETER( IRCLGI = 3 )           

