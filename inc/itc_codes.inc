c
c *** itc_codes.inc
c
c *** Updated 19-AUG-1994 sss to match current pascal version.
c
      integer 
     &     ITC__SUCCESS,
     &     ITC__TRUNCATED,
     &     ITC__NO_MESSAGE,
     &     ITC__NO_ACTIVITY,
     &     ITC__TARNAMTRUNC,
     &     ITC__CONALRINPL,
     &     ITC__TARGETALRON,
     &     ITC__INDISCO,
     &     ITC__ALREA_DISCO,
     &     ITC__BUFFER_FULL,
     &     ITC__TIMEOUT,
     &     ITC__BAD_ARG,
     &     ITC__NO_CHANNEL,
     &     ITC__NO_PARTNER,
     &     ITC__NO_TARGET,
     &     ITC__NO_ANSWER,
     &     ITC__EXCHANQUO,
     &     ITC__BAD_CHAN,
     &     ITC__CONREJECT,
     &     ITC_K_MESSAGE,
     &     ITC_K_ESTABLISH,
     &     ITC_K_BROKEN

      parameter (
     &     ITC__SUCCESS	        = '08938009'X,
     &     ITC__TRUNCATED	= '08938083'X,
     &     ITC__NO_MESSAGE	= '0893808B'X,
     &     ITC__NO_ACTIVITY     = '08938093'X,
     &     ITC__TARNAMTRUNC     = '0893809B'X,
     &     ITC__CONALRINPL	= '089380A3'X,
     &     ITC__TARGETALRON     = '089380AB'X,
     &     ITC__INDISCO 	= '089380B0'X,
     &     ITC__ALREA_DISCO     = '089380B8'X,
     &     ITC__BUFFER_FULL     = '089380C0'X,
     &     ITC__TIMEOUT	        = '089380C8'X,
     &     ITC__BAD_ARG	        = '08938102'X,
     &     ITC__NO_CHANNEL	= '0893810A'X,
     &     ITC__NO_PARTNER	= '08938112'X,
     &     ITC__NO_TARGET	= '0893811A'X,
     &     ITC__NO_ANSWER	= '08938122'X,
     &     ITC__EXCHANQUO	= '0893812A'X,
     &     ITC__BAD_CHAN	= '08938132'X,
     &     ITC__CONREJECT	= '0893813A'X,
     &     ITC_K_MESSAGE	= '00000001'X,
     &     ITC_K_ESTABLISH	= '00000002'X,
     &     ITC_K_BROKEN	        = '00000003'X
     &     )
