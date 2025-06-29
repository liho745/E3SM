module RtmHistFlds

!-----------------------------------------------------------------------
! !DESCRIPTION:
! Module containing initialization of RTM history fields and files
! This is the module that the user must modify in order to add new
! history fields or modify defaults associated with existing history
! fields.
!
! !USES:
  use shr_kind_mod   , only: r8 => shr_kind_r8
  use RunoffMod      , only : rtmCTL
  use RtmHistFile    , only : RtmHistAddfld, RtmHistPrintflds
  use RtmVar         , only : wrmflag, inundflag, sediflag, heatflag, river_bgc, rstraflag, use_ocn_rof_two_way

  use WRM_type_mod  , only : ctlSubwWRM, WRMUnit, StorWater

  use rof_cpl_indices, only : nt_rtm, rtm_tracers  

  implicit none
!
! !PUBLIC MEMBER FUNCTIONS:
  public :: RtmHistFldsInit 
  public :: RtmHistFldsSet
!
!------------------------------------------------------------------------

contains

!-----------------------------------------------------------------------

  subroutine RtmHistFldsInit()

    !-------------------------------------------------------
    ! DESCRIPTION:
    ! Build master field list of all possible fields in a history file.
    ! Each field has associated with it a ``long\_name'' netcdf attribute that
    ! describes what the field is, and a ``units'' attribute. A subroutine is
    ! called to add each field to the masterlist.
    !
    ! ARGUMENTS:
    implicit none
    !-------------------------------------------------------

    call RtmHistAddfld (fname='MASK', units='1',  &
         avgflag='A', long_name='MOSART mask 1=land 2=ocean 3=outlet ', &
         ptr_rof=rtmCTL%rmask, default='active')

    call RtmHistAddfld (fname='GINDEX', units='1',  &
         avgflag='A', long_name='MOSART global index ', &
         ptr_rof=rtmCTL%rgindex, default='active')

    call RtmHistAddfld (fname='DSIG', units='1',  &
         avgflag='A', long_name='MOSART downstream index ', &
         ptr_rof=rtmCTL%rdsig, default='active')

    call RtmHistAddfld (fname='OUTLETG', units='1',  &
         avgflag='A', long_name='MOSART outlet index ', &
         ptr_rof=rtmCTL%routletg, default='active')

    call RtmHistAddfld (fname='RIVER_DISCHARGE_OVER_LAND'//'_'//trim(rtm_tracers(1)), units='m3/s',  &
         avgflag='A', long_name='MOSART river basin flow: '//trim(rtm_tracers(1)), &
         ptr_rof=rtmCTL%runofflnd_nt1, default='active')

    call RtmHistAddfld (fname='RIVER_DISCHARGE_OVER_LAND'//'_'//trim(rtm_tracers(2)), units='m3/s',  &
         avgflag='A', long_name='MOSART river basin flow: '//trim(rtm_tracers(2)), &
         ptr_rof=rtmCTL%runofflnd_nt2, default='active')

    call RtmHistAddfld (fname='RIVER_DISCHARGE_TO_OCEAN'//'_'//trim(rtm_tracers(1)), units='m3/s',  &
         avgflag='A', long_name='MOSART river discharge into ocean: '//trim(rtm_tracers(1)), &
         ptr_rof=rtmCTL%runoffocn_nt1, default='active')

    call RtmHistAddfld (fname='RIVER_DISCHARGE_TO_OCEAN'//'_'//trim(rtm_tracers(2)), units='m3/s',  &
         avgflag='A', long_name='MOSART river discharge into ocean: '//trim(rtm_tracers(2)), &
         ptr_rof=rtmCTL%runoffocn_nt2, default='active')

    call RtmHistAddfld (fname='TOTAL_DISCHARGE_TO_OCEAN'//'_'//trim(rtm_tracers(1)), units='m3/s', &
         avgflag='A', long_name='MOSART total discharge into ocean: '//trim(rtm_tracers(1)), &
         ptr_rof=rtmCTL%runofftot_nt1, default='active')

    call RtmHistAddfld (fname='TOTAL_DISCHARGE_TO_OCEAN'//'_'//trim(rtm_tracers(2)), units='m3/s', &
         avgflag='A', long_name='MOSART total discharge into ocean: '//trim(rtm_tracers(2)), &
         ptr_rof=rtmCTL%runofftot_nt2, default='active')

    call RtmHistAddfld (fname='DIRECT_DISCHARGE_TO_OCEAN'//'_'//trim(rtm_tracers(1)), units='m3/s', &
         avgflag='A', long_name='MOSART direct discharge into ocean: '//trim(rtm_tracers(1)), &
         ptr_rof=rtmCTL%runoffdir_nt1, default='active')

    call RtmHistAddfld (fname='DIRECT_DISCHARGE_TO_OCEAN'//'_'//trim(rtm_tracers(2)), units='m3/s', &
         avgflag='A', long_name='MOSART direct discharge into ocean: '//trim(rtm_tracers(2)), &
         ptr_rof=rtmCTL%runoffdir_nt2, default='active')

    call RtmHistAddfld (fname='Main_Channel_STORAGE'//'_'//trim(rtm_tracers(1)), units='m3',  &
         avgflag='A', long_name='MOSART main channel storage: '//trim(rtm_tracers(1)), &
         ptr_rof=rtmCTL%wr_nt1, default='active')

    call RtmHistAddfld (fname='STORAGE'//'_'//trim(rtm_tracers(1)), units='m3',  &
         avgflag='A', long_name='MOSART storage: '//trim(rtm_tracers(1)), &
         ptr_rof=rtmCTL%volr_nt1, default='active')

    call RtmHistAddfld (fname='STORAGE'//'_'//trim(rtm_tracers(2)), units='m3',  &
         avgflag='A', long_name='MOSART storage: '//trim(rtm_tracers(2)), &
         ptr_rof=rtmCTL%volr_nt2, default='active')

    call RtmHistAddfld (fname='DISCHARGE_FROM_TRIBUTARY'//'_'//trim(rtm_tracers(1)), units='m3/s',  &
         avgflag='A', long_name='MOSART river flow in tributaries: '//trim(rtm_tracers(1)), &
         ptr_rof=rtmCTL%QTrib_nt1, default='active')

    call RtmHistAddfld (fname='DISCHARGE_FROM_TRIBUTARY'//'_'//trim(rtm_tracers(2)), units='m3/s',  &
         avgflag='A', long_name='MOSART river flow in tributaries: '//trim(rtm_tracers(2)), &
         ptr_rof=rtmCTL%QTrib_nt2, default='active')

    call RtmHistAddfld (fname='DVOLRDT_LND'//'_'//trim(rtm_tracers(1)), units='m3/s',  &
         avgflag='A', long_name='MOSART land change in storage: '//trim(rtm_tracers(1)), &
         ptr_rof=rtmCTL%dvolrdtlnd_nt1, default='inactive')

    call RtmHistAddfld (fname='DVOLRDT_LND'//'_'//trim(rtm_tracers(2)), units='m3/s',  &
         avgflag='A', long_name='MOSART land change in storage: '//trim(rtm_tracers(2)), &
         ptr_rof=rtmCTL%dvolrdtlnd_nt2, default='inactive')

    call RtmHistAddfld (fname='DVOLRDT_OCN'//'_'//trim(rtm_tracers(1)), units='m3/s',  &
         avgflag='A', long_name='MOSART ocean change of storage: '//trim(rtm_tracers(1)), &
         ptr_rof=rtmCTL%dvolrdtocn_nt1, default='inactive')

    call RtmHistAddfld (fname='DVOLRDT_OCN'//'_'//trim(rtm_tracers(2)), units='m3/s',  &
         avgflag='A', long_name='MOSART ocean change of storage: '//trim(rtm_tracers(2)), &
         ptr_rof=rtmCTL%dvolrdtocn_nt2, default='inactive')

    call RtmHistAddfld (fname='QSUR'//'_'//trim(rtm_tracers(1)), units='m3/s',  &
         avgflag='A', long_name='MOSART input surface runoff: '//trim(rtm_tracers(1)), &
         ptr_rof=rtmCTL%qsur_nt1, default='active')

    call RtmHistAddfld (fname='QSUR'//'_'//trim(rtm_tracers(2)), units='m3/s',  &
         avgflag='A', long_name='MOSART input surface runoff: '//trim(rtm_tracers(2)), &
         ptr_rof=rtmCTL%qsur_nt2, default='active')

    call RtmHistAddfld (fname='QSUB'//'_'//trim(rtm_tracers(1)), units='m3/s',  &
         avgflag='A', long_name='MOSART input subsurface runoff: '//trim(rtm_tracers(1)), &
         ptr_rof=rtmCTL%qsub_nt1, default='active')

    call RtmHistAddfld (fname='QSUB'//'_'//trim(rtm_tracers(2)), units='m3/s',  &
         avgflag='A', long_name='MOSART input subsurface runoff: '//trim(rtm_tracers(2)), &
         ptr_rof=rtmCTL%qsub_nt2, default='active')

    call RtmHistAddfld (fname='QGWL'//'_'//trim(rtm_tracers(1)), units='m3/s',  &
         avgflag='A', long_name='MOSART input GWL runoff: '//trim(rtm_tracers(1)), &
         ptr_rof=rtmCTL%qgwl_nt1, default='active')

    call RtmHistAddfld (fname='QGWL'//'_'//trim(rtm_tracers(2)), units='m3/s',  &
         avgflag='A', long_name='MOSART input GWL runoff: '//trim(rtm_tracers(2)), &
         ptr_rof=rtmCTL%qgwl_nt2, default='active')

    call RtmHistAddfld (fname='QDTO'//'_'//trim(rtm_tracers(1)), units='m3/s',  &
         avgflag='A', long_name='MOSART input direct to ocean runoff: '//trim(rtm_tracers(1)), &
         ptr_rof=rtmCTL%qdto_nt1, default='active')

    call RtmHistAddfld (fname='QDTO'//'_'//trim(rtm_tracers(2)), units='m3/s',  &
         avgflag='A', long_name='MOSART input direct to ocean runoff: '//trim(rtm_tracers(2)), &
         ptr_rof=rtmCTL%qdto_nt2, default='active')

    call RtmHistAddfld (fname='QDEM'//'_'//trim(rtm_tracers(1)), units='m3/s',  &
         avgflag='A', long_name='MOSART total demand: '//trim(rtm_tracers(1)), &
         ptr_rof=rtmCTL%qdem_nt1, default='active')

    call RtmHistAddfld (fname='QDEM'//'_'//trim(rtm_tracers(2)), units='m3/s',  &
         avgflag='A', long_name='MOSART total demand: '//trim(rtm_tracers(2)), &
         ptr_rof=rtmCTL%qdem_nt2, default='active')

    call RtmHistAddfld (fname='Main_Channel_Water_Depth'//'_'//trim(rtm_tracers(1)), units='m',  &
           avgflag='A', long_name='MOSART main channel water depth:'//trim(rtm_tracers(1)), &
           ptr_rof=rtmCTL%yr_nt1, default='active')

    if (sediflag) then
       call RtmHistAddfld (fname='RIVER_DISCHARGE_OVER_LAND'//'_'//trim(rtm_tracers(3)), units='kg/s',  &
            avgflag='A', long_name='MOSART river basin flow: '//trim(rtm_tracers(3)), &
            ptr_rof=rtmCTL%runofflnd_nt3, default='active')

       call RtmHistAddfld (fname='RIVER_DISCHARGE_OVER_LAND'//'_'//trim(rtm_tracers(4)), units='kg/s',  &
            avgflag='A', long_name='MOSART river basin flow: '//trim(rtm_tracers(4)), &
            ptr_rof=rtmCTL%runofflnd_nt4, default='active')

       call RtmHistAddfld (fname='RIVER_DISCHARGE_TO_OCEAN'//'_'//trim(rtm_tracers(3)), units='kg/s',  &
            avgflag='A', long_name='MOSART river discharge into ocean: '//trim(rtm_tracers(3)), &
            ptr_rof=rtmCTL%runoffocn_nt3, default='active')

       call RtmHistAddfld (fname='RIVER_DISCHARGE_TO_OCEAN'//'_'//trim(rtm_tracers(4)), units='kg/s',  &
            avgflag='A', long_name='MOSART river discharge into ocean: '//trim(rtm_tracers(4)), &
            ptr_rof=rtmCTL%runoffocn_nt4, default='active')

       call RtmHistAddfld (fname='STORAGE'//'_'//trim(rtm_tracers(3)), units='kg',  &
            avgflag='A', long_name='MOSART storage: '//trim(rtm_tracers(3)), &
            ptr_rof=rtmCTL%volr_nt3, default='active')

       call RtmHistAddfld (fname='STORAGE'//'_'//trim(rtm_tracers(4)), units='kg',  &
            avgflag='A', long_name='MOSART storage: '//trim(rtm_tracers(4)), &
            ptr_rof=rtmCTL%volr_nt4, default='active')

       call RtmHistAddfld (fname='QSUR'//'_'//trim(rtm_tracers(3)), units='kg/s',  &
            avgflag='A', long_name='MOSART sediment yield from hillslope: '//trim(rtm_tracers(3)), &
            ptr_rof=rtmCTL%qsur_nt3, default='active')
    end if

    if (wrmflag) then

      call RtmHistAddfld (fname='WRM_IRR_SUPPLY', units='m3/s',  &
         avgflag='A', long_name='WRM supply provided ', &
         ptr_rof=StorWater%Supply, default='active')                                                                                                                              

      call RtmHistAddfld (fname='WRM_IRR_DEMAND', units='m3/s',  &
         avgflag='A', long_name='WRM demand requested ', &
         ptr_rof=StorWater%demand0, default='active')

      call RtmHistAddfld (fname='WRM_IRR_DEFICIT', units='m3/s',  &
         avgflag='A', long_name='WRM deficit ', &
         ptr_rof=StorWater%deficit, default='active')

      call RtmHistAddfld (fname='WRM_STORAGE', units='m3',  &
         avgflag='A', long_name='WRM storage ', &
         ptr_rof=StorWater%storageG, default='active')
    endif

    if (inundflag) then
      call RtmHistAddfld (fname='FLOODPLAIN_VOLUME', units='m3',  &
         avgflag='A', long_name='MOSART floodplain water volume', &
         ptr_rof=rtmCTL%inundwf, default='active')
      call RtmHistAddfld (fname='FLOODPLAIN_DEPTH', units='m',  &
         avgflag='A', long_name='MOSART floodplain water depth', &
         ptr_rof=rtmCTL%inundhf, default='active')
      call RtmHistAddfld (fname='FLOODPLAIN_FRACTION', units='1',  &
         avgflag='A', long_name='MOSART floodplain water area fraction', &
         ptr_rof=rtmCTL%inundff, default='active')
      call RtmHistAddfld (fname='FLOODED_FRACTION', units='1',  &
         avgflag='A', long_name='MOSART flooded water area fraction', &
         ptr_rof=rtmCTL%inundffunit, default='active')
    endif

    if(heatflag) then
      call RtmHistAddfld (fname='TEMP_QSUR', units='Kelvin',  &
           avgflag='A', long_name='Temperature of surface runoff', &
           ptr_rof=rtmCTL%templand_Tqsur_nt1)
    
      call RtmHistAddfld (fname='TEMP_QSUB', units='Kelvin',  &
           avgflag='A', long_name='Temperature of subsurface runoff', &
           ptr_rof=rtmCTL%templand_Tqsub_nt1)
    
      call RtmHistAddfld (fname='TEMP_TRIB', units='Kelvin',  &
           avgflag='A', long_name='Water temperature of tributary channels', &
           ptr_rof=rtmCTL%templand_Ttrib_nt1)
    
      call RtmHistAddfld (fname='TEMP_CHANR', units='Kelvin',  &
           avgflag='A', long_name='Water temperature of main channels', &
           ptr_rof=rtmCTL%templand_Tchanr_nt1)         
    end if     

    if ( river_bgc ) then
        
		call RtmHistAddfld (fname='RIVER_DISCHARGE_OVER_LAND'//'_'//trim(rtm_tracers(5)), units='kgC/s',  &
             avgflag='A', long_name='MOSART river basin flow: '//trim(rtm_tracers(5)), &
             ptr_rof=rtmCTL%runofflnd_nt5, default='active')
    
        call RtmHistAddfld (fname='RIVER_DISCHARGE_TO_OCEAN'//'_'//trim(rtm_tracers(5)), units='kgC/s',  &
             avgflag='A', long_name='MOSART river discharge into ocean: '//trim(rtm_tracers(5)), &
             ptr_rof=rtmCTL%runoffocn_nt5, default='active')
    
        call RtmHistAddfld (fname='TOTAL_DISCHARGE_TO_OCEAN'//'_'//trim(rtm_tracers(5)), units='kgC/s', &
             avgflag='A', long_name='MOSART total discharge into ocean: '//trim(rtm_tracers(5)), &
             ptr_rof=rtmCTL%runofftot_nt5, default='active')
    
        call RtmHistAddfld (fname='DIRECT_DISCHARGE_TO_OCEAN'//'_'//trim(rtm_tracers(5)), units='kgC/s', &
             avgflag='A', long_name='MOSART direct discharge into ocean: '//trim(rtm_tracers(5)), &
             ptr_rof=rtmCTL%runoffdir_nt5, default='active')
    
        call RtmHistAddfld (fname='STORAGE'//'_'//trim(rtm_tracers(5)), units='kgC',  &
             avgflag='A', long_name='MOSART storage: '//trim(rtm_tracers(5)), &
             ptr_rof=rtmCTL%volr_nt5, default='active')
    
        call RtmHistAddfld (fname='DISCHARGE_FROM_TRIBUTARY'//'_'//trim(rtm_tracers(5)), units='kgC/s',  &
             avgflag='A', long_name='MOSART river flow in tributaries: '//trim(rtm_tracers(5)), &
             ptr_rof=rtmCTL%QTrib_nt5, default='active')

        call RtmHistAddfld (fname='DVOLRDT_LND'//'_'//trim(rtm_tracers(5)), units='kgC/s',  &
             avgflag='A', long_name='MOSART land change in storage: '//trim(rtm_tracers(5)), &
             ptr_rof=rtmCTL%dvolrdtlnd_nt5, default='active')
    
        call RtmHistAddfld (fname='DVOLRDT_OCN'//'_'//trim(rtm_tracers(5)), units='kgC/s',  &
             avgflag='A', long_name='MOSART ocean change of storage: '//trim(rtm_tracers(5)), &
             ptr_rof=rtmCTL%dvolrdtocn_nt5, default='active')

        call RtmHistAddfld (fname='QSUR'//'_'//trim(rtm_tracers(5)), units='kg/s',  &
             avgflag='A', long_name='MOSART input surface runoff: '//trim(rtm_tracers(5)), &
             ptr_rof=rtmCTL%qsur_nt5, default='active')
    
        call RtmHistAddfld (fname='QSUB'//'_'//trim(rtm_tracers(5)), units='kg/s',  &
             avgflag='A', long_name='MOSART input subsurface runoff: '//trim(rtm_tracers(5)), &
             ptr_rof=rtmCTL%qsub_nt5, default='active')
			 
        call RtmHistAddfld (fname='RIVER_DISCHARGE_OVER_LAND'//'_'//trim(rtm_tracers(6)), units='kgC/s',  &
             avgflag='A', long_name='MOSART river basin flow: '//trim(rtm_tracers(6)), &
             ptr_rof=rtmCTL%runofflnd_nt6, default='active')
    
        call RtmHistAddfld (fname='RIVER_DISCHARGE_TO_OCEAN'//'_'//trim(rtm_tracers(6)), units='kgC/s',  &
             avgflag='A', long_name='MOSART river discharge into ocean: '//trim(rtm_tracers(6)), &
             ptr_rof=rtmCTL%runoffocn_nt6, default='active')
    
        call RtmHistAddfld (fname='TOTAL_DISCHARGE_TO_OCEAN'//'_'//trim(rtm_tracers(6)), units='kgC/s', &
             avgflag='A', long_name='MOSART total discharge into ocean: '//trim(rtm_tracers(6)), &
             ptr_rof=rtmCTL%runofftot_nt6, default='active')
    
        call RtmHistAddfld (fname='DIRECT_DISCHARGE_TO_OCEAN'//'_'//trim(rtm_tracers(6)), units='kgC/s', &
             avgflag='A', long_name='MOSART direct discharge into ocean: '//trim(rtm_tracers(6)), &
             ptr_rof=rtmCTL%runoffdir_nt6, default='active')
    
        call RtmHistAddfld (fname='STORAGE'//'_'//trim(rtm_tracers(6)), units='kgC',  &
             avgflag='A', long_name='MOSART storage: '//trim(rtm_tracers(6)), &
             ptr_rof=rtmCTL%volr_nt6, default='active')
    
        call RtmHistAddfld (fname='DISCHARGE_FROM_TRIBUTARY'//'_'//trim(rtm_tracers(6)), units='kgC/s',  &
             avgflag='A', long_name='MOSART river flow in tributaries: '//trim(rtm_tracers(6)), &
             ptr_rof=rtmCTL%QTrib_nt6, default='active') 

        call RtmHistAddfld (fname='DVOLRDT_LND'//'_'//trim(rtm_tracers(6)), units='kgC/s',  &
             avgflag='A', long_name='MOSART land change in storage: '//trim(rtm_tracers(6)), &
             ptr_rof=rtmCTL%dvolrdtlnd_nt6, default='active')
    
        call RtmHistAddfld (fname='DVOLRDT_OCN'//'_'//trim(rtm_tracers(6)), units='kgC/s',  &
             avgflag='A', long_name='MOSART ocean change of storage: '//trim(rtm_tracers(6)), &
             ptr_rof=rtmCTL%dvolrdtocn_nt6, default='active')
			 
			 
        call RtmHistAddfld (fname='RIVER_DISCHARGE_OVER_LAND'//'_'//trim(rtm_tracers(7)), units='kgC/s',  &
             avgflag='A', long_name='MOSART river basin flow: '//trim(rtm_tracers(7)), &
             ptr_rof=rtmCTL%runofflnd_nt7, default='active')
    
        call RtmHistAddfld (fname='RIVER_DISCHARGE_TO_OCEAN'//'_'//trim(rtm_tracers(7)), units='kgC/s',  &
             avgflag='A', long_name='MOSART river discharge into ocean: '//trim(rtm_tracers(7)), &
             ptr_rof=rtmCTL%runoffocn_nt7, default='active')
    
        call RtmHistAddfld (fname='TOTAL_DISCHARGE_TO_OCEAN'//'_'//trim(rtm_tracers(7)), units='kgC/s', &
             avgflag='A', long_name='MOSART total discharge into ocean: '//trim(rtm_tracers(7)), &
             ptr_rof=rtmCTL%runofftot_nt7, default='active')
    
        call RtmHistAddfld (fname='DIRECT_DISCHARGE_TO_OCEAN'//'_'//trim(rtm_tracers(7)), units='kgC/s', &
             avgflag='A', long_name='MOSART direct discharge into ocean: '//trim(rtm_tracers(7)), &
             ptr_rof=rtmCTL%runoffdir_nt7, default='active')
    
        call RtmHistAddfld (fname='STORAGE'//'_'//trim(rtm_tracers(7)), units='kgC',  &
             avgflag='A', long_name='MOSART storage: '//trim(rtm_tracers(7)), &
             ptr_rof=rtmCTL%volr_nt7, default='active')
    
        call RtmHistAddfld (fname='DISCHARGE_FROM_TRIBUTARY'//'_'//trim(rtm_tracers(7)), units='kgC/s',  &
             avgflag='A', long_name='MOSART river flow in tributaries: '//trim(rtm_tracers(7)), &
             ptr_rof=rtmCTL%QTrib_nt7, default='active')

        call RtmHistAddfld (fname='DVOLRDT_LND'//'_'//trim(rtm_tracers(7)), units='kgC/s',  &
             avgflag='A', long_name='MOSART land change in storage: '//trim(rtm_tracers(7)), &
             ptr_rof=rtmCTL%dvolrdtlnd_nt7, default='active')
    
        call RtmHistAddfld (fname='DVOLRDT_OCN'//'_'//trim(rtm_tracers(7)), units='kgC/s',  &
             avgflag='A', long_name='MOSART ocean change of storage: '//trim(rtm_tracers(7)), &
             ptr_rof=rtmCTL%dvolrdtocn_nt7, default='active')

        call RtmHistAddfld (fname='QSUR'//'_'//trim(rtm_tracers(7)), units='kg/s',  &
             avgflag='A', long_name='MOSART input surface runoff: '//trim(rtm_tracers(7)), &
             ptr_rof=rtmCTL%qsur_nt7, default='active')
    
        call RtmHistAddfld (fname='QSUB'//'_'//trim(rtm_tracers(7)), units='kg/s',  &
             avgflag='A', long_name='MOSART input subsurface runoff: '//trim(rtm_tracers(7)), &
             ptr_rof=rtmCTL%qsub_nt7, default='active')
			 
        call RtmHistAddfld (fname='RIVER_DISCHARGE_OVER_LAND'//'_'//trim(rtm_tracers(8)), units='kgC/s',  &
             avgflag='A', long_name='MOSART river basin flow: '//trim(rtm_tracers(8)), &
             ptr_rof=rtmCTL%runofflnd_nt8, default='active')
    
        call RtmHistAddfld (fname='RIVER_DISCHARGE_TO_OCEAN'//'_'//trim(rtm_tracers(8)), units='kgC/s',  &
             avgflag='A', long_name='MOSART river discharge into ocean: '//trim(rtm_tracers(8)), &
             ptr_rof=rtmCTL%runoffocn_nt8, default='active')
    
        call RtmHistAddfld (fname='TOTAL_DISCHARGE_TO_OCEAN'//'_'//trim(rtm_tracers(8)), units='kgC/s', &
             avgflag='A', long_name='MOSART total discharge into ocean: '//trim(rtm_tracers(8)), &
             ptr_rof=rtmCTL%runofftot_nt8, default='active')
    
        call RtmHistAddfld (fname='DIRECT_DISCHARGE_TO_OCEAN'//'_'//trim(rtm_tracers(8)), units='kgC/s', &
             avgflag='A', long_name='MOSART direct discharge into ocean: '//trim(rtm_tracers(8)), &
             ptr_rof=rtmCTL%runoffdir_nt8, default='active')
    
        call RtmHistAddfld (fname='STORAGE'//'_'//trim(rtm_tracers(8)), units='kgC',  &
             avgflag='A', long_name='MOSART storage: '//trim(rtm_tracers(8)), &
             ptr_rof=rtmCTL%volr_nt8, default='active')
    
        call RtmHistAddfld (fname='DISCHARGE_FROM_TRIBUTARY'//'_'//trim(rtm_tracers(8)), units='kgC/s',  &
             avgflag='A', long_name='MOSART river flow in tributaries: '//trim(rtm_tracers(8)), &
             ptr_rof=rtmCTL%QTrib_nt8, default='active')

        call RtmHistAddfld (fname='DVOLRDT_LND'//'_'//trim(rtm_tracers(8)), units='kgC/s',  &
             avgflag='A', long_name='MOSART land change in storage: '//trim(rtm_tracers(8)), &
             ptr_rof=rtmCTL%dvolrdtlnd_nt8, default='active')
    
        call RtmHistAddfld (fname='DVOLRDT_OCN'//'_'//trim(rtm_tracers(8)), units='kgC/s',  &
             avgflag='A', long_name='MOSART ocean change of storage: '//trim(rtm_tracers(8)), &
             ptr_rof=rtmCTL%dvolrdtocn_nt8, default='active')
    end if	

    if (wrmflag .and. heatflag .and. rstraflag) then
      call RtmHistAddfld (fname='RSRV_SURF', units='Kelvin',  &
           avgflag='A', long_name='Reservoir surface temperature', &
           ptr_rof=WRMUnit%resrv_surf)
    endif

    if (use_ocn_rof_two_way) then
      call RtmHistAddfld (fname='SSH', units='m',  &
           avgflag='A', long_name='MOSART sea surface height ', &
           ptr_rof=rtmCTL%ssh, default='active')
    endif

    ! Print masterlist of history fields

    call RtmHistPrintflds()

  end subroutine RtmHistFldsInit

!-----------------------------------------------------------------------

  subroutine RtmHistFldsSet()

    !-----------------------------------------------------------------------
    ! !DESCRIPTION:
    ! Set mosart history fields as 1d poitner arrays
    !
    implicit none
    integer :: idam, ig
    !-----------------------------------------------------------------------

    ! Currently only have two tracers

    rtmCTL%rmask(:)          = rtmCTL%mask(:)
    rtmCTL%rgindex(:)        = rtmCTL%gindex(:)
    rtmCTL%rdsig(:)          = rtmCTL%dsig(:)
    rtmCTL%routletg(:)       = rtmCTL%outletg(:)

    rtmCTL%runofflnd_nt1(:)  = rtmCTL%runofflnd(:,1)
    rtmCTL%runofflnd_nt2(:)  = rtmCTL%runofflnd(:,2)

    rtmCTL%runoffocn_nt1(:)  = rtmCTL%runoffocn(:,1)
    rtmCTL%runoffocn_nt2(:)  = rtmCTL%runoffocn(:,2)

    rtmCTL%runofftot_nt1(:)  = rtmCTL%runofftot(:,1)
    rtmCTL%runofftot_nt2(:)  = rtmCTL%runofftot(:,2)

    rtmCTL%runoffdir_nt1(:)  = rtmCTL%direct(:,1)
    rtmCTL%runoffdir_nt2(:)  = rtmCTL%direct(:,2)

    rtmCTL%dvolrdtlnd_nt1(:) = rtmCTL%dvolrdtlnd(:,1)
    rtmCTL%dvolrdtlnd_nt2(:) = rtmCTL%dvolrdtlnd(:,2)

    rtmCTL%dvolrdtocn_nt1(:) = rtmCTL%dvolrdtocn(:,1)
    rtmCTL%dvolrdtocn_nt2(:) = rtmCTL%dvolrdtocn(:,2)

    rtmCTL%wr_nt1(:)         = rtmCTL%wr(:,1)

    rtmCTL%volr_nt1(:)       = rtmCTL%volr(:,1)
    rtmCTL%volr_nt2(:)       = rtmCTL%volr(:,2)

    rtmCTL%QTrib_nt1(:)      = rtmCTL%QTrib(:,1)
    rtmCTL%QTrib_nt2(:)      = rtmCTL%QTrib(:,2)

    rtmCTL%qsub_nt1(:)       = rtmCTL%qsub(:,1)
    rtmCTL%qsub_nt2(:)       = rtmCTL%qsub(:,2)

    rtmCTL%qsur_nt1(:)       = rtmCTL%qsur(:,1)
    rtmCTL%qsur_nt2(:)       = rtmCTL%qsur(:,2)

    rtmCTL%qgwl_nt1(:)       = rtmCTL%qgwl(:,1)
    rtmCTL%qgwl_nt2(:)       = rtmCTL%qgwl(:,2)

    rtmCTL%qdto_nt1(:)       = rtmCTL%qdto(:,1)
    rtmCTL%qdto_nt2(:)       = rtmCTL%qdto(:,2)

    rtmCTL%qdem_nt1(:)       = rtmCTL%qdem(:,1)
    rtmCTL%qdem_nt2(:)       = rtmCTL%qdem(:,2)
  
    rtmCTL%yr_nt1(:)         = rtmCTL%yr(:,1)  ! water depth

    if(sediflag) then
        rtmCTL%runofflnd_nt3(:)  = rtmCTL%runofflnd(:,3)
        rtmCTL%runofflnd_nt4(:)  = rtmCTL%runofflnd(:,4)
        rtmCTL%runoffocn_nt3(:)  = rtmCTL%runoffocn(:,3)
        rtmCTL%runoffocn_nt4(:)  = rtmCTL%runoffocn(:,4)
        rtmCTL%volr_nt3(:)       = rtmCTL%volr(:,3)
        rtmCTL%volr_nt4(:)       = rtmCTL%volr(:,4)
        rtmCTL%qsur_nt3(:)       = rtmCTL%qsur(:,3)
    end if

    if (wrmflag) then
       StorWater%storageG = 0._r8
       do idam = 1, ctlSubwWRM%localNumDam
          ig = WRMUnit%icell(idam)
          StorWater%storageG(ig) = StorWater%storage(idam)
       enddo
    endif

    if(heatflag) then
      rtmCTL%templand_Tqsur_nt1(:) = rtmCTL%templand_Tqsur(:)
      rtmCTL%templand_Tqsur_nt2(:) = rtmCTL%templand_Tqsur(:)
      rtmCTL%templand_Tqsub_nt1(:) = rtmCTL%templand_Tqsub(:)
      rtmCTL%templand_Tqsub_nt2(:) = rtmCTL%templand_Tqsub(:)
      rtmCTL%templand_Ttrib_nt1(:) = rtmCTL%templand_Ttrib(:)
      rtmCTL%templand_Ttrib_nt2(:) = rtmCTL%templand_Ttrib(:)
      rtmCTL%templand_Tchanr_nt1(:) = rtmCTL%templand_Tchanr(:)
      rtmCTL%templand_Tchanr_nt2(:) = rtmCTL%templand_Tchanr(:)
    end if

   if ( river_bgc ) then
		rtmCTL%runofflnd_nt5(:)  = rtmCTL%runofflnd(:,5)
        rtmCTL%runoffocn_nt5(:)  = rtmCTL%runoffocn(:,5)
        rtmCTL%runofftot_nt5(:)  = rtmCTL%runofftot(:,5)
        rtmCTL%runoffdir_nt5(:)  = rtmCTL%direct(:,5)
        rtmCTL%dvolrdtlnd_nt5(:) = rtmCTL%dvolrdtlnd(:,5)
        rtmCTL%dvolrdtocn_nt5(:) = rtmCTL%dvolrdtocn(:,5)
        rtmCTL%volr_nt5(:)       = rtmCTL%volr(:,5)
        rtmCTL%QTrib_nt5(:)      = rtmCTL%QTrib(:,5)
        rtmCTL%qsur_nt5(:)       = rtmCTL%qsur(:,5)
        rtmCTL%qsub_nt5(:)       = rtmCTL%qsub(:,5)
	
        rtmCTL%runofflnd_nt6(:)  = rtmCTL%runofflnd(:,6)
        rtmCTL%runoffocn_nt6(:)  = rtmCTL%runoffocn(:,6)
        rtmCTL%runofftot_nt6(:)  = rtmCTL%runofftot(:,6)
        rtmCTL%runoffdir_nt6(:)  = rtmCTL%direct(:,6)
        rtmCTL%dvolrdtlnd_nt6(:) = rtmCTL%dvolrdtlnd(:,6)
        rtmCTL%dvolrdtocn_nt6(:) = rtmCTL%dvolrdtocn(:,6)
        rtmCTL%volr_nt6(:)       = rtmCTL%volr(:,6)
        rtmCTL%QTrib_nt6(:)      = rtmCTL%QTrib(:,6)
		
		rtmCTL%runofflnd_nt7(:)  = rtmCTL%runofflnd(:,7)
        rtmCTL%runoffocn_nt7(:)  = rtmCTL%runoffocn(:,7)
        rtmCTL%runofftot_nt7(:)  = rtmCTL%runofftot(:,7)
        rtmCTL%runoffdir_nt7(:)  = rtmCTL%direct(:,7)
        rtmCTL%dvolrdtlnd_nt7(:) = rtmCTL%dvolrdtlnd(:,7)
        rtmCTL%dvolrdtocn_nt7(:) = rtmCTL%dvolrdtocn(:,7)
        rtmCTL%volr_nt7(:)       = rtmCTL%volr(:,7)
        rtmCTL%QTrib_nt7(:)      = rtmCTL%QTrib(:,7)
        rtmCTL%qsur_nt7(:)       = rtmCTL%qsur(:,7)
        rtmCTL%qsub_nt7(:)       = rtmCTL%qsub(:,7)
	
        rtmCTL%runofflnd_nt8(:)  = rtmCTL%runofflnd(:,8)
        rtmCTL%runoffocn_nt8(:)  = rtmCTL%runoffocn(:,8)
        rtmCTL%runofftot_nt8(:)  = rtmCTL%runofftot(:,8)
        rtmCTL%runoffdir_nt8(:)  = rtmCTL%direct(:,8)
        rtmCTL%dvolrdtlnd_nt8(:) = rtmCTL%dvolrdtlnd(:,8)
        rtmCTL%dvolrdtocn_nt8(:) = rtmCTL%dvolrdtocn(:,8)
        rtmCTL%volr_nt8(:)       = rtmCTL%volr(:,8)
        rtmCTL%QTrib_nt8(:)      = rtmCTL%QTrib(:,8)
		
	end if
     
  end subroutine RtmHistFldsSet


end module RtmHistFlds
