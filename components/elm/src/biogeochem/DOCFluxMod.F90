module DOCFluxMod 

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Calculate DOC fluxes from soil C pool 
  ! Developed by Md Monir Hossain Sep, 2021
  !
  use shr_kind_mod      , only : r8 => shr_kind_r8
  use shr_log_mod       , only : errMsg => shr_log_errMsg
  use decompMod         , only : bounds_type
  use elm_varcon        , only : dzsoi_decomp, denh2o, denice
  use elm_varpar        , only : ndecomp_pools, nlevdecomp, nlevsoi, nlevgrnd
  use CNCarbonFluxType  , only : carbonflux_type
  use CNCarbonStateType , only : carbonstate_type
  use CNDecompCascadeConType , only : decomp_cascade_con
  use CNNitrogenFluxType  , only : nitrogenflux_type
  use CNNitrogenStateType , only : nitrogenstate_type
  use PhosphorusFluxType  , only : phosphorusflux_type
  use PhosphorusStateType , only : phosphorusstate_type
  use SedFluxType       , only : sedflux_type
  use SoilStateType     , only : soilstate_type
  use SoilHydrologyType,  only : soilhydrology_type
  use ColumnDataType    , only : col_cs, col_ns, col_ps, col_wf, col_es
  use ColumnDataType    , only : col_cf, col_nf, col_pf , col_ws
  use elm_varctl        , only : use_century_decomp, use_fates
  use ColumnType        , only : col_pp
  !
  ! !PUBLIC TYPES:
  implicit none
  save
  private
  !
  
  public :: DOCFluxes           ! Calculate DOC fluxes from soil C pool 
  ! !MODULE CONSTANTS
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine DOCFluxes (bounds, num_soilc, filter_soilc, &
    soilhydrology_vars, soilstate_vars, sedflux_vars, dt)
    !
    ! !DESCRIPTION:
    ! Calculate DOC fluxes from soil C pool
    !
    ! !USES:
    use elm_varctl      , only : iulog
    use spmdMod         , only : masterproc
    !
    ! !ARGUMENTS:
    type(bounds_type)        , intent(in)    :: bounds
    integer                  , intent(in)    :: num_soilc       ! number of column soil points in column filter
    integer                  , intent(in)    :: filter_soilc(:) ! filter for soil columns
    type(soilhydrology_type) , intent(in)    :: soilhydrology_vars
    type(soilstate_type)     , intent(in)    :: soilstate_vars
    type(sedflux_type)       , intent(in)    :: sedflux_vars 
    real(r8)                 , intent(in)    :: dt          ! radiation time step (seconds)
    !
    ! !LOCAL VARIABLES:
    integer  :: c, fc, l, j, izwt                       ! indices
    ! for DOC leaches
    
    real(r8) :: Pr                                      ! SOC->DOC transformation coefficient (m^3 soil / m^3 water)
    real(r8) :: avg_DOC_conc                            ! average dissolved organic C concentration in top soil layers (g C/kg water)
    real(r8) :: doc_qsur                                ! DOC from surface runoff (g C/m2/s)
    real(r8) :: doc_qsub                                ! DOC from subsurface runoff (g C/m2/s)
    real(r8), parameter :: depth_runoff_Closs = 0.15_r8 !0.05_r8 ! (m) top soil depth over which most DOC leaching (with runoff) occurs
    real(r8) :: cltot                                   ! dummy
    real(r8) :: DOC_conc                                ! dissolved organic C concentration (g C/kg water)
    real(r8) :: sum_doc                                 ! sum of doc storage in the soil layers below groundwater table [g C/m2]
    real(r8) :: sum_water                               ! sum of water storage below groundwater table [kg water/m2]
    real(r8) :: weight_dz                               ! ratio of saturated depth within a soil layer when groundwater table is within this layer [-]
    real(r8) :: sum_dz, sum_tmp                         ! sum of soil layer thickness below groundwater table [m]
    character(len=32) :: subname = 'DOCFluxes'          ! subroutine name
    !-----------------------------------------------------------------------

    associate(                                                        &
         decomp_cpools_vr =>    col_cs%decomp_cpools_vr       , & ! Input: [real(r8) (:,:,:) ] SOM C pools [gC/m3]
         bd               =>    soilstate_vars%bd_col         , & ! Input: [real(r8) (:,:) ] soil bulk density (kg/m3)
         Pr_array         =>    soilstate_vars%Pr_col         , & ! Input: [real(r8) (:) ] SOC->DOC transformation coefficient (m^3 soil / m^3 water)
         qflx_surf        =>    col_wf%qflx_surf              , & ! Input: [real(r8) (:) ] surface runoff (mm H2O/s) (1000*m^3 water / m^2 area /s)
         t_soisno         =>    col_es%t_soisno               , & ! Input: [real(r8) (:,:) ]  soil temperature (Kelvin) 
         qflx_drain       =>    col_wf%qflx_drain             , & ! Input: [real(r8) (:) ] subsurface runoff (mm H2O/s)
         !qflx_drain_perched   =>    col_wf%qflx_drain_perched , & ! Input: [real(r8) (:) ] sub-surface runoff from perched wt (mm H2O /s)
         h2osoi_liq       =>    col_ws%h2osoi_liq             , & ! Input: [real(r8) (:,:) ]  liquid soil water (kg/m2) (new) (-nlevsno+1:nlevgrnd)
         zwt_col          =>    soilhydrology_vars%zwt_col    , & ! Input: [real(r8) (:) ]  groundwater table depth (m)
         f_doc_soil_qsur  =>    col_cf%f_doc_soil_qsur        , & ! Output
         f_doc_soil_qsub  =>    col_cf%f_doc_soil_qsub          & ! Output
         )

         ! soil col filters
         do fc = 1, num_soilc
            c = filter_soilc(fc)
            f_doc_soil_qsur(c)     = 0._r8
            f_doc_soil_qsub(c)     = 0._r8
                   
    !if (use_century_decomp) then  ! for centrury carbon decompostion model, DOC is coming from only one soil pool
           doc_qsur = 0._r8
           doc_qsub = 0._r8
           cltot = 0._r8               
           if (.not.use_fates) then
              l=5         ! l=decomp_cpool. The DOC is coming from only one pool(soil1)due to fast decomposition(clm5_fig21.2)
           else
              l=4         ! l=decomp_cpool. The DOC is coming from only one pool(soil1)due to fast decomposition(clm5_fig21.2). Not considering CWD pool.
           end if
            
           Pr    = Pr_array(c)    ! using the same Pr value for all soil layers
           !write(unit=2001,fmt="(e14.6)") Pr
           
           ! DOC leaching with surface runoff from top soil layer(s) only
           sum_doc = 0._r8
           sum_water = 0._r8
           !sum_dz = 0._r8
           
           do j=1, nlevdecomp
               if(col_pp%zi(c,j) <= depth_runoff_Closs) then
                   !cltot = sum(decomp_cpools_vr(c,j,:)) 
                   !cltot = sum(decomp_cpools_vr(c,j,:)) 
                   cltot = 0._r8
                   do l = 1, ndecomp_pools
                      if ( decomp_cascade_con%is_soil(l)) then
                         cltot = cltot + decomp_cpools_vr(c,j,l)
                      end if
                   end do
                   sum_doc = sum_doc + cltot*Pr*col_pp%dz(c,j) ! gC * m/(m^3 water)
               elseif(col_pp%zi(c,j-1) < depth_runoff_Closs) then
                   cltot = 0._r8
                   do l = 1, ndecomp_pools
                      if ( decomp_cascade_con%is_soil(l)) then
                         cltot = cltot + decomp_cpools_vr(c,j,l)
                      end if
                   end do
                   weight_dz = (depth_runoff_Closs - col_pp%zi(c,j-1)) / col_pp%dz(c,j)
                   sum_doc = sum_doc + cltot*Pr*col_pp%dz(c,j)*weight_dz ! gC * m/(m^3 water)
               end if
           end do
           DOC_conc = sum_doc / depth_runoff_Closs  ! gC/(m^3 water)
           doc_qsur = DOC_conc * qflx_surf(c) * 0.001_r8              ! gC/m2/s 
           !doc_qsur = min(doc_qsur, cltot * Pr * col_pp%dz(c,j)/dt) ! ensure that leaching within one time step isn't larger than soil DOC pool
           doc_qsur = max(doc_qsur, 0._r8) !limit the leaching flux to a positive value
                      
           ! DOC leaching with subsurface runoff
           cltot = 0._r8
           ! determine which layer the groundwater table is located in, then count the layers below
           if(zwt_col(c) >= col_pp%zi(c,nlevdecomp)) then ! water table below the bedrock, no subsurface leaching
               DOC_conc = 0._r8
               doc_qsub = 0._r8
           elseif(zwt_col(c) <= 0._r8) then ! water table above ground surface, all soil layers contributing
               sum_doc = 0._r8
               sum_dz = 0._r8
               do j=1, nlevdecomp
                   cltot = 0._r8
                   do l = 1, ndecomp_pools
                      if ( decomp_cascade_con%is_soil(l)) then
                         cltot = cltot + decomp_cpools_vr(c,j,l)
                      end if
                   end do
                   sum_doc = sum_doc + cltot*Pr*col_pp%dz(c,j) ! gC * m/(m^3 water)
                   sum_dz  = sum_dz + col_pp%dz(c,j)
               end do
               DOC_conc = sum_doc / sum_dz  !gC/(m^3 water)
               doc_qsub = DOC_conc * qflx_drain(c) * 0.001_r8 ! gC/m2/s 
                       
           else ! most common simuation, water table below surface but above bedrock
               sum_doc = 0._r8
               sum_dz = col_pp%zi(c,nlevdecomp) - zwt_col(c)
               sum_tmp = 0._r8
               izwt = 0
               do j=1, nlevdecomp
                   if(zwt_col(c) <=col_pp%zi(c,j)) then
                      izwt = j
                      exit
                   end if
               end do
               j=izwt
               cltot = 0._r8
               do l = 1, ndecomp_pools
                  if ( decomp_cascade_con%is_soil(l)) then
                     cltot = cltot + decomp_cpools_vr(c,j,l)
                  end if
               end do
               weight_dz = (col_pp%zi(c,j) - zwt_col(c))/col_pp%dz(c,j)
               sum_doc = sum_doc + cltot*Pr*col_pp%dz(c,j)*weight_dz ! ! gC * m/(m^3 water)
               sum_tmp  = sum_tmp + col_pp%dz(c,j)*weight_dz
               if(izwt < nlevdecomp) then
               do j=izwt+1, nlevdecomp
                   if(zwt_col(c) <=col_pp%zi(c,j)) then
                      cltot = 0._r8
                      do l = 1, ndecomp_pools
                         if ( decomp_cascade_con%is_soil(l)) then
                            cltot = cltot + decomp_cpools_vr(c,j,l)
                         end if
                      end do
                      weight_dz = (col_pp%zi(c,j) - zwt_col(c))/col_pp%dz(c,j)
                      sum_doc = sum_doc + cltot*Pr*col_pp%dz(c,j)*weight_dz ! ! gC * m/(m^3 water)
                      sum_tmp  = sum_tmp + col_pp%dz(c,j)*weight_dz
                      exit
                   else
                      cltot = 0._r8
                      do l = 1, ndecomp_pools
                         if ( decomp_cascade_con%is_soil(l)) then
                            cltot = cltot + decomp_cpools_vr(c,j,l)
                         end if
                      end do
                      sum_doc = sum_doc + cltot*Pr*col_pp%dz(c,j) ! ! gC * m/(m^3 water)
                      sum_tmp  = sum_tmp + col_pp%dz(c,j)
                   end if
                   !write(unit=1003,fmt="(i10, 3e14.6)")  j, sum_dz, sum_tmp, col_pp%dz(c,j)
               end do
               end if
               DOC_conc = sum_doc / sum_dz
               doc_qsub = DOC_conc * qflx_drain(c) * 0.001_r8
           end if
           doc_qsub = min(doc_qsub, sum_doc/dt) ! ensure that leaching within one time step isn't larger than soil DOC pool
           doc_qsub = max(doc_qsub, 0._r8) !limit the leaching flux to a positive value
                  
           !f_doc_soil(c) = doc_qsur + doc_qsub
           f_doc_soil_qsur(c) = doc_qsur
           f_doc_soil_qsub(c) = doc_qsub

         end do ! soil col filters
    
    end associate

  end subroutine DOCFluxes

end module DOCFluxMod 
