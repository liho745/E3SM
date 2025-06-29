!
MODULE MOSART_bgc_mod
! Description: core code of MOSART. Can be incoporated within any land model via a interface module
!  
! Developed by Hongyi Li, June/2017. 
! REVISION HISTORY:
! Md Monir Hossain, June/2021
! Hongyi Li, March/2024
!-----------------------------------------------------------------------

! !USES:
    use shr_kind_mod  , only : r8 => shr_kind_r8, SHR_KIND_CL
    use RunoffMod , only : Tctl, TUnit, TRunoff, THeat, Ttran, TPara
    use RunoffMod , only : rtmCTL
    use RtmVar    , only : heatflag
    use rof_cpl_indices, only : nt_rtm, rtm_tracers, nt_nliq, nt_nice, nt_nliq_DOC, nt_nice_DOC, nt_nliq_POC, nt_nice_POC
    implicit none
    real(r8), parameter :: TINYVALUE1 = 1.0e-14_r8  ! double precision variable has a significance of about 16 decimal digits

    public hillslopeSource
    public subnetworkSource
    public mainchannelSource    
    
! !PUBLIC MEMBER FUNCTIONS:
    contains
    
    subroutine hillslopeSource(iunit, nt,theDeltaT)
    ! !DESCRIPTION: calculate the source/sink term due to transformation between different elements.
        implicit none
        
        integer, intent(in) :: iunit, nt
        real(r8), intent(in) :: theDeltaT

    end subroutine hillslopeSource

    subroutine subnetworkSource(iunit, nt,theDeltaT)
    ! !DESCRIPTION: calculate the source/sink term due to transformation between different elements.
        implicit none
        
        integer, intent(in) :: iunit, nt
        real(r8), intent(in) :: theDeltaT
        real(r8) :: DOC2DECAY
        
        !Including POC and DOC transformation for subnetwork
        TRunoff%etsource(iunit,nt) = 0._r8
        
        DOC2DECAY = 0._r8
        
        if (.not. heatflag) then
           THeat%Tt(iunit) = cr_S_curve_bgc(iunit,THeat%forc_t(iunit))
        end if    

        !write(unit=7001,fmt="(1e14.6)")THeat%Tt(iunit)        
        
        if(nt == nt_nliq_POC) then
            Ttran%kt_convert(iunit,nt_nliq_POC,nt_nliq_DOC) = cr_kPOC2DOC(THeat%Tt(iunit)-273.15_r8)
            !write(unit=7002,fmt="(3e14.6)")Ttran%kt_convert(iunit,nt_nliq_POC,nt_nliq_DOC),TRunoff%wt(iunit,nt_nliq_POC),THeat%Tt(iunit)
            Ttran%et_convert(iunit,nt_nliq_POC,nt_nliq_DOC) = - cr_ePOC2DOC(TRunoff%wt(iunit,nt_nliq_POC),Ttran%kt_convert(iunit,nt_nliq_POC,nt_nliq_DOC))
            !write(unit=7003,fmt="(1e14.6)")Ttran%et_convert(iunit,nt_nliq_POC,nt_nliq_DOC)
            If((Ttran%et_convert(iunit,nt_nliq_POC,nt_nliq_DOC) + TRunoff%etsource(iunit,nt_nliq_POC)) < -TINYVALUE1 .and. &
                TRunoff%wt(iunit,nt_nliq_POC) + (TRunoff%etsource(iunit,nt_nliq_POC) + Ttran%et_convert(iunit,nt_nliq_POC,nt_nliq_DOC)) * theDeltaT < TINYVALUE1) then
                Ttran%et_convert(iunit,nt_nliq_POC,nt_nliq_DOC) = - TRunoff%wt(iunit,nt_nliq_POC) / theDeltaT - TRunoff%etsource(iunit,nt_nliq_POC)
            end if
            
            Ttran%kt_convert(iunit,nt_nliq_DOC,nt_nliq_POC) = 0._r8
            ! The decaying POC is not coverting to DOC. No need to tranfer it to DOC
            !Ttran%et_convert(iunit,nt_nliq_DOC,nt_nliq_POC) = - Ttran%et_convert(iunit,nt_nliq_POC,nt_nliq_DOC)
            
            ! in fact, for one element, there could be more than one source fluxes
            TRunoff%etsource(iunit,nt_nliq_POC) = TRunoff%etsource(iunit,nt_nliq_POC) + Ttran%et_convert(iunit,nt_nliq_POC,nt_nliq_DOC)
            !write(unit=7004,fmt="(1e14.6)")TRunoff%etsource(iunit,nt_nliq_POC)
            ! POC is not converting to DOC. No need to update DOC storagfe
            !TRunoff%etsource(iunit,nt_nliq_DOC) = TRunoff%etsource(iunit,nt_nliq_DOC) + Ttran%et_convert(iunit,nt_nliq_DOC,nt_nliq_POC)
        end if
        
        if(nt == nt_nliq_DOC) then
            DOC2DECAY = - cr_eDOC2DECAY(TRunoff%wt(iunit,nt_nliq_DOC),THeat%Tt(iunit)-273.15_r8)
            !write(unit=7005,fmt="(1e14.6)")DOC2DECAY
            
            If((TRunoff%etsource(iunit,nt_nliq_DOC) + DOC2DECAY) < -TINYVALUE1 .and. &
                TRunoff%wt(iunit,nt_nliq_DOC) + (TRunoff%etsource(iunit,nt_nliq_DOC) + DOC2DECAY) * theDeltaT < TINYVALUE1) then
                DOC2DECAY = - TRunoff%wt(iunit,nt_nliq_DOC) / theDeltaT - TRunoff%etsource(iunit,nt_nliq_DOC)
            end if
            
            TRunoff%etsource(iunit,nt_nliq_DOC) = TRunoff%etsource(iunit,nt_nliq_DOC) + DOC2DECAY
            !write(unit=7006,fmt="(1e14.6)")TRunoff%etsource(iunit,nt_nliq_DOC)
        end if
        
        
        TRunoff%dwt(iunit,nt) = TRunoff%etsource(iunit,nt)
        
        !if(iunit==2496) then
        !    TRunoff%etsource(iunit,nt) = 1.e-1
        !    TRunoff%dwt(iunit,nt) = TRunoff%etsource(iunit,nt)
        !else
        !    TRunoff%etsource(iunit,nt) = 0._r8
        !    TRunoff%dwt(iunit,nt) = TRunoff%etsource(iunit,nt)
        !end if        

    end subroutine subnetworkSource

    subroutine mainchannelSource(iunit, nt,theDeltaT)
    ! !DESCRIPTION: calculate the source/sink term due to transformation between different elements.
        implicit none
        
        integer, intent(in) :: iunit, nt
        real(r8), intent(in) :: theDeltaT
        real(r8) :: DOC2DECAY
        !real(r8)  :: tmp1
        
        TRunoff%ersource(iunit,nt) = 0._r8
        
        DOC2DECAY = 0._r8
        
        if (.not. heatflag) then
           THeat%Tr(iunit) = cr_S_curve_bgc(iunit,THeat%forc_t(iunit))
        end if    

        !write(unit=7008,fmt="(1e14.6)")THeat%Tr(iunit)        
        
        if(nt == nt_nliq_POC) then
            Ttran%kr_convert(iunit,nt_nliq_POC,nt_nliq_DOC) = cr_kPOC2DOC(THeat%Tr(iunit)-273.15_r8)
            !write(unit=7009,fmt="(3e14.6)")Ttran%kr_convert(iunit,nt_nliq_POC,nt_nliq_DOC), TRunoff%wr(iunit,nt_nliq_POC),THeat%Tr(iunit)
            Ttran%er_convert(iunit,nt_nliq_POC,nt_nliq_DOC) = - cr_ePOC2DOC(TRunoff%wr(iunit,nt_nliq_POC),Ttran%kr_convert(iunit,nt_nliq_POC,nt_nliq_DOC))
            !write(unit=7010,fmt="(1e14.6)")Ttran%er_convert(iunit,nt_nliq_POC,nt_nliq_DOC)
            If((Ttran%er_convert(iunit,nt_nliq_POC,nt_nliq_DOC) + TRunoff%ersource(iunit,nt_nliq_POC)) < -TINYVALUE1 .and. &
                TRunoff%wr(iunit,nt_nliq_POC) + (TRunoff%ersource(iunit,nt_nliq_POC) + Ttran%er_convert(iunit,nt_nliq_POC,nt_nliq_DOC)) * theDeltaT < TINYVALUE1) then
                Ttran%er_convert(iunit,nt_nliq_POC,nt_nliq_DOC) = - TRunoff%wr(iunit,nt_nliq_POC) / theDeltaT - TRunoff%ersource(iunit,nt_nliq_POC)
            end if
            
            Ttran%kr_convert(iunit,nt_nliq_DOC,nt_nliq_POC) = 0._r8
            ! The decaying POC is not coverting to DOC. No need to tranfer it to DOC
            Ttran%er_convert(iunit,nt_nliq_DOC,nt_nliq_POC) = - Ttran%er_convert(iunit,nt_nliq_POC,nt_nliq_DOC)
            
            ! in fact, for one element, there could be more than one source fluxes
            TRunoff%ersource(iunit,nt_nliq_POC) = TRunoff%ersource(iunit,nt_nliq_POC) + Ttran%er_convert(iunit,nt_nliq_POC,nt_nliq_DOC)
            !write(unit=7011,fmt="(1e14.6)")TRunoff%ersource(iunit,nt_nliq_POC)
            
            ! POC is not converting to DOC. No need to update DOC storagfe
            !TRunoff%ersource(iunit,nt_nliq_DOC) = TRunoff%ersource(iunit,nt_nliq_DOC) + Ttran%er_convert(iunit,nt_nliq_DOC,nt_nliq_POC)
        end if
        
        if(nt == nt_nliq_DOC) then
            DOC2DECAY = - cr_eDOC2DECAY(TRunoff%wr(iunit,nt_nliq_DOC),THeat%Tr(iunit)-273.15_r8)
            !write(unit=7012,fmt="(1e14.6)")DOC2DECAY
            
            If((TRunoff%ersource(iunit,nt_nliq_DOC) + DOC2DECAY) < -TINYVALUE1 .and. &
                TRunoff%wr(iunit,nt_nliq_DOC) + (TRunoff%ersource(iunit,nt_nliq_DOC) + DOC2DECAY) * theDeltaT < TINYVALUE1) then
                DOC2DECAY = - TRunoff%wr(iunit,nt_nliq_DOC) / theDeltaT - TRunoff%ersource(iunit,nt_nliq_DOC)
            end if
            
            TRunoff%ersource(iunit,nt_nliq_DOC) = TRunoff%ersource(iunit,nt_nliq_DOC) + DOC2DECAY
            !write(unit=7013,fmt="(1e14.6)")TRunoff%ersource(iunit,nt_nliq_DOC)
        end if
        
        
        TRunoff%dwr(iunit,nt) = TRunoff%ersource(iunit,nt)
        !write(unit=7014,fmt="(1e14.6)")TRunoff%dwr(iunit,nt)
        
    end subroutine mainchannelSource

    
    
    function cr_S_curve_bgc(iunit_,Ta_) result(Tw_)
        ! closure relationship to calculate water temperature based on the S-curve 
        implicit none
        integer, intent(in)  :: iunit_ ! 
        real(r8), intent(in) :: Ta_    ! temperature of air (Kelvin)
        real(r8) :: Tw_                ! temperature of water (Kelvin)

        real(r8) :: Ttmp1, Ttmp2  !
        real(r8) :: alpha, mu, gamma, beta ! parameters for the S-curve
        
        alpha = 27.19_r8
        beta  = 13.63_r8
        gamma = 0.1576_r8
        mu    = 0.5278_r8
        
        Ttmp1 = alpha - mu
        Ttmp2 = 1._r8 + exp(gamma*(beta - (Ta_-273.15_r8)))
        Tw_ = mu + Ttmp1/Ttmp2 + 273.15_r8
        if(Tw_ < 273.15_r8) then
               Tw_ = 273.15_r8
        end if
    
        return
    end function cr_S_curve_bgc
    
    
    function cr_kPOC2DOC(T_) result(kPOC2DOC_)
        ! closure relationship for for reaction/transformation rates
        implicit none
        real(r8), intent(in) :: T_  ! water temperature, [celcius degree]
        real(r8) :: kPOC2DOC_       ! [/s]
        
        real(r8) :: k_0
        real(r8) :: Q10
        real(r8) :: Tref
        real(r8) :: f_T
        
        k_0  = 0.005_r8/86400._r8    !KPOC=0.005 day-1  Tian et al. (2015)
        Q10  = 2._r8
        Tref = 20._r8
        f_T  = 1._r8
        
        kPOC2DOC_ = k_0 * (Q10 **((T_-Tref)/10._r8)) *f_T !  
        
        return
    end function cr_kPOC2DOC
    
    function cr_ePOC2DOC(s_POC_, k_) result(ePOC2DOC_)
        ! closure relationship for reaction/transformation fluxes
        implicit none
        real(r8), intent(in) :: s_POC_  ! storage of POC, [kg]
        real(r8), intent(in) :: k_      ! transformation rate, [s-1]
        real(r8) :: ePOC2DOC_           ! [kg/s]
        
        ePOC2DOC_ = k_ * s_POC_    
        
        return
    end function cr_ePOC2DOC
    
    function cr_eDOC2DECAY(s_DOC_, T_) result(eDOC2DECAY_)
        ! closure relationship for reaction/transformation fluxes
        implicit none
        real(r8), intent(in) :: s_DOC_  ! storage of DOC, [kg]
        real(r8), intent(in) :: T_      ! water temperature [celcius degree] 
        real(r8) :: eDOC2DECAY_           ! [kg/s]
        
        real(r8) :: k_0
        real(r8) :: Q10
        real(r8) :: Tref
        real(r8) :: f_T
        real(r8) :: k_
        
        k_0  = 0.01_r8/86400._r8     !KDOC=0.01 day-1  Tian et al. (2015)
        Q10  = 2._r8
        Tref = 20._r8
        f_T  = 1._r8
        
        k_ = k_0 * (Q10 **((T_-Tref)/10._r8)) *f_T ! 
        
        eDOC2DECAY_ = k_ * s_DOC_   
        
        return
    end function cr_eDOC2DECAY
    
    
end MODULE MOSART_bgc_mod