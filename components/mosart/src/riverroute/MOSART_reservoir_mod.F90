!
MODULE MOSART_reservoir_mod
! Description: core code of MOSART-reservoir. Can be incoporated within any river model via a interface module
! 
! Developed by Hongyi Li, 11/2016.
! REVISION HISTORY:
!-----------------------------------------------------------------------

! !USES:
    use shr_kind_mod  , only : r8 => shr_kind_r8, SHR_KIND_CL
    use shr_const_mod , only : denh2o => SHR_CONST_RHOFW, denice => SHR_CONST_RHOICE, grav => SHR_CONST_G, SHR_CONST_REARTH, SHR_CONST_PI
    use shr_sys_mod   , only : shr_sys_abort
    use RtmVar        , only : iulog
    !use clm_varcon , only : denh2o, grav !!density of liquid water [kg/m3], gravity constant [m/s2]
    use RunoffMod, only : Tctl, TUnit, TRunoff, TPara
    use RunoffMod, only : rtmCTL
    use rof_cpl_indices, only : nt_rtm, rtm_tracers, nt_nliq, nt_nice, nt_nmud, nt_nsan
    use MOSART_RES_type, only : Tres_para, Tres
    use WRM_type_mod   , only : ctlSubwWRM, WRMUnit, StorWater
    implicit none
    real(r8), parameter :: TINYVALUE_r = 1.0e-14_r8  ! double precision variable has a significance of about 16 decimal digits

! !PUBLIC MEMBER FUNCTIONS:
    contains


    subroutine res_trapping(iunit, nt)
    ! !DESCRIPTION: trapping of particulate fluxes in the reservoirs on the main channels (e.g., sediment)
    ! !assume the trapping occurs after the channel routing, i.e., won't affect the current channel mass balance, but only change the outflow to the downstream
        implicit none

        integer, intent(in) :: iunit, nt
        !real(r8), intent(in) :: theDeltaT

        real(r8) :: trapping_eff  ! trapping efficiency, proportion
        trapping_eff = Tres_para%Eff_trapping(iunit)

        Tres%eres_in(iunit,nt) = -TRunoff%erout(iunit,nt)  * trapping_eff
        Tres%eres_out(iunit,nt) = 0._r8  ! currently assuming no sediment release from the dam bottom (this might be OK in U.S., but wrong in other plances such as China)
        Tres%dwres(iunit,nt) =  Tres%eres_in(iunit,nt) + Tres%eres_out(iunit,nt)

        TRunoff%erout(iunit,nt) = TRunoff%erout(iunit,nt) * (1._r8 - trapping_eff)

    end subroutine res_trapping

    subroutine res_trapping_t(iunit, nt)
    ! !DESCRIPTION: trapping of particulate fluxes in the reservoirs on the sub-network channels (e.g., sediment)
    ! !assume the trapping occurs after the channel routing, i.e., won't affect the current channel mass balance, but only change the outflow to the downstream
        implicit none

        integer, intent(in) :: iunit, nt
        !real(r8), intent(in) :: theDeltaT

        real(r8) :: trapping_eff_t  ! trapping efficiency, propoation
        trapping_eff_t = Tres_para%Eff_trapping_t(iunit)

        Tres%eres_in_t(iunit,nt) = -TRunoff%erlateral(iunit,nt)  * trapping_eff_t
        Tres%eres_out_t(iunit,nt) = 0._r8  ! currently assuming no sediment release from the dam bottom (this might be OK in U.S., but wrong in other plances such as China)
        Tres%dwres_t(iunit,nt) =  Tres%eres_in_t(iunit,nt) + Tres%eres_out_t(iunit,nt)

        TRunoff%erlateral(iunit,nt) = TRunoff%erlateral(iunit,nt) * (1._r8 - trapping_eff_t)

    end subroutine res_trapping_t

    subroutine res_trapping_r(iunit, nt)
    ! !DESCRIPTION: trapping of particulate fluxes in the reservoirs on the main channels (e.g., sediment)
    ! !assume the trapping occurs after the channel routing, i.e., won't affect the current channel mass balance, but only change the outflow to the downstream
        implicit none

        integer, intent(in) :: iunit, nt
        !real(r8), intent(in) :: theDeltaT

        real(r8) :: trapping_eff_r  ! trapping efficiency, proportion
        trapping_eff_r = Tres_para%Eff_trapping_r(iunit)

        Tres%eres_in(iunit,nt) = -TRunoff%erout(iunit,nt)  * trapping_eff_r
        Tres%eres_out(iunit,nt) = 0._r8  ! currently assuming no sediment release from the dam bottom (this might be OK in U.S., but wrong in other plances such as China)
        Tres%dwres(iunit,nt) =  Tres%eres_in(iunit,nt) + Tres%eres_out(iunit,nt)

        TRunoff%erout(iunit,nt) = TRunoff%erout(iunit,nt) * (1._r8 - trapping_eff_r)

    end subroutine res_trapping_r
	
    subroutine res_BGC_update(iunit, nt)
    ! !DESCRIPTION: updating BGC concentration in reservoirs 
        
        integer, intent(in) :: iunit, nt
        integer  :: damID,k,isDam
        real(r8) :: temp_stor_liq

        damID = WRMUnit%INVicell(iunit) 
        if (damID > ctlSubwWRM%LocalNumDam .OR. damID <= 0 .or. WRMUnit%MeanMthFlow(damID,13) <= 0.01_r8) then
           return
        end if
        temp_stor_liq = StorWater%storage(damID) ! please do not assign StorWater%storage(damID) to Tres%wres(iunit, nt_nliq). It'd be double counting of reservoir water storage		
		!write(unit=5003,fmt="(1e14.6)")Tres%eres_in(iunit,nt)
        !Tres%eres_in(iunit,nt) = -TRunoff%erout(iunit,nt)
		if(temp_stor_liq < TINYVALUE_r) then
		    Tres%conc(iunit,nt) = 0._r8
		else
		    Tres%conc(iunit,nt) = Tres%wres(iunit,nt)/temp_stor_liq
		end if
        
    end subroutine res_BGC_update	


    subroutine res_BGC_release(iunit, nt, theDeltaT)
    ! !DESCRIPTION: BGC fluxes with the reservoir release (after regulation)
	! ! assuming BGC fluxes are well mixed with reservoir water (good for dissolved BGC at least)
        
        integer, intent(in) :: iunit, nt
        real(r8), intent(in) :: theDeltaT

		Tres%eres_out(iunit,nt) = Tres%conc(iunit,nt) * TRunoff%erout(iunit,nt_nliq)
		if(Tres%wres(iunit,nt) +  Tres%eres_out(iunit,nt) * theDeltaT < TINYVALUE_r) then
		    Tres%eres_out(iunit,nt) = - 0.95_r8*Tres%wres(iunit,nt)/theDeltaT
		end if
		        
    end subroutine res_BGC_release	

    subroutine res_DOC_mineralization(iunit, nt, theDeltaT)
    ! !DESCRIPTION: DOC mineralization in reservoirs following Maavara et al., 2017, Nat. Comm.
	! ! assuming DOC fluxes are well mixed with reservoir water
        
        integer, intent(in) :: iunit, nt
        real(r8), intent(in) :: theDeltaT
		real(r8)    :: para_b  ! coefficient [-]
		real(r8)    :: para_beta  ! coefficient
		real(r8)    :: coeff_mineral  ! fraction of incoming DOC flux gets mineralized
		real(r8)    :: temp_eres_in 
		
		temp_eres_in = -TRunoff%erout(iunit,nt)
		para_b = 1._r8
		para_beta = 0.0391_r8
		if(temp_eres_in >= TINYVALUE_r) then
		    coeff_mineral = para_b * ( 1._r8 - 1._r8/(1._r8 + para_beta * (WRMUnit%resrv_Tavg(iunit) - 273.15) * Tres_para%Tres(iunit)))
			if(coeff_mineral >= 1._r8) coeff_mineral = 1._r8
		else
		    coeff_mineral = 0._r8
		end if
    if(isnan(coeff_mineral)) then
	    write(unit=1118,fmt="(i10, 4(e20.4))") iunit, para_b, para_beta, WRMUnit%resrv_Tavg(iunit), Tres_para%Tres(iunit)
	end if	
		
		Tres%eres_source(iunit,nt) = -temp_eres_in * coeff_mineral
		
		if(Tres%wres(iunit,nt) +  Tres%eres_source(iunit,nt) * theDeltaT < TINYVALUE_r) then
		    Tres%eres_source(iunit,nt) = - 0.95_r8*Tres%wres(iunit,nt)/theDeltaT
		end if
		
		TRunoff%erout(iunit,nt) = TRunoff%erout(iunit,nt) - Tres%eres_source(iunit,nt)

	if(Tres%eres_source(iunit,nt) >= TINYVALUE_r) then
	    write(unit=1119,fmt="(i10, 5(e20.5))") iunit, Tres%eres_in(iunit,nt), Tres%eres_source(iunit,nt) , coeff_mineral, WRMUnit%resrv_Tavg(iunit), Tres_para%Tres(iunit)
	end if	
	
    if(isnan(Tres%eres_source(iunit,nt))) then
	    write(unit=1120,fmt="(i10, 5(e20.5))") iunit, Tres%eres_in(iunit,nt), para_b, para_beta, WRMUnit%resrv_Tavg(iunit), Tres_para%Tres(iunit)
	end if
		        
    end subroutine res_DOC_mineralization	

end MODULE MOSART_reservoir_mod 