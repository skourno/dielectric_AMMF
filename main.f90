Program main
	Use nrtype
	Use phys_consts
	Use Laguerre,   Only : Laguerre_fast
	Use g_mvt,      Only : g_atContact
	Use dielectric, Only : G1, dielectric_const

	Implicit None

	Real(DP)            :: mu, mu2, rho
	Real(DP)            :: diel, G, sigBH, temp, rep_l, attr_l, scale, log_mu2(6)
	Integer(I4B)        :: i 


	rho    = 0.05     ! dimensionless
	temp   = 1.00
	mu2    = 0.1*0.1    ! dimensionless with eps
	rep_l  = 12
	attr_l = 6
	scale  = 1.0

        mu2    = mu2/temp ! the routine use dip mom in kT not eps
	!mu     = sqrt(mu2)
	!print*, mu, mu*mu
	log_mu2 = (/0.04,0.50,1.00,2.00,3.00,4.00/)

    	OPEN(UNIT=23, FILE='./out.dat', STATUS='UNKNOWN', FORM='FORMATTED')
	
!     	do while (mu2*temp*scale < 5.0)
	i = 1
     	do while (rho <= 1.05)
!     	do while (rho < 1.0)
!    	do while (temp <= 2.51)
		do i=1,6
			mu2 = log_mu2(i)
        		mu    = sqrt(mu2) 
			call Laguerre_fast(Temp,rep_l,attr_l,sigBH)			
 			G     =               G1(rho, mu, ONE) 
			diel  = dielectric_const(rho, mu, ONE,scale)

			write( *,111) temp, rho, mu2*temp, sigBH, diel, G ! mu*mu output in epsilon units
			write(23,111) temp, rho, mu2*temp, sigBH, diel, G ! mu*mu output in epsilon units

			!write( *,111) temp, rho, mu*mu, sigBH, diel 
			!write(23,111) temp, rho, mu*mu, sigBH, diel 
			!rho = rho + 0.025D0
	!enddo
!	rho  = 0.60
			!i = i+1
			!mu2 = (((i+1)*0.1D0)**2)/temp
			!temp = temp + 0.02D0
!			mu2  = 5.0 / temp
		enddo	
       		rho = rho + 0.05			
        enddo


111 FORMAT ( F10.2, 2X, F9.3, 2X, F14.6, 2X, F10.4, 2X, F10.2, 2X, F10.2)

End program main
