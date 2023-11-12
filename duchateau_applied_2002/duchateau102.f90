! DuChateau and Zachmann, Applied Partial Differential Equations, algorithm 10.2

program duchateau102
    implicit none
    
    integer, parameter :: RP    = selected_real_kind(15, 307) ! double
    integer, parameter :: IP    = selected_int_kind(7)
    
    integer(kind=IP), parameter :: mmax  = 9_IP  ! number of interior $x$ grid points
    integer(kind=IP), parameter :: nmax  = 9_IP  ! number of interior $y$ grid points
    integer(kind=IP), parameter :: itmax = 100_IP ! maximum number of iterations allowed
    real(kind=RP), parameter    :: a     = 1.0_RP ! $x$ dimension
    real(kind=RP), parameter    :: b     = 1.0_RP ! $y$ dimension
    real(kind=RP), parameter    :: omega = 1.0_RP ! relaxation parameter
    real(kind=RP), parameter    :: tol   = 0.005_RP ! tolerance for maximum of absolute value of residual
    
    real(kind=RP) :: hx
    real(kind=RP) :: hy
    real(kind=RP) :: q
    
    real(kind=RP), dimension(mmax + 2_IP, nmax + 2_IP) :: U ! numerical solution
    real(kind=RP), dimension(mmax + 2_IP, nmax + 2_IP) :: hx2F ! scaled forcing function
    integer(kind=IP) :: i ! number of SOR iterations performed
    real(kind=RP), dimension(mmax + 2_IP) :: x
    real(kind=RP), dimension(nmax + 2_IP) :: y
    real(kind=RP) :: U_old ! numerical solution for previous iteration
    real(kind=RP) :: U_sum, ave, rmax, res
    
    integer(kind=IP) :: m, n ! loop indices
    
    ! Step 2: Set boundary conditions and initialize `U`
    
    hx = a / real(mmax + 1_IP, RP)
    hy = b / real(nmax + 1_IP, RP)
    q  = (hx / hy)**2_IP
    
    U_sum          = 0.0_RP
    x(1_IP)        = 0.0_RP
    x(mmax + 2_IP) = a
    y(1_IP)        = 0.0_RP
    y(nmax + 2_IP) = b
    
    do m = 2_IP, mmax + 1_IP
        x(m) = x(m - 1_IP) + hx
        
        U(m, 1_IP) = py(x(m))
        U(m, nmax + 2_IP) = qy(x(m))
        U_sum = U_sum + U(m, 1_IP) + U(m, nmax + 2_IP)
    end do
    
    do n = 2_IP, nmax + 1_IP
        y(n) = y(n - 1_IP) + hy
        
        U(1_IP, n) = px(y(n))
        U(mmax + 2_IP, n) = qx(x(n))
        U_sum = U_sum + U(1_IP, n) + U(mmax + 2_IP, n)
    end do
    
    ave = U_sum / (2.0_RP * (mmax + nmax))
    write(unit=*, fmt="(a, f5.3)") " starting guess = ", ave
    
    do n = 2_IP, nmax + 1_IP
        do m = 2_IP, mmax + 1_IP
            ! initialize `U` to average of boundary values
            U(m, n) = ave
            
            !hx2F(m, n) = -(hx**2_IP) * f(x(m), y(n))
            hx2F(m, n) = -(hx**2_IP) * f()
        end do
    end do
    
    ! Step 3: Begin SOR iterations
    
    do i = 1_IP, itmax
        ! Step 4. Calculate `i`th SOR iterate
        do n = 2_IP, nmax + 1_IP
            do m = 2_IP, mmax + 1_IP
                U_old = U(m, n)
                
                U(m, n) = (U(m - 1_IP, n) &
                            + q * U(m, n - 1_IP) &
                            + U(m + 1_IP, n) &
                            + q * U(m, n + 1_IP) &
                            + hx2F(m, n)) &
                            / (2.0_RP + 2.0_RP * q)
                
                U(m, n) = omega * U(m, n) + (1.0_RP - omega) * U_old
            end do
        end do
        
        ! Step 5. Calculate maximum residual for `i`th SOR iterate
        rmax = 0.0_RP
        do n = 2_IP, nmax + 1_IP
            do m = 2_IP, mmax + 1_IP
                res = (hx2F(m, n) &
                        + U(m - 1_IP, n) &
                        + U(m + 1_IP, n) &
                        - (2.0_RP + 2.0 * q) * U(m, n) &
                        + q * U(m, n - 1_IP) &
                        + q * U(m, n + 1_IP)) &
                        / (hx**2_IP)
                rmax = max(abs(res), rmax)
            end do
        end do
        
        write(unit=*, fmt="(a, i4, a, es15.6)") " i = ", i, " res = ", rmax
        
        ! Step 6. Compare maximum residual to tolerance
        if (rmax < tol) then
            exit
        end if  
    end do
    
    ! Step 7. Output solution
    
    write(unit=*, fmt="(a)", advance="no") "       x = "
    do m = 2_IP, mmax + 1_IP
        write(unit=*, fmt="(a, f5.3, a)", advance="no") " ", x(m), " "
    end do
    write(unit=*, fmt="(a)")
    
    do n = nmax + 1_IP, 2_IP, -1_IP
        write(unit=*, fmt="(a, f5.3, a)", advance="no") " y = ", y(n), " "
        do m = 2_IP, mmax + 1_IP
            write(unit=*, fmt="(a, f5.3, a)", advance="no") " ", U(m, n), " "
        end do
        
        write(unit=*, fmt="(a)")
    end do
    
    stop
contains
    function f() !(x, y)
        ! right side of Poisson equation
        
        !real(kind=RP), intent(in) :: x, y
        
        real(kind=RP) :: f
        
        f = 4.0_RP
        
        return
    end function f
    
    function px(y)
        ! $u(0, y) = px(y)$
        ! boundary condition on $x = 0$
        
        real(kind=RP), intent(in) :: y
        
        real(kind=RP) :: px
        
        px = y**2_IP
        
        return
    end function px
    
    function py(x)
        ! $u(x, 0) = py(x)$
        ! boundary condition on $y = 0$
        
        real(kind=RP), intent(in) :: x
        
        real(kind=RP) :: py
        
        py = x**2_IP
        
        return
    end function py
    
    function qx(y)
        ! $u(a, y) = qx(y)$
        ! boundary condition on $x = a$
        
        real(kind=RP), intent(in) :: y
        
        real(kind=RP) :: qx
        
        qx = 1.0_RP + y**2_IP
        
        return
    end function qx
    
    function qy(x)
        ! $u(x, b) = qy(x)$
        ! boundary condition on $y = b$
        
        real(kind=RP), intent(in) :: x
        
        real(kind=RP) :: qy
        
        qy = x**2_IP + 1.0_RP
        
        return
    end function qy
end program duchateau102
