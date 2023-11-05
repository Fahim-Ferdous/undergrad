! Solve 2nd order of
! D2(x, t) + 25x(t) = 0, x(0) = 1, x'(0) = 0, 0 <= t <= 2
! x(t) = cos(5t), iter = 20, h = 0.1

! Notes:
! We need to split the 2nd order ODE to 1st order. Let,
! z(t) = dx/dt = g(z, t)
! so, D2(x, t) = dz/dt
!
! using this, we can write,
! dz/dt = -25x(t) = f(x, t)
program euler_2nd_order

   implicit none
   real :: t(100), x(100), y(100), e(100)
   real :: a = 0, h = 0.1
   real :: tx, ty
   integer :: i, n = 20

   real :: f, g, act

   x(1) = 1
   y(1) = 0
   e(1) = 0.0

   do i = 1, n + 1
      t(i) = a + (i - 1)*h
   end do

   do i = 1, n
      ! Soybean Oiler
      x(i + 1) = x(i) + h*g(t(i), x(i), y(i))
      y(i + 1) = y(i) + h*f(t(i), x(i), y(i))
      e(i + 1) = abs(x(i + 1) - act(t(i + 1)))
   end do

   write (*, *) "Soybean Oiler"
   write (*, "(1a4,2a17,1a19,1a17)") "t", "x", "y", "act", "err"
   do i = 1, n + 1
      write (*, *) t(i), x(i), y(i), act(t(i)), e(i)
   end do

   do i = 1, n
      ! Extra Virgin Olive Oiler
      tx = x(i) + h*g(t(i), x(i), y(i))
      ty = y(i) + h*f(t(i), x(i), y(i))

      x(i + 1) = x(i) + h/2*(g(t(i), x(i), y(i)) + g(t(i + 1), tx, ty))
      y(i + 1) = y(i) + h/2*(f(t(i), x(i), y(i)) + f(t(i + 1), tx, ty))

      e(i + 1) = abs(x(i + 1) - act(t(i + 1)))
   end do

   write (*, *)
   write (*, *) "Extra Virgin Olive Oiler"
   write (*, "(1a4,2a17,1a19,1a17)") "t", "x", "y", "act", "err"
   do i = 1, n + 1
      write (*, *) t(i), x(i), y(i), act(t(i)), e(i)
   end do

   do i = 1, n
      ! Eager Extra Virgin Olive Oiler
      ! Here, both x and y get to use the updated value.
      x(i + 1) = x(i) + h*g(t(i), x(i), y(i))
      y(i + 1) = y(i) + h*f(t(i), x(i), y(i))

      x(i + 1) = x(i) + h/2*(g(t(i), x(i), y(i)) + g(t(i + 1), x(i + 1), y(i + 1)))
      y(i + 1) = y(i) + h/2*(f(t(i), x(i), y(i)) + f(t(i + 1), x(i + 1), y(i + 1)))

      e(i + 1) = abs(x(i + 1) - act(t(i + 1)))
   end do

   write (*, *)
   write (*, *) "Eager Extra Virgin Olive Oiler"
   write (*, "(1a4,2a17,1a19,1a17)") "t", "x", "y", "act", "err"
   do i = 1, n + 1
      write (*, *) t(i), x(i), y(i), act(t(i)), e(i)
   end do
end program euler_2nd_order

function g(t, x, y)

   implicit none
   real, intent(in) :: t, x, y
   real :: g

   g = y
end function g

function f(t, x, y)

   implicit none
   real, intent(in) :: t, x, y
   real :: f

   f = -25*x
end function f

function act(t)

   implicit none
   real, intent(in) :: t
   real :: act

   act = cos(5*t)
end function act
