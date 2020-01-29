function dx1(x1,x2)
double precision dx1, x1, x2, si
si = 10
dx1 = si*(x2 - x1)
end function dx1 

function dx2(x1,x2,x3)
double precision dx2, x1, x2,x3, ro
ro = 28
dx2 = ro*x1 - x2 - x1*x3
end function dx2 

function dx3(x1,x2,x3)
double precision dx3, x1, x2, x3, be
be = 2.66
dx3 = x1*x2 - be*x3
end function dx3 

program project
implicit none
integer i
double precision ener,vel,s,x1,tn,x2,x3,x1o,x2o,x3o,dx1,dx2,dx3
double precision k1(3), k2(3), k3(3), k4(3)

s = 0.001
x1 = 11
x2 = 15
x3 = 18

open(10,file ='plot.txt', status='unknown')
do i = 1, 50000
k1(1) = s*dx1(x1,x2)
k1(2) = s*dx2(x1,x2,x3)
k1(3) = s*dx3(x1,x2,x3)

k2(1) = s*dx1(x1+k1(1)*0.5,x2+k1(2)*0.5)  
k2(2) = s*dx2(x1+k1(1)*0.5,x2+k1(2)*0.5,x3+k1(3)*0.5)
k2(3) = s*dx3(x1+k1(1)*0.5,x2+k1(2)*0.5,x3+k1(3)*0.5)

k3(1) = s*dx1(x1+k2(1)*0.5,x2+k2(2)*0.5)  
k3(2) = s*dx2(x1+k2(1)*0.5,x2+k2(2)*0.5,x3+k2(3)*0.5)
k3(3) = s*dx3(x1+k2(1)*0.5,x2+k2(2)*0.5,x3+k2(3)*0.5)

k4(1) = s*dx1(x1+k3(1),x2+k3(2))
k4(2) = s*dx2(x1+k3(1),x2+k3(2),x3+k3(3))
k4(3) = s*dx3(x1+k3(1),x2+k3(2),x3+k3(3))

x1 = x1 + (k1(1) + 2*(k2(1) + k3(1)) + k4(1))/6
x2 = x2 + (k1(2) + 2*(k2(2) + k3(2)) + k4(2))/6
x3 = x3 + (k1(3) + 2*(k2(3) + k3(3)) + k4(3))/6

write (10,*) x1,x2,x3 
end do

end program project


