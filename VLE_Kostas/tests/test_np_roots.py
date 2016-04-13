import numpy as np
import matplotlib.pyplot as plt

# -----------------------------------------------------------------------------------
# this is a test to know how the polynomial works based on the notes taken from the : 
# http://docs.scipy.org/doc/numpy-1.10.0/reference/generated/numpy.poly.html
# -----------------------------------------------------------------------------------

'''
# Parameters
r = 3.74
T = np.linspace(0.0,1.0,10)

# Coefficients
C = np.zeros((len(T),3))
print C, len(T)
C[:,0] = T-1
C[:,1] = r + 1 - 0.5*T
C[:,2] = r
#print C[:,2], C[:,1]

# Roots
R = np.zeros((len(T),2))
for i in range(len(T)):
    R[i] = np.roots(C[i])
    print R[i]
print len(T)

# Plot
fig = plt.figure()
plt.plot(T,R[:,0])
plt.plot(T,R[:,1])

plt.show()
'''


Big_B = 5
Big_A = 5

coeffs = [0. for i in range( 4 ) ]; print ' * initially the coeffs are, ', coeffs ;  print     # Coefficient of the polynomial 
Z_root = [0. for i in range( 3 ) ]; print ' * initially the Z_roots are, ', Z_root;  print     # Roots of the polynomial

coeffs[ 0 ] = 1.
coeffs[ 1 ] = - ( 1. - Big_B )
coeffs[ 2 ] = Big_A - 2. * Big_B - 3. * Big_B**2
coeffs[ 3 ] = - ( Big_A * Big_B - Big_B**3 - Big_B**2 )
print ' * the coeffs from the polynomial are =', coeffs 
print
Z_root = np.roots( coeffs ) # Calculating the roots of the cubic eqn
print ' * the Z_roots are = ', Z_root
print

smallvalue = -1.e15
Z_realroot = [smallvalue for i in range( 3 ) ]; print ' * the Z_realroots are = ', Z_realroot; print

Z_realroot = np.sort( Z_realroot ) # Quick-sort algorithm -- Largest/smallest real root: vapour / liquid
Zvapour = 1.e-6 ; Zliquid = 1.e6

for i in range( 3 ):
    print ' * i = ', i
    Zvapour = max( Zvapour, Z_realroot[ i ] ); print Zvapour # Largest real root: vapour
    Zliquid = min( Zliquid, Z_realroot[ i ] ); print Zliquid # Smallest real rrot: liquid
    print
print ' * finally I have  the max & min value ' , Zvapour, Zliquid

