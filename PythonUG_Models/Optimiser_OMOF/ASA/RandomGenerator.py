
#!/usr/bin/env python

import math
import random
import time

###
### Random number generators 
###
def RandomNumberGenerator( n, Lower, Upper ):

    rn = []
    # Initialisation:
    for i in range( n ):
        r = random.SystemRandom()
        seed = time.time() # seed
        if ( seed % 2 > 1. ):
            r = random.SystemRandom( seed )
        else:
            r0, r1 = math.modf( seed )
            if ( i % 2 == 0 ):
                r = random.SystemRandom( r0 )
            else:
                r = random.SystemRandom( r1 )

        #rn.append( r.random() )
        rn.append( r.uniform( Upper[ i ], Lower[ i ]  ) )
                
    return rn
