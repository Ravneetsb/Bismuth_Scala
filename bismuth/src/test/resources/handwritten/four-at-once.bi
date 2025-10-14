(512, 512);

n = 10;
tn = theta / (2 * pi) * n;

rays = if tn < 1 {
    red
} else {
    if tn < 2 {
	white
    } else {
	if tn < 3 {
	    red
	} else {
	    if tn < 4 {
		white
	    } else {
		if tn < 5 {
		    red
		} else {
		    if tn < 6 {
			white
		    } else {
			if tn < 7 {
			    red
			} else {
				if tn < 8 {
				    white
				} else {
					if tn < 9 {
					    red
					} else {
					    white
					}
				}
			}
		    }
		}
	    }
	}
    }
};

sunAndRays = if r < 0.5 { yellow } else { rays };

lightBlue = [0, (11/16), 1];
vstripe = replicate(4, x > 0);
hstripe = rotate(pi/2, vstripe);
checker = vstripe ^ hstripe;
bavaria = if scale(2, 1, rotate(pi/8, checker)) { lightBlue } else { white };

s = swirl(pi, sin(theta*20));
swirly = [s, s, 1];

alpha = 2 * pi / 5;
gon = {
    n = 5;
    r' = cos (pi/n) / cos (theta - pi / n);
    theta > 0 && theta < alpha && r < r'
};

pentagon = gon
         || rotate(    2 * pi / 5, gon)
         || rotate(2 * 2 * pi / 5, gon)
         || rotate(3 * 2 * pi / 5, gon)
         || rotate(4 * 2 * pi / 5, gon);

(sunAndRays <..> swirly) <:> (bavaria <..> if pentagon { white } else { green })
